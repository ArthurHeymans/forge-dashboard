;;; forge-dashboard.el --- Triage-oriented Forge dashboard  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Maintainer: Arthur Heymans <arthur@aheymans.xyz>
;; Version: 0.1.0
;; Homepage: https://github.com/ArthurHeymans/forge-dashboard
;; Keywords: tools, vc
;; Package-Requires: ((emacs "29.1") (forge "0.5.0") (magit "4.0.0") (transient "0.9.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of forge-dashboard.

;; forge-dashboard is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; forge-dashboard is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A compact, local-only overview of open Forge topics in owned and
;; organization repositories.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'forge)
(require 'forge-commands)
(require 'forge-topics)
(require 'forge-dashboard-triage)
(require 'magit-section)
(require 'seq)
(require 'subr-x)
(require 'transient)

(defgroup forge-dashboard nil
  "A triage-oriented dashboard for Forge."
  :group 'forge)

(defcustom forge-dashboard-organizations nil
  "Extra owners whose tracked repositories count as member repositories.
Membership is normally derived automatically: a repository is a member
repository when your githost login is among its assignable users in
Forge's database.  This option only adds owners on top of that, for
repositories whose assignee data has not been synced."
  :type '(repeat string)
  :group 'forge-dashboard)

(defcustom forge-dashboard-topics-per-repo nil
  "Maximum number of open topics shown when a repository is expanded.
Nil means show every topic.  Repository sections are collapsed by default."
  :type '(choice (const :tag "All" nil) natnum)
  :group 'forge-dashboard)

(defcustom forge-dashboard-cache-file
  (expand-file-name "forge-dashboard-cache.el" user-emacs-directory)
  "File holding the last successful local database snapshot.
Set to nil to disable caching across Emacs sessions.  Database scans still
run in a separate Emacs process.  This file contains private topic data."
  :type '(choice (const :tag "No disk cache" nil) file)
  :group 'forge-dashboard)

(defvar-local forge-dashboard--snapshot nil)
(defvar-local forge-dashboard--refresh-process nil)
(defvar-local forge-dashboard--render-only nil)

(defcustom forge-dashboard-stale-after 14
  "Number of days after which a topic is considered stale."
  :type 'natnum
  :group 'forge-dashboard)

(defface forge-dashboard-unread
  '((t :inherit forge-topic-slug-unread :foreground "red"))
  "Face used for unread topic rows."
  :group 'forge-dashboard)

(defface forge-dashboard-pending
  '((t :inherit warning :foreground "orange"))
  "Face used for pending topic rows."
  :group 'forge-dashboard)

(defface forge-dashboard-age-fresh
  '((t :inherit shadow))
  "Face used for fresh topic ages."
  :group 'forge-dashboard)

(defface forge-dashboard-age-aging
  '((t :inherit warning :foreground "orange"))
  "Face used for topic ages over seven days."
  :group 'forge-dashboard)

(defface forge-dashboard-age-stale
  '((t :inherit error))
  "Face used for stale topic ages."
  :group 'forge-dashboard)

(defface forge-dashboard-issue
  '((t :inherit font-lock-constant-face))
  "Face used for issue labels and counts."
  :group 'forge-dashboard)

(defface forge-dashboard-ready
  '((t :inherit success :weight bold))
  "Face used for items that are ready to merge."
  :group 'forge-dashboard)

(defface forge-dashboard-blocked
  '((t :inherit error :weight bold))
  "Face used for items blocked on the user."
  :group 'forge-dashboard)

(defface forge-dashboard-waiting
  '((t :inherit warning))
  "Face used for items waiting on someone else."
  :group 'forge-dashboard)

(defvar-local forge-dashboard-topic-type 'all
  "Topic type displayed in the current dashboard buffer.")

(defvar-local forge-dashboard-show-owned t
  "Whether the owned-repositories section is displayed.")

(defvar-local forge-dashboard-show-organizations t
  "Whether the organizations section is displayed.")

(defun forge-dashboard--time (timestamp)
  "Parse Forge TIMESTAMP, returning nil when it is absent or malformed."
  (and timestamp (ignore-errors (date-to-time timestamp))))

(defun forge-dashboard--age-days (timestamp &optional now)
  "Return the non-negative age of TIMESTAMP in days relative to NOW."
  (if-let* ((then (forge-dashboard--time timestamp)))
      (max 0 (floor (/ (float-time (time-subtract (or now (current-time)) then))
                       86400)))
    0))

(defun forge-dashboard--age-label (days)
  "Return a compact human-readable label for DAYS."
  (cond ((< days 1) "today")
        ((= days 1) "1d")
        ((< days 7) (format "%dd" days))
        ((< days 60) (format "%dw" (/ days 7)))
        (t (format "%dmo" (/ days 30)))))

(defun forge-dashboard--age-face (days)
  "Return the age face appropriate for DAYS."
  (cond ((> days forge-dashboard-stale-after) 'forge-dashboard-age-stale)
        ((> days 7) 'forge-dashboard-age-aging)
        (t 'forge-dashboard-age-fresh)))

(defun forge-dashboard--topic-type (topic)
  "Return a display name for TOPIC's type."
  (cond ((forge-pullreq-p topic) "PR")
        ((forge-issue-p topic) "issue")
        (t "discussion")))

(defun forge-dashboard--topic-row (topic &optional now)
  "Compute display data for TOPIC relative to NOW.
The returned plist contains no rendered text or buffer state."
  (let* ((updated (or (oref topic updated) (oref topic created)))
         (age (forge-dashboard--age-days updated now)))
    (list :number (oref topic number)
          :title (oref topic title)
          :type (forge-dashboard--topic-type topic)
          :author (or (oref topic author) "unknown")
          :status (oref topic status)
          :updated updated
          :age age)))

(defun forge-dashboard--topic-spec (type)
  "Return the Forge query specification for open topics of TYPE."
  (forge--topics-spec :type type :active nil :state 'open :status nil
                      :order 'recently-updated :limit nil))

(defun forge-dashboard--repo-data (repo)
  "Compute counts and selected topic objects for REPO."
  (let* ((discussions
          (forge--list-topics (forge-dashboard--topic-spec 'discussion)
                              repo 'discussion))
         (issues (forge--list-topics (forge-dashboard--topic-spec 'issue)
                                     repo 'issue))
         (pullreqs (forge--list-topics (forge-dashboard--topic-spec 'pullreq)
                                       repo 'pullreq))
         (all (append discussions issues pullreqs))
         (topics (pcase forge-dashboard-topic-type
                   ('pr pullreqs)
                   ('issue issues)
                   (_ (sort (copy-sequence all)
                            (lambda (left right)
                              (string> (or (oref left updated) "")
                                       (or (oref right updated) ""))))))))
    (list :repo repo
          :open-topics (length all)
          :open-pullreqs (length pullreqs)
          :open-issues (length issues)
          :unread (seq-count (lambda (topic)
                               (eq (oref topic status) 'unread))
                             all)
          :topics topics)))

(defun forge-dashboard--tracked-repositories ()
  "Return repositories explicitly tracked in Forge's local database."
  (seq-filter (lambda (repo) (eq (oref repo condition) :tracked))
              (forge--ls-repos)))

(defvar forge-dashboard--repo-data-cache nil
  "Refresh-local cache of repository data keyed by type and repository.")

(defvar forge-dashboard--classification-cache nil
  "Refresh-local cache of repository classifications keyed by repository ID.")

(defun forge-dashboard--cached-repo-data (repo)
  "Return REPO's data, reusing the current refresh-local cache when present."
  (let* ((type forge-dashboard-topic-type)
         (entry (seq-find
                 (lambda (entry)
                   (and (eq (nth 0 entry) type)
                        (equal (oref (nth 1 entry) id) (oref repo id))))
                 forge-dashboard--repo-data-cache)))
    (or (nth 2 entry)
        (let ((data (forge-dashboard--repo-data repo)))
          (push (list type repo data) forge-dashboard--repo-data-cache)
          data))))

(defun forge-dashboard--repo-login (repo)
  "Return the configured githost username for REPO, or nil.
Use Ghub's Forge repository method so host-specific Git variables and
fallbacks are handled consistently, without prompting."
  (ignore-errors (ghub--username repo)))

(defun forge-dashboard--assignable-p (repo login)
  "Return non-nil when LOGIN is an assignable user of REPO.
Assignability implies membership or collaborator access."
  (and login
       (forge-sql1 [:select [login] :from assignee
                    :where (and (= repository $s1) (= login $s2))]
                   (oref repo id) login)
       t))

(defun forge-dashboard--classification (owner login owned-accounts orgs
                                              assignable)
  "Classify a repository as `owned', `member', or nil.
Pure decision over the repository OWNER, my LOGIN, the OWNED-ACCOUNTS
and ORGS overrides, and whether I am ASSIGNABLE in the repository."
  (cond ((or (member owner owned-accounts)
             (and login (equal owner login)))
         'owned)
        ((or (member owner orgs) assignable) 'member)))

(defun forge-dashboard--classify (repo)
  "Classify REPO as `owned', `member', or nil (external)."
  (if (hash-table-p forge-dashboard--classification-cache)
      (let* ((id (oref repo id))
             (entry (gethash id forge-dashboard--classification-cache)))
        (if entry
            (car entry)
          (let ((classification
                 (forge-dashboard--classify-uncached repo)))
            (puthash id (list classification)
                     forge-dashboard--classification-cache)
            classification)))
    (forge-dashboard--classify-uncached repo)))

(defun forge-dashboard--classify-uncached (repo)
  "Classify REPO without consulting the refresh-local cache."
  (let ((login (forge-dashboard--repo-login repo)))
    (forge-dashboard--classification
     (oref repo owner) login
     (mapcar #'car forge-owned-accounts)
     forge-dashboard-organizations
     (forge-dashboard--assignable-p repo login))))

(defun forge-dashboard--dashboard-repositories ()
  "Return all tracked repositories classified as owned or member."
  (seq-filter #'forge-dashboard--classify
              (forge-dashboard--tracked-repositories)))

(defun forge-dashboard--latest-update ()
  "Return the latest repository or topic update timestamp from Forge's db."
  (forge-sql1
   (concat "SELECT MAX(updated) FROM ("
           "SELECT updated FROM repository UNION ALL "
           "SELECT updated FROM discussion UNION ALL "
           "SELECT updated FROM issue UNION ALL "
           "SELECT updated FROM pullreq)")))

(defun forge-dashboard--updated-label ()
  "Return a label describing the latest local database update."
  (forge-dashboard--format-updated-label (forge-dashboard--latest-update)))

(defun forge-dashboard--format-updated-label (updated)
  "Describe cached UPDATED relative to now, without querying the database."
  (if updated
      (let ((days (forge-dashboard--age-days updated)))
        (if (zerop days)
            "<1d ago"
          (format "%s ago" (forge-dashboard--age-label days))))
    "never"))

(defun forge-dashboard--topic-type-face (topic)
  "Return the Forge-style face for TOPIC's type label."
  (cond ((forge-pullreq-p topic) 'forge-pullreq-open)
        ((forge-issue-p topic) 'forge-dashboard-issue)
        (t 'forge-discussion-open)))

(defun forge-dashboard--unread-badge (status)
  "Return a fixed-width badge for topic STATUS."
  (if (eq status 'unread)
      (propertize "[NEW] " 'font-lock-face 'forge-dashboard-unread)
    "      "))

(defun forge-dashboard--insert-topic (topic &optional depth)
  "Insert one TOPIC row at indentation DEPTH using Forge styling."
  (let* ((row (forge-dashboard--topic-row topic))
         (age (plist-get row :age)))
    (magit-insert-section ((eval (oref topic closql-table)) topic t)
      (insert
       (make-string (* 2 (or depth 0)) ?\s)
       (forge-dashboard--unread-badge (plist-get row :status))
       (string-pad (forge--format-topic-slug topic) 7)
       (string-pad
        (truncate-string-to-width (forge--format-topic-title topic)
                                  48 nil nil t)
        49)
       (propertize (string-pad (plist-get row :type) 6)
                   'font-lock-face (forge-dashboard--topic-type-face topic))
       (propertize (format "@%-16s " (plist-get row :author))
                   'font-lock-face 'forge-dimmed)
       (propertize (forge-dashboard--age-label age)
                   'font-lock-face (forge-dashboard--age-face age))
       "\n"))))

(defun forge-dashboard--repository-heading (data)
  "Return a colorful repository heading for DATA, omitting zero counts."
  (let* ((repo (plist-get data :repo))
         (pullreqs (plist-get data :open-pullreqs))
         (issues (plist-get data :open-issues))
         (unread (plist-get data :unread))
         (counts
          (delq nil
                (list
                 (and (> pullreqs 0)
                      (propertize (format "%d PR" pullreqs)
                                  'font-lock-face 'forge-pullreq-open))
                 (and (> issues 0)
                      (propertize (format "%d issue" issues)
                                  'font-lock-face 'forge-dashboard-issue))
                 (and (> unread 0)
                      (propertize (format "%d unread" unread)
                                  'font-lock-face 'forge-dashboard-unread))))))
    (concat (propertize (oref repo slug) 'font-lock-face 'bold)
            (and counts (concat "  " (string-join counts "  "))))))

(defun forge-dashboard--insert-repository (data &optional depth)
  "Insert repository DATA and its topic children at indentation DEPTH."
  (let* ((repo (plist-get data :repo))
         (topics (plist-get data :topics))
         (visible (if forge-dashboard-topics-per-repo
                      (seq-take topics forge-dashboard-topics-per-repo)
                    topics))
         (remaining (- (length topics) (length visible))))
    (magit-insert-section (forge-repo repo t)
      (magit-insert-heading
        (concat (make-string (* 2 (or depth 0)) ?\s)
                (forge-dashboard--repository-heading data)))
      (magit-insert-section-body
        (dolist (topic visible)
          (forge-dashboard--insert-topic topic (1+ (or depth 0))))
        (when (> remaining 0)
          (magit-insert-section (forge-dashboard-more repo)
            (insert (make-string (* 2 (1+ (or depth 0))) ?\s)
                    (format "…%d more (RET to list all)\n" remaining))))))))

(defun forge-dashboard--active-repo-data (repos)
  "Return display data for REPOS that have at least one open topic."
  (seq-keep (lambda (repo)
              (let ((data (forge-dashboard--cached-repo-data repo)))
                (and (> (plist-get data :open-topics) 0) data)))
            repos))

(defun forge-dashboard--insert-repositories (heading repos)
  "Insert a section named HEADING containing non-empty REPOS."
  (magit-insert-section (forge-dashboard-group)
    (magit-insert-heading heading)
    (if-let* ((data (forge-dashboard--active-repo-data repos)))
        (dolist (repo-data data)
          (forge-dashboard--insert-repository repo-data 1))
      (insert "  No matching tracked repositories\n"))))

(defun forge-dashboard--insert-owned (repos)
  "Insert owned repositories selected from REPOS."
  (forge-dashboard--insert-repositories
   "Owned repositories"
   (seq-filter (lambda (repo)
                 (eq (forge-dashboard--classify repo) 'owned))
               repos)))

(defun forge-dashboard--insert-organizations (repos)
  "Insert member repositories selected from REPOS, grouped by owner."
  (magit-insert-section (forge-dashboard-organizations)
    (magit-insert-heading "Member repositories")
    (let ((groups
           (seq-group-by
            (lambda (data) (oref (plist-get data :repo) owner))
            (forge-dashboard--active-repo-data
             (seq-filter
              (lambda (repo)
                (eq (forge-dashboard--classify repo) 'member))
              repos)))))
      (if groups
          (dolist (group groups)
            (magit-insert-section (forge-dashboard-organization (car group))
              (magit-insert-heading (concat "  " (car group)))
              (dolist (data (cdr group))
                (forge-dashboard--insert-repository data 2))))
        (insert "  No matching tracked repositories\n")))))

(defvar-keymap forge-dashboard-mode-map
  :doc "Keymap for `forge-dashboard-mode'."
  :parent (make-composed-keymap forge-common-map magit-mode-map)
  "RET" #'forge-dashboard-visit
  "<return>" #'forge-dashboard-visit
  "b" #'forge-dashboard-browse
  "o" #'forge-dashboard-browse
  "<remap> <magit-browse-thing>" #'forge-dashboard-browse
  "<remap> <forge-browse-topic>" #'forge-dashboard-browse
  "<remap> <forge-browse-discussion>" #'forge-dashboard-browse
  "<remap> <forge-browse-issue>" #'forge-dashboard-browse
  "<remap> <forge-browse-pullreq>" #'forge-dashboard-browse
  "y" #'forge-dashboard-copy-url
  "g" #'forge-dashboard-refresh
  "G" #'forge-dashboard-pull
  "t" #'forge-dashboard-triage
  "z" #'forge-dashboard-snooze
  "d" #'forge-dashboard-done
  "C" #'forge-dashboard-nudge
  "M" #'forge-dashboard-merge
  "?" #'forge-dashboard-menu)

;; `defvar-keymap' preserves an existing map when this file is reloaded.
;; Reapply browse bindings so iterative reloads behave like a fresh session.
(dolist (binding '("b" "o" "<remap> <magit-browse-thing>"
                   "<remap> <forge-browse-topic>"
                   "<remap> <forge-browse-discussion>"
                   "<remap> <forge-browse-issue>"
                   "<remap> <forge-browse-pullreq>"))
  (keymap-set forge-dashboard-mode-map binding #'forge-dashboard-browse))
(keymap-set forge-dashboard-mode-map "g" #'forge-dashboard-refresh)

(define-derived-mode forge-dashboard-mode magit-mode "Forge Dashboard"
  "Major mode for the Forge dashboard."
  :interactive nil
  (setq-local forge-buffer-unassociated-p t)
  (add-hook 'kill-buffer-hook #'forge-dashboard--cancel-refresh nil t))

(defun forge-dashboard--attention-items (repos &optional now)
  "Return urgency-sorted attention items for REPOS at NOW."
  (let ((forge-dashboard-triage--state-cache
         (or forge-dashboard-triage--state-cache
             (forge-dashboard-triage--load-state-cache))))
    (forge-dashboard-sort-attention
     (seq-keep
      (lambda (topic) (forge-dashboard-triage-item topic now))
      (mapcan
       (lambda (repo)
         (let ((forge-dashboard-topic-type 'all))
           (copy-sequence (plist-get (forge-dashboard--cached-repo-data repo)
                                     :topics))))
       repos)))))

(defun forge-dashboard--ci-badge (ci)
  "Return an informational badge for CI."
  (pcase ci ('success "CI ✓") ('failed "CI ✗") (_ "CI n/a")))

(defun forge-dashboard--attention-reason (item)
  "Return the human-readable reason for attention ITEM."
  (let ((age (plist-get item :age)))
    (pcase (plist-get item :state)
      ('changes-requested "changes requested")
      ('they-replied "they replied")
      ('review-requested "review requested")
      ('awaiting-review (format "awaiting review %dd" age))
      ('stale (format "stale %dd" age)))))

(defun forge-dashboard--insert-attention-item (item ready &optional depth)
  "Insert attention ITEM at DEPTH, using READY format when non-nil."
  (let* ((topic (plist-get item :topic))
         (data (plist-get item :data))
         (repo (or (plist-get data :repo) "unknown"))
         (indent (make-string (* 2 (or depth 0)) ?\s)))
    (magit-insert-section ((eval (oref topic closql-table)) topic t)
      (if ready
          (insert
           indent
           (propertize "✅ " 'font-lock-face 'forge-dashboard-ready)
           (propertize (format "%-32s " repo) 'font-lock-face 'bold)
           (propertize (format "#%-5d " (oref topic number))
                       'font-lock-face 'forge-pullreq-open)
           (format "%-45s "
                   (truncate-string-to-width (forge--format-topic-title topic)
                                             45 nil nil t))
           (propertize
            (format "%d approval%s  %s  M to merge\n"
                    (plist-get data :approvals)
                    (if (= (plist-get data :approvals) 1) "" "s")
                    (forge-dashboard--ci-badge (plist-get data :ci)))
            'font-lock-face 'forge-dashboard-ready))
        (let* ((state (plist-get item :state))
               (blocked (memq state '(changes-requested they-replied
                                      review-requested)))
               (state-face (if blocked 'forge-dashboard-blocked
                             'forge-dashboard-waiting)))
          (insert
           indent
           (propertize (if blocked "⛔ " "⏳ ")
                       'font-lock-face state-face)
           (propertize (format "%-32s " repo) 'font-lock-face 'bold)
           (propertize (format "#%-5d " (oref topic number))
                       'font-lock-face (forge-dashboard--topic-type-face topic))
           (format "%-45s "
                   (truncate-string-to-width (forge--format-topic-title topic)
                                             45 nil nil t))
           (propertize
            (format "%-22s %dd\n"
                    (forge-dashboard--attention-reason item)
                    (plist-get item :age))
            'font-lock-face state-face)))))))

(defun forge-dashboard--insert-attention-group (heading items)
  "Insert an indented attention group named HEADING containing ITEMS."
  (when items
    (magit-insert-section (forge-dashboard-attention-group heading)
      (magit-insert-heading (concat "  " heading))
      (dolist (item items)
        (forge-dashboard--insert-attention-item item nil 2)))))

(defun forge-dashboard--insert-attention (items)
  "Insert ready and action-grouped attention sections from ITEMS.
Subgroups follow `forge-dashboard-attention-groups'."
  (let ((ready (seq-filter
                (lambda (item)
                  (eq (plist-get item :state) 'ready-to-merge))
                items))
        (groups (mapcar (lambda (group)
                          (cons (car group)
                                (forge-dashboard-triage-group-items
                                 items (cdr group))))
                        forge-dashboard-attention-groups)))
    (magit-insert-section (forge-dashboard-ready)
      (magit-insert-heading "Ready to merge")
      (if ready
          (dolist (item ready)
            (forge-dashboard--insert-attention-item item t 1))
        (insert "  Nothing ready to merge\n")))
    (magit-insert-section (forge-dashboard-attention)
      (magit-insert-heading "Needs attention")
      (if (seq-some #'cdr groups)
          (dolist (group groups)
            (forge-dashboard--insert-attention-group (car group) (cdr group)))
        (insert "  Nothing needs attention\n")))))

(defun forge-dashboard--collect-snapshot ()
  "Collect dashboard data synchronously, in the worker Emacs only."
  (let* ((forge-dashboard-topic-type 'all)
         (forge-dashboard--repo-data-cache nil)
         (forge-dashboard--classification-cache (make-hash-table :test #'equal))
         (repos (forge-dashboard--dashboard-repositories))
         (attention (forge-dashboard--attention-items repos)))
    (list :repos repos
          :classes (mapcar (lambda (repo)
                             (cons (oref repo id) (forge-dashboard--classify repo)))
                           repos)
          :data (mapcar #'forge-dashboard--cached-repo-data repos)
          :attention attention
          :updated (forge-dashboard--latest-update))))

(defun forge-dashboard--encode (value)
  "Convert VALUE to readable data, omitting live Closql connections."
  (cond
   ((eieio-object-p value)
    (list :forge-dashboard-object (eieio-object-class value)
          (seq-keep
           (lambda (slot)
             (let* ((name (eieio-slot-descriptor-name slot))
                    ;; Do not materialize lazy database-backed slots.
                    (raw (if (cl-typep value 'closql-object)
                             (closql--oref value name)
                           (condition-case nil (slot-value value name)
                             (unbound-slot eieio--unbound)))))
               (when (and (not (eq name 'closql-database))
                          (not (eq raw eieio--unbound)))
                 (cons name (forge-dashboard--encode raw)))))
           (eieio-class-slots (eieio-object-class value)))))
   ((consp value)
    (cons (forge-dashboard--encode (car value))
          (forge-dashboard--encode (cdr value))))
   ((vectorp value) (vconcat (mapcar #'forge-dashboard--encode value)))
   (t value)))

(defun forge-dashboard--decode (value &optional connection)
  "Restore VALUE's Forge objects, attaching them to CONNECTION."
  (cond
   ((and (consp value) (eq (car value) :forge-dashboard-object))
    (let ((object (make-instance (nth 1 value))))
      (dolist (slot (nth 2 value))
        (setf (slot-value object (car slot))
              (forge-dashboard--decode (cdr slot) connection)))
      (when (cl-typep object 'closql-object)
        (closql--oset object 'closql-database connection))
      object))
   ((consp value)
    (cons (forge-dashboard--decode (car value) connection)
          (forge-dashboard--decode (cdr value) connection)))
   ((vectorp value)
    (vconcat (mapcar (lambda (item) (forge-dashboard--decode item connection))
                    value)))
   (t value)))

(defun forge-dashboard--worker-settings ()
  "Return configuration needed by the worker and identifying disk caches."
  (mapcar (lambda (variable) (list variable (symbol-value variable)))
          '(user-emacs-directory forge-database-file forge-owned-accounts
            forge-dashboard-organizations forge-dashboard-stale-after
            forge-dashboard-awaiting-review-after forge-dashboard-urgency-weights
            forge-dashboard-triage-file)))

(defun forge-dashboard--read-snapshot (file settings)
  "Read a snapshot from FILE if its configuration matches SETTINGS."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((record (read (current-buffer))))
      (when (and (equal (plist-get record :version) 1)
                 (equal (plist-get record :settings) settings))
        (forge-dashboard--decode (plist-get record :snapshot)
                                 (oref (forge-db) connection))))))

(defun forge-dashboard--worker (file)
  "Write a fresh, serializable snapshot to FILE in a batch Emacs."
  (let ((print-length nil) (print-level nil))
    (with-temp-file file
      (prin1 (list :version 1 :settings (forge-dashboard--worker-settings)
                   :snapshot (forge-dashboard--encode
                              (forge-dashboard--collect-snapshot)))
             (current-buffer)))))

(defun forge-dashboard--cancel-refresh ()
  "Cancel this buffer's database scan, if any."
  (let ((process forge-dashboard--refresh-process))
    (setq forge-dashboard--refresh-process nil)
    (when (process-live-p process) (delete-process process))))

(defun forge-dashboard--start-refresh ()
  "Start one background database scan for the current dashboard."
  (unless (process-live-p forge-dashboard--refresh-process)
    (let* ((buffer (current-buffer))
           (settings (forge-dashboard--worker-settings))
           (file (make-temp-file "forge-dashboard-"))
           (log (generate-new-buffer " *forge-dashboard-worker*"))
           (command
            (append
             (list (expand-file-name invocation-name invocation-directory)
                   "-Q" "--batch")
             (mapcan (lambda (dir)
                       (when (and (stringp dir) (not (file-remote-p dir)))
                         (list "-L" (expand-file-name dir))))
                     load-path)
             (list "--eval" "(setq load-prefer-newer t)"
                   "-l" "forge-dashboard"
                   "--eval" (prin1-to-string
                              `(progn ,@(mapcar (pcase-lambda (`(,var ,value))
                                                  `(setq ,var ',value))
                                                settings)
                                      (forge-dashboard--worker ,file)))))))
      (condition-case err
          (setq forge-dashboard--refresh-process
                (make-process
                 :name "forge-dashboard-refresh" :buffer log
                 :command command :connection-type 'pipe :noquery t
                 :sentinel
                 (lambda (process _event)
                   (when (memq (process-status process) '(exit signal))
                     (unwind-protect
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (when (eq process forge-dashboard--refresh-process)
                               (setq forge-dashboard--refresh-process nil)
                               (condition-case failure
                                   (progn
                                     (unless (zerop (process-exit-status process))
                                       (error "Worker failed: %s"
                                              (with-current-buffer log
                                                (buffer-string))))
                                     ;; Settings may have changed during the scan.
                                     (if (equal settings
                                                (forge-dashboard--worker-settings))
                                       (let ((snapshot
                                              (forge-dashboard--read-snapshot
                                               file settings)))
                                         (unless snapshot (error "Invalid snapshot"))
                                         (setq forge-dashboard--snapshot snapshot)
                                         (when forge-dashboard-cache-file
                                           (condition-case cache-error
                                               (forge-dashboard--save-cache file)
                                             (error
                                              (message "Forge dashboard cache: %s"
                                                       (error-message-string
                                                        cache-error)))))
                                         (let ((forge-dashboard--render-only t))
                                           (magit-refresh-buffer)))
                                       (forge-dashboard--start-refresh)))
                                 (error
                                  (message "Forge dashboard refresh: %s"
                                           (error-message-string failure)))))))
                       (delete-file file)
                       (kill-buffer log))))))
        (error
         (delete-file file)
         (kill-buffer log)
         (message "Forge dashboard refresh: %s" (error-message-string err)))))))

(defun forge-dashboard--save-cache (source)
  "Atomically save the successful snapshot in SOURCE to the cache file."
  (let* ((target (expand-file-name forge-dashboard-cache-file))
         (_ (make-directory (file-name-directory target) t))
         (temp (make-temp-file (concat target "."))))
    (unwind-protect
        (progn
          (copy-file source temp t)
          (set-file-modes temp #o600)
          (rename-file temp target t))
      (when (file-exists-p temp) (delete-file temp)))))

(defun forge-dashboard--visible-attention ()
  "Return cached attention items, honoring current snooze and done marks."
  (let ((forge-dashboard-triage--state-cache
         (forge-dashboard-triage--load-state-cache)))
    (seq-remove
     (lambda (item)
       (let ((data (plist-get item :data)))
         (or (forge-dashboard-triage-snoozed-p (plist-get data :id))
             (forge-dashboard-triage-done-p (plist-get data :id)
                                            (plist-get data :updated)))))
     (plist-get forge-dashboard--snapshot :attention))))

(defun forge-dashboard-refresh-buffer ()
  "Render the last snapshot and request a background database scan.
Magit's refresh machinery preserves point and section visibility."
  (unless forge-dashboard--render-only (forge-dashboard--start-refresh))
  (let* ((repos (plist-get forge-dashboard--snapshot :repos))
         (forge-dashboard--classification-cache
          (make-hash-table :test #'equal))
         (forge-dashboard--repo-data-cache
          (mapcar
           (lambda (data)
             (let ((filtered (copy-sequence data)))
               (plist-put filtered :topics
                          (seq-filter
                           (lambda (topic)
                             (pcase forge-dashboard-topic-type
                               ('pr (forge-pullreq-p topic))
                               ('issue (forge-issue-p topic))
                               (_ t)))
                           (plist-get data :topics)))
               (list forge-dashboard-topic-type (plist-get data :repo) filtered)))
           (plist-get forge-dashboard--snapshot :data))))
    (dolist (entry (plist-get forge-dashboard--snapshot :classes))
      (puthash (car entry) (list (cdr entry))
               forge-dashboard--classification-cache))
    (magit-insert-section (forge-dashboard)
      (insert (propertize "Forge Dashboard"
                          'font-lock-face 'magit-section-heading)
              (propertize (format "  updated %s%s\n\n"
                                  (if forge-dashboard--snapshot
                                      (forge-dashboard--format-updated-label
                                       (plist-get forge-dashboard--snapshot :updated))
                                    "unknown")
                                  (if forge-dashboard--refresh-process
                                      " (refreshing…)" ""))
                          'font-lock-face 'forge-dimmed))
      (if forge-dashboard--snapshot
          (progn
            (forge-dashboard--insert-attention (forge-dashboard--visible-attention))
            (when forge-dashboard-show-owned (forge-dashboard--insert-owned repos))
            (when forge-dashboard-show-organizations
              (forge-dashboard--insert-organizations repos)))
        (insert "  Loading local Forge data in the background…\n")))))

(defun forge-dashboard-refresh ()
  "Request a fresh database scan without blocking the dashboard."
  (interactive)
  (forge-dashboard--cancel-refresh)
  (magit-refresh-buffer))

;;;###autoload
(defun forge-dashboard ()
  "Display cached dashboard data immediately, then update in the background.
Only the local database is scanned; network pulls remain explicit."
  (interactive)
  (let ((buffer (get-buffer-create "*forge-dashboard*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'forge-dashboard-mode)
        (forge-dashboard-mode)
        (when (and forge-dashboard-cache-file
                   (file-readable-p forge-dashboard-cache-file))
          (setq forge-dashboard--snapshot
                (ignore-errors
                  (forge-dashboard--read-snapshot
                   forge-dashboard-cache-file (forge-dashboard--worker-settings)))))
        (let ((forge-dashboard--render-only t)) (magit-refresh-buffer t))))
    (magit-display-buffer buffer)
    (with-current-buffer buffer
      (forge-dashboard--start-refresh))
    buffer))

(defun forge-dashboard-visit ()
  "Visit the topic or repository at point."
  (interactive)
  (cond ((forge-topic-at-point)
         (forge-visit-topic (forge-topic-at-point)))
        ((magit-section-value-if 'forge-dashboard-more)
         (forge-list-topics
          (magit-section-value-if 'forge-dashboard-more)))
        ((forge-repository-at-point)
         (forge-list-topics (forge-repository-at-point)))
        (t (user-error "No topic or repository at point"))))

(defun forge-dashboard-browse ()
  "Browse the topic or repository at point."
  (interactive)
  (cond ((forge-topic-at-point)
         (forge-browse-topic (forge-topic-at-point)))
        ((forge-repository-at-point)
         (forge-browse-repository (forge-repository-at-point)))
        (t (user-error "No topic or repository at point"))))

(defun forge-dashboard-triage (&optional page)
  "Start linear triage over the current dashboard attention queue.
With a prefix argument, prompt for a triage PAGE (see
`forge-dashboard-triage-page-names'); otherwise triage all items."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Triage page: "
                            (forge-dashboard-triage-page-names)
                            nil t nil nil "All"))))
  (forge-dashboard-triage-start
   (forge-dashboard--visible-attention)
   (current-buffer) page))

(defun forge-dashboard-copy-url ()
  "Copy the URL of the topic or repository at point."
  (interactive)
  (let ((object (or (forge-topic-at-point) (forge-repository-at-point))))
    (unless object
      (user-error "No topic or repository at point"))
    (let ((url (forge-get-url object)))
      (kill-new url)
      (message "Copied %s" url))))

(defun forge-dashboard--pull-selective (repo rest buffer)
  "Pull selective REPO, then continue with REST in dashboard BUFFER.
Forge's GitHub and GitLab methods do not invoke their callback for selective
repositories, so use their post-storage message as the completion signal."
  (let (completion)
    (setq completion
          (lambda (pulled _echo done format &rest _args)
            (when (and (eq pulled repo)
                       done
                       (equal format "Storing REPO"))
              (advice-remove 'forge--msg completion)
              (forge-dashboard--pull-repositories rest buffer))))
    (advice-add 'forge--msg :after completion)
    (condition-case err
        (forge--pull repo)
      (error
       (advice-remove 'forge--msg completion)
       (signal (car err) (cdr err))))))

(defun forge-dashboard--pull-repositories (repos buffer)
  "Pull REPOS sequentially, then refresh dashboard BUFFER.
Forge API-backed pulls invoke their callback after storing data, except for
selective repositories.  Classes without a topic API complete synchronously."
  (when (buffer-live-p buffer)
    (if-let* ((repo (car repos)))
        (let ((next (lambda (&rest _args)
                      ;; Forge 0.5.x passes the repository to this callback;
                      ;; newer local Forge revisions call it without arguments.
                      (forge-dashboard--pull-repositories (cdr repos) buffer))))
          (with-current-buffer buffer
            (cond ((or (cl-typep repo 'forge-noapi-repository)
                       (cl-typep repo 'forge-unusedapi-repository))
                   (forge--pull repo)
                   (funcall next))
                  ((oref repo selective-p)
                   (forge-dashboard--pull-selective repo (cdr repos) buffer))
                  (t
                   (forge--pull repo next)))))
      (with-current-buffer buffer
        (magit-refresh)))))

(defun forge-dashboard-pull ()
  "Pull Forge data for each dashboard repository sequentially."
  (interactive)
  (forge-dashboard--pull-repositories
   (forge-dashboard--dashboard-repositories)
   (current-buffer)))

(defun forge-dashboard-pull-all ()
  "Pull Forge data for every tracked repository sequentially.
This includes repositories hidden from the dashboard and repositories on
any supported forge host."
  (interactive)
  (forge-dashboard--pull-repositories
   (forge-dashboard--tracked-repositories)
   (current-buffer)))

(defun forge-dashboard-toggle-owned ()
  "Toggle the owned-repositories section."
  (interactive)
  (setq forge-dashboard-show-owned (not forge-dashboard-show-owned))
  (magit-refresh))

(defun forge-dashboard-toggle-organizations ()
  "Toggle the member-repositories section."
  (interactive)
  (setq forge-dashboard-show-organizations
        (not forge-dashboard-show-organizations))
  (magit-refresh))

(defun forge-dashboard-set-type (type)
  "Set dashboard topic filter to TYPE and refresh."
  (setq forge-dashboard-topic-type type)
  (magit-refresh))

(defun forge-dashboard-show-all ()
  "Show discussions, issues, and pull requests."
  (interactive)
  (forge-dashboard-set-type 'all))

(defun forge-dashboard-show-pullreqs ()
  "Show pull requests only."
  (interactive)
  (forge-dashboard-set-type 'pr))

(defun forge-dashboard-show-issues ()
  "Show issues only."
  (interactive)
  (forge-dashboard-set-type 'issue))

(defun forge-dashboard-set-limit (limit)
  "Set per-repository topic LIMIT, with nil meaning all topics."
  (interactive
   (list (let ((input (read-string "Topics per repository (blank for all): ")))
           (and (not (string-empty-p input))
                (max 0 (string-to-number input))))))
  (setq-local forge-dashboard-topics-per-repo limit)
  (magit-refresh))

(transient-define-prefix forge-dashboard-menu ()
  "Control and refresh the Forge dashboard."
  [["Sections"
    ("o" "Owned repositories" forge-dashboard-toggle-owned)
    ("O" "Member repositories" forge-dashboard-toggle-organizations)]
   ["Topic type"
    ("a" "All" forge-dashboard-show-all)
    ("p" "Pull requests" forge-dashboard-show-pullreqs)
    ("i" "Issues" forge-dashboard-show-issues)]
   ["Display"
    ("l" "Per-repo limit" forge-dashboard-set-limit)]
   ["Refresh"
    ("g" "Local database" forge-dashboard-refresh)
    ("G" "Pull dashboard repos" forge-dashboard-pull)
    ("A" "Pull all tracked repos" forge-dashboard-pull-all)]])

(provide 'forge-dashboard)
;;; forge-dashboard.el ends here
