;;; forge-dashboard.el --- Triage-oriented Forge dashboard  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1") (forge "0.5.0") (magit "4.0.0") (transient "0.9.0"))

;;; Commentary:

;; A compact, local-only overview of open Forge topics in owned and
;; organization repositories.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'forge)
(require 'forge-commands)
(require 'forge-topics)
(require 'magit-section)
(require 'seq)
(require 'subr-x)
(require 'transient)

(defgroup forge-dashboard nil
  "A triage-oriented dashboard for Forge."
  :group 'forge)

(defcustom forge-dashboard-organizations nil
  "Organization owners whose tracked repositories appear in the dashboard."
  :type '(repeat string)
  :group 'forge-dashboard)

(defcustom forge-dashboard-topics-per-repo 3
  "Maximum number of open topics initially shown for each repository."
  :type 'natnum
  :group 'forge-dashboard)

(defcustom forge-dashboard-stale-after 14
  "Number of days after which a topic is considered stale."
  :type 'natnum
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

(defun forge-dashboard--repo-topics (repo)
  "Return selected open topics for REPO, newest activity first."
  (let ((types (pcase forge-dashboard-topic-type
                 ('pr '(pullreq))
                 ('issue '(issue))
                 (_ '(issue pullreq)))))
    (sort (mapcan (lambda (type)
                    (forge--list-topics (forge-dashboard--topic-spec type)
                                        repo type))
                  types)
          (lambda (left right)
            (string> (or (oref left updated) "")
                     (or (oref right updated) ""))))))

(defun forge-dashboard--repo-data (repo)
  "Compute counts and selected topic objects for REPO."
  (let* ((issues (forge--list-topics (forge-dashboard--topic-spec 'issue)
                                     repo 'issue))
         (pullreqs (forge--list-topics (forge-dashboard--topic-spec 'pullreq)
                                       repo 'pullreq))
         (all (append issues pullreqs))
         (topics (pcase forge-dashboard-topic-type
                   ('pr pullreqs)
                   ('issue issues)
                   (_ (sort (copy-sequence all)
                            (lambda (left right)
                              (string> (or (oref left updated) "")
                                       (or (oref right updated) ""))))))))
    (list :repo repo
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

(defun forge-dashboard--owned-owner-p (owner)
  "Return non-nil when OWNER is configured in `forge-owned-accounts'."
  (member owner (mapcar #'car forge-owned-accounts)))

(defun forge-dashboard--dashboard-repositories ()
  "Return all tracked repositories selected by dashboard configuration."
  (seq-filter
   (lambda (repo)
     (or (forge-dashboard--owned-owner-p (oref repo owner))
         (member (oref repo owner) forge-dashboard-organizations)))
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
  (if-let* ((updated (forge-dashboard--latest-update)))
      (format "%s ago"
              (forge-dashboard--age-label
               (forge-dashboard--age-days updated)))
    "never"))

(defun forge-dashboard--insert-topic (topic)
  "Insert one TOPIC row."
  (let* ((row (forge-dashboard--topic-row topic))
         (age (plist-get row :age))
         (status (plist-get row :status))
         (row-face (pcase status
                     ('unread 'forge-topic-slug-unread)
                     ('pending 'forge-dashboard-pending))))
    (magit-insert-section ((eieio-object-class topic) topic)
      (insert
       (propertize
        (format "#%-5d %-48s %-5s @%-16s "
                (plist-get row :number)
                (truncate-string-to-width (plist-get row :title) 48 nil nil t)
                (plist-get row :type)
                (plist-get row :author))
        'face row-face)
       (propertize (forge-dashboard--age-label age)
                   'face (forge-dashboard--age-face age))
       "\n"))))

(defun forge-dashboard--insert-repository (data)
  "Insert repository DATA and its topic children."
  (let* ((repo (plist-get data :repo))
         (topics (plist-get data :topics))
         (visible (seq-take topics forge-dashboard-topics-per-repo))
         (remaining (- (length topics) (length visible))))
    (magit-insert-section (forge-repo repo)
      (magit-insert-heading
        (format "%s  %d PR  %d issue  %d unread"
                (oref repo slug)
                (plist-get data :open-pullreqs)
                (plist-get data :open-issues)
                (plist-get data :unread)))
      (dolist (topic visible)
        (forge-dashboard--insert-topic topic))
      (when (> remaining 0)
        (magit-insert-section (forge-dashboard-more repo)
          (insert (format "…%d more (RET to list all)\n" remaining)))))))

(defun forge-dashboard--insert-repositories (heading repos)
  "Insert a section named HEADING containing REPOS."
  (magit-insert-section (forge-dashboard-group)
    (magit-insert-heading heading)
    (if repos
        (dolist (repo repos)
          (forge-dashboard--insert-repository
           (forge-dashboard--repo-data repo)))
      (insert "No matching tracked repositories\n"))))

(defun forge-dashboard--insert-owned (repos)
  "Insert owned repositories selected from REPOS."
  (forge-dashboard--insert-repositories
   "Owned repositories"
   (seq-filter (lambda (repo)
                 (forge-dashboard--owned-owner-p (oref repo owner)))
               repos)))

(defun forge-dashboard--insert-organizations (repos)
  "Insert organization repositories selected from REPOS, grouped by owner."
  (magit-insert-section (forge-dashboard-organizations)
    (magit-insert-heading "Organizations")
    (let ((groups
           (seq-group-by
            (lambda (repo) (oref repo owner))
            (seq-filter
             (lambda (repo)
               (and (member (oref repo owner) forge-dashboard-organizations)
                    (not (forge-dashboard--owned-owner-p (oref repo owner)))))
             repos))))
      (if groups
          (dolist (group groups)
            (magit-insert-section (forge-dashboard-organization (car group))
              (magit-insert-heading (car group))
              (dolist (repo (cdr group))
                (forge-dashboard--insert-repository
                 (forge-dashboard--repo-data repo)))))
        (insert "No matching tracked repositories\n")))))

(defvar-keymap forge-dashboard-mode-map
  :doc "Keymap for `forge-dashboard-mode'."
  :parent (make-composed-keymap forge-common-map magit-mode-map)
  "RET" #'forge-dashboard-visit
  "<return>" #'forge-dashboard-visit
  "b" #'forge-dashboard-browse
  "y" #'forge-dashboard-copy-url
  "g" #'magit-refresh
  "G" #'forge-dashboard-pull
  "?" #'forge-dashboard-menu)

(define-derived-mode forge-dashboard-mode magit-mode "Forge Dashboard"
  "Major mode for the Forge dashboard."
  :interactive nil
  (setq-local default-directory "/")
  (setq-local forge-buffer-unassociated-p t))

(defun forge-dashboard-refresh-buffer ()
  "Render the Forge dashboard from the local database."
  (let ((repos (forge-dashboard--tracked-repositories)))
    (magit-insert-section (forge-dashboard)
      (insert (propertize "Forge Dashboard" 'face 'bold)
              (format "  updated %s\n\n" (forge-dashboard--updated-label)))
      (when forge-dashboard-show-owned
        (forge-dashboard--insert-owned repos))
      (when forge-dashboard-show-organizations
        (forge-dashboard--insert-organizations repos)))))

;;;###autoload
(defun forge-dashboard ()
  "Display the Forge dashboard."
  (interactive)
  (magit-setup-buffer #'forge-dashboard-mode nil
    :buffer (get-buffer-create "*forge-dashboard*")
    (default-directory "/")
    (forge-buffer-unassociated-p t)))

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

(defun forge-dashboard-copy-url ()
  "Copy the URL of the topic or repository at point."
  (interactive)
  (let ((object (or (forge-topic-at-point) (forge-repository-at-point))))
    (unless object
      (user-error "No topic or repository at point"))
    (let ((url (forge-get-url object)))
      (kill-new url)
      (message "Copied %s" url))))

(defun forge-dashboard-pull ()
  "Pull Forge data for each repository represented by the dashboard."
  (interactive)
  (dolist (repo (forge-dashboard--dashboard-repositories))
    (let ((forge-buffer-repository (oref repo id)))
      (call-interactively #'forge-pull)))
  (magit-refresh))

(defun forge-dashboard-toggle-owned ()
  "Toggle the owned-repositories section."
  (interactive)
  (setq forge-dashboard-show-owned (not forge-dashboard-show-owned))
  (magit-refresh))

(defun forge-dashboard-toggle-organizations ()
  "Toggle the organizations section."
  (interactive)
  (setq forge-dashboard-show-organizations
        (not forge-dashboard-show-organizations))
  (magit-refresh))

(defun forge-dashboard-set-type (type)
  "Set dashboard topic filter to TYPE and refresh."
  (setq forge-dashboard-topic-type type)
  (magit-refresh))

(defun forge-dashboard-show-all ()
  "Show issues and pull requests."
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
  "Set per-repository topic LIMIT."
  (interactive (list (read-number "Topics per repository: "
                                  forge-dashboard-topics-per-repo)))
  (setq-local forge-dashboard-topics-per-repo (max 0 limit))
  (magit-refresh))

(transient-define-prefix forge-dashboard-menu ()
  "Control and refresh the Forge dashboard."
  [["Sections"
    ("o" "Owned repositories" forge-dashboard-toggle-owned)
    ("O" "Organizations" forge-dashboard-toggle-organizations)]
   ["Topic type"
    ("a" "All" forge-dashboard-show-all)
    ("p" "Pull requests" forge-dashboard-show-pullreqs)
    ("i" "Issues" forge-dashboard-show-issues)]
   ["Display"
    ("l" "Per-repo limit" forge-dashboard-set-limit)]
   ["Refresh"
    ("g" "Local database" magit-refresh)
    ("G" "Pull dashboard repos" forge-dashboard-pull)]])

(provide 'forge-dashboard)
;;; forge-dashboard.el ends here
