;;; forge-dashboard-triage.el --- Triage support for Forge dashboard  -*- lexical-binding: t; -*-

;;; Commentary:

;; Local attention classification, urgency ordering, and triage state for
;; `forge-dashboard'.  Classification operates on normalized plists so hosts
;; that do not persist a particular datum can simply omit it.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'forge)
(require 'forge-commands)
(require 'forge-pullreq)
(require 'seq)
(require 'sqlite)
(require 'subr-x)

(defvar forge-dashboard-stale-after)
(declare-function forge-dashboard--age-days "forge-dashboard")
(declare-function forge-dashboard--classify "forge-dashboard")

(defgroup forge-dashboard-triage nil
  "Triage support for the Forge dashboard."
  :group 'forge-dashboard)

(defcustom forge-dashboard-awaiting-review-after 7
  "Days without review activity before an owned pull request needs a nudge."
  :type 'natnum
  :group 'forge-dashboard-triage)

(defcustom forge-dashboard-triage-file
  (locate-user-emacs-file "forge-dashboard-triage.sqlite")
  "SQLite file used for local snooze and done marks."
  :type 'file
  :group 'forge-dashboard-triage)

(defcustom forge-dashboard-nudge-templates
  '((gentle . "Gentle ping — when you have a moment, could you take another look?"))
  "Alist mapping nudge template names to comment text."
  :type '(alist :key-type symbol :value-type string)
  :group 'forge-dashboard-triage)

(defvar forge-dashboard-triage--database nil)
(defvar forge-dashboard-triage--database-file nil)

(defun forge-dashboard-triage-open-store (&optional file)
  "Open and initialize the triage store at FILE.
FILE defaults to `forge-dashboard-triage-file' and may be the special string
\=\":memory:\"."
  (let ((file (or file forge-dashboard-triage-file)))
    (forge-dashboard-triage-close-store)
    (unless (equal file ":memory:")
      (when-let* ((directory (file-name-directory file)))
        (make-directory directory t)))
    (setq forge-dashboard-triage--database
          (sqlite-open (unless (equal file ":memory:") file))
          forge-dashboard-triage--database-file file)
    (sqlite-execute
     forge-dashboard-triage--database
     (concat "CREATE TABLE IF NOT EXISTS topic_state ("
             "topic_id TEXT PRIMARY KEY, snooze_until INTEGER, "
             "done_updated TEXT)"))
    forge-dashboard-triage--database))

(defun forge-dashboard-triage-close-store ()
  "Close the current triage store, if any."
  (when forge-dashboard-triage--database
    (sqlite-close forge-dashboard-triage--database)
    (setq forge-dashboard-triage--database nil
          forge-dashboard-triage--database-file nil)))

(defun forge-dashboard-triage--store ()
  "Return the configured triage store, opening it if necessary."
  (if (and forge-dashboard-triage--database
           (equal forge-dashboard-triage--database-file
                  forge-dashboard-triage-file))
      forge-dashboard-triage--database
    (forge-dashboard-triage-open-store forge-dashboard-triage-file)))

(defun forge-dashboard-triage--put (topic-id snooze-until done-updated)
  "Store TOPIC-ID with SNOOZE-UNTIL and DONE-UPDATED."
  (sqlite-execute
   (forge-dashboard-triage--store)
   (concat "INSERT INTO topic_state(topic_id,snooze_until,done_updated) "
           "VALUES(?,?,?) ON CONFLICT(topic_id) DO UPDATE SET "
           "snooze_until=excluded.snooze_until, "
           "done_updated=excluded.done_updated")
   (vector topic-id snooze-until done-updated)))

(defun forge-dashboard-triage-state (topic-id)
  "Return the stored state plist for TOPIC-ID, or nil."
  (when-let* ((row (car (sqlite-select
                         (forge-dashboard-triage--store)
                         (concat "SELECT snooze_until,done_updated "
                                 "FROM topic_state WHERE topic_id=?")
                         (vector topic-id)))))
    (list :snooze-until (nth 0 row) :done-updated (nth 1 row))))

(defun forge-dashboard-triage-snooze (topic-id until)
  "Snooze TOPIC-ID until time UNTIL."
  (let ((state (forge-dashboard-triage-state topic-id)))
    (forge-dashboard-triage--put
     topic-id (truncate (float-time until)) (plist-get state :done-updated))))

(defun forge-dashboard-triage-done (topic-id updated)
  "Mark TOPIC-ID done at its activity timestamp UPDATED."
  (let ((state (forge-dashboard-triage-state topic-id)))
    (forge-dashboard-triage--put
     topic-id (plist-get state :snooze-until) updated)))

(defun forge-dashboard-triage-snoozed-p (topic-id &optional now)
  "Return non-nil if TOPIC-ID remains snoozed at NOW."
  (let ((until (plist-get (forge-dashboard-triage-state topic-id)
                          :snooze-until)))
    (and until (> until (truncate (float-time (or now (current-time))))))))

(defun forge-dashboard-triage-done-p (topic-id updated)
  "Return non-nil if TOPIC-ID is done and has not changed since UPDATED."
  (let ((done-updated
         (plist-get (forge-dashboard-triage-state topic-id) :done-updated)))
    (and done-updated updated (not (string< done-updated updated)))))

(defun forge-dashboard-attention-state (data)
  "Classify normalized topic DATA into an attention state.
Absent host data does not satisfy rules that depend on that datum.  DATA uses
`:mine', `:owned', `:kind', `:approvals', `:review-states', `:latest-review',
`:draft',
`:merge-conflict', `:last-comment-mine', `:status', `:review-requested',
`:reviewed-by-me', `:activity-age', and `:review-age'."
  (let* ((mine (plist-get data :mine))
         (pullreq (eq (plist-get data :kind) 'pullreq))
         (reviews (plist-get data :review-states))
         (latest-review (or (plist-get data :latest-review)
                            (car (last reviews))))
         (changes (and reviews (memq 'changes-requested reviews))))
    (cond
     ((plist-get data :snoozed) 'snoozed)
     ((and mine pullreq (eq latest-review 'changes-requested))
      'changes-requested)
     ((and mine
           (eq (plist-get data :last-comment-mine) nil)
           (plist-member data :last-comment-mine)
           (memq (plist-get data :status) '(unread pending)))
      'they-replied)
     ((and pullreq (plist-get data :review-requested)
           (not (plist-get data :reviewed-by-me)))
      'review-requested)
     ((and pullreq (or mine (plist-get data :owned))
           (numberp (plist-get data :approvals))
           (> (plist-get data :approvals) 0)
           reviews (not changes)
           (plist-member data :draft) (not (plist-get data :draft))
           (plist-member data :merge-conflict)
           (not (plist-get data :merge-conflict)))
      'ready-to-merge)
     ((and mine pullreq
           (numberp (plist-get data :review-age))
           (>= (plist-get data :review-age)
               forge-dashboard-awaiting-review-after))
      'awaiting-review)
     ((and (numberp (plist-get data :activity-age))
           (> (plist-get data :activity-age) forge-dashboard-stale-after))
      'stale))))

(defun forge-dashboard-urgency-score (state age)
  "Return numeric urgency for STATE and AGE in days."
  (+ (* (pcase state
          ('ready-to-merge 3)
          ((or 'changes-requested 'they-replied 'review-requested) 2)
          ((or 'awaiting-review 'stale) 1)
          (_ 0))
        1000000)
     (- (max 0 (or age 0)))))

(defun forge-dashboard-sort-attention (items)
  "Return a copy of attention ITEMS sorted by urgency.
Each item is a plist containing `:state' and `:age'."
  (sort (copy-sequence items)
        (lambda (left right)
          (> (forge-dashboard-urgency-score
              (plist-get left :state) (plist-get left :age))
             (forge-dashboard-urgency-score
              (plist-get right :state) (plist-get right :age))))))

(defun forge-dashboard-triage--slot (object slot)
  "Return OBJECT's SLOT, or nil when unavailable or unbound."
  (and (eieio-object-p object)
       (slot-exists-p object slot)
       (ignore-errors (slot-value object slot))))

(defun forge-dashboard-triage--field (object field)
  "Return FIELD from plist or alist OBJECT."
  (cond ((and (listp object) (plist-member object field))
         (plist-get object field))
        ((listp object)
         (or (alist-get field object)
             (alist-get (intern (substring (symbol-name field) 1)) object)))))

(defun forge-dashboard-triage--review-state (review)
  "Return normalized state symbol for REVIEW."
  (when-let* ((raw (or (forge-dashboard-triage--field review :state)
                       (forge-dashboard-triage--slot review 'state))))
    (pcase (upcase (format "%s" raw))
      ("APPROVED" 'approved)
      ((or "CHANGES_REQUESTED" "CHANGES-REQUESTED") 'changes-requested)
      (_ 'commented))))

(defun forge-dashboard-triage--review-author (review)
  "Return the login associated with REVIEW."
  (or (forge-dashboard-triage--field review :author)
      (forge-dashboard-triage--field review :login)
      (forge-dashboard-triage--slot review 'author)))

(defun forge-dashboard-triage--login (assignee)
  "Return login string for ASSIGNEE.
Handles login strings, raw (id login name …) database rows as stored
in slots like `review-requests', and EIEIO objects with a login slot."
  (or (and (stringp assignee) assignee)
      (and (consp assignee) (stringp (nth 1 assignee)) (nth 1 assignee))
      (forge-dashboard-triage--slot assignee 'login)))

(defun forge-dashboard-triage-topic-data (topic &optional now)
  "Normalize Forge TOPIC into pure triage data relative to NOW."
  (let* ((repo (ignore-errors (forge-get-repository topic)))
         (me (and repo (ignore-errors (ghub--username repo))))
         (reviews (and (forge-pullreq-p topic)
                       (forge-dashboard-triage--slot topic 'reviews)))
         (review-states (and (listp reviews)
                             (delq nil (mapcar
                                        #'forge-dashboard-triage--review-state
                                        reviews))))
         (posts (forge-dashboard-triage--slot topic 'posts))
         (last-post (car (last (and (listp posts) posts))))
         (updated (or (forge-dashboard-triage--slot topic 'updated)
                      (forge-dashboard-triage--slot topic 'created)))
         (age (forge-dashboard--age-days updated now))
         (requests (and (forge-pullreq-p topic)
                        (forge-dashboard-triage--slot topic 'review-requests))))
    (append
     (list :id (forge-dashboard-triage--slot topic 'id)
           :kind (if (forge-pullreq-p topic) 'pullreq 'topic)
           :mine (and me (equal me (forge-dashboard-triage--slot topic 'author)))
           :owned (and repo (eq (forge-dashboard--classify repo) 'owned))
           :approvals (and review-states (seq-count
                                          (lambda (state) (eq state 'approved))
                                          review-states))
           :review-states review-states
           :latest-review (car (last review-states))
           :draft (forge-dashboard-triage--slot topic 'draft-p)
           ;; Forge 0.5.x persists neither mergeability nor CI.  Their absence
           ;; intentionally prevents readiness while keeping CI out of the gate.
           :ci nil
           :status (forge-dashboard-triage--slot topic 'status)
           :review-requested
           (and me (seq-some (lambda (request)
                               (equal me (forge-dashboard-triage--login request)))
                             requests))
           :reviewed-by-me
           (and me (seq-some (lambda (review)
                               (equal me
                                      (forge-dashboard-triage--review-author review)))
                             reviews))
           :activity-age age :review-age age :updated updated
           :repo (and repo (ignore-errors (oref repo slug))))
     (when last-post
       (list :last-comment-mine
             (and me (equal me (forge-dashboard-triage--slot
                                last-post 'author))))))))

(defun forge-dashboard-triage-item (topic &optional now)
  "Return an attention item for TOPIC at NOW, or nil."
  (let* ((data (forge-dashboard-triage-topic-data topic now))
         (id (plist-get data :id))
         (updated (plist-get data :updated)))
    (unless (or (forge-dashboard-triage-snoozed-p id now)
                (forge-dashboard-triage-done-p id updated))
      (when-let* ((state (forge-dashboard-attention-state data)))
        (list :topic topic :data data :state state
              :age (plist-get data :activity-age))))))

(defvar-local forge-dashboard-triage--items nil)
(defvar-local forge-dashboard-triage--position 0)
(defvar-local forge-dashboard-triage--source-buffer nil)

(defun forge-dashboard-triage--current-topic ()
  "Return the current triage or dashboard topic."
  (or (and (derived-mode-p 'forge-dashboard-triage-mode)
           (plist-get (nth forge-dashboard-triage--position
                           forge-dashboard-triage--items)
                      :topic))
      (forge-topic-at-point t)))

(defun forge-dashboard-triage--refresh-source ()
  "Refresh the dashboard from which triage was started."
  (when (buffer-live-p forge-dashboard-triage--source-buffer)
    (with-current-buffer forge-dashboard-triage--source-buffer
      (magit-refresh))))

(defun forge-dashboard-triage--advance ()
  "Advance to the next triage item."
  (when (derived-mode-p 'forge-dashboard-triage-mode)
    (cl-incf forge-dashboard-triage--position)
    (forge-dashboard-triage--render)))

(defun forge-dashboard-triage--finish-action ()
  "Refresh the dashboard and advance triage after an action."
  (if (derived-mode-p 'forge-dashboard-mode)
      (magit-refresh)
    (forge-dashboard-triage--refresh-source)
    (forge-dashboard-triage--advance)))

(defun forge-dashboard-triage--read-until ()
  "Read a snooze duration and return its ending time."
  (pcase (read-char-choice "Snooze: [1] day, [3] days, [7] week, [c]ustom "
                           '(?1 ?3 ?7 ?c))
    (?1 (time-add nil (days-to-time 1)))
    (?3 (time-add nil (days-to-time 3)))
    (?7 (time-add nil (days-to-time 7)))
    (?c (date-to-time (read-string "Snooze until (date/time): ")))))

(defun forge-dashboard-snooze (&optional until)
  "Snooze the topic at point until UNTIL."
  (interactive)
  (let ((topic (forge-dashboard-triage--current-topic)))
    (forge-dashboard-triage-snooze
     (forge-dashboard-triage--slot topic 'id)
     (or until (forge-dashboard-triage--read-until))))
  (forge-dashboard-triage--finish-action))

(defun forge-dashboard-done ()
  "Hide the topic at point until it receives new activity."
  (interactive)
  (let ((topic (forge-dashboard-triage--current-topic)))
    (forge-dashboard-triage-done
     (forge-dashboard-triage--slot topic 'id)
     (or (forge-dashboard-triage--slot topic 'updated)
         (forge-dashboard-triage--slot topic 'created))))
  (forge-dashboard-triage--finish-action))

(defun forge-dashboard-merge ()
  "Merge the ready pull request at point using Forge."
  (interactive)
  (let* ((topic (forge-dashboard-triage--current-topic))
         (item (forge-dashboard-triage-item topic)))
    (unless (and (forge-pullreq-p topic)
                 (eq (plist-get item :state) 'ready-to-merge))
      (user-error "This pull request is not ready to merge"))
    (forge-merge topic (forge-select-merge-method)))
  (forge-dashboard-triage--finish-action))

(defun forge-dashboard-checkout ()
  "Check out the pull request at point."
  (interactive)
  (let ((origin (current-buffer))
        (topic (forge-dashboard-triage--current-topic)))
    (unless (forge-pullreq-p topic)
      (user-error "The topic at point is not a pull request"))
    (forge-checkout-pullreq topic)
    (when (buffer-live-p origin)
      (with-current-buffer origin
        (forge-dashboard-triage--finish-action)))))

(defun forge-dashboard-close-topic ()
  "Close the topic at point through its Forge API."
  (interactive)
  (let ((topic (forge-dashboard-triage--current-topic)))
    (forge--set-topic-state
     (forge-get-repository topic) topic
     (if (forge-pullreq-p topic) 'rejected 'completed)))
  (forge-dashboard-triage--finish-action))

(defun forge-dashboard-nudge ()
  "Open a pre-filled Forge comment using a configured nudge template."
  (interactive)
  (let* ((topic (forge-dashboard-triage--current-topic))
         (name (intern
                (completing-read "Nudge template: "
                                 (mapcar (lambda (entry)
                                           (symbol-name (car entry)))
                                         forge-dashboard-nudge-templates)
                                 nil t nil nil
                                 (symbol-name
                                  (caar forge-dashboard-nudge-templates)))))
         (text (alist-get name forge-dashboard-nudge-templates)))
    (forge-dashboard-triage--finish-action)
    (forge-visit-topic topic)
    (forge-create-post)
    (goto-char (point-max))
    (insert text)))

(defun forge-dashboard-triage-visit ()
  "Visit the current topic and advance the triage queue."
  (interactive)
  (let ((topic (forge-dashboard-triage--current-topic)))
    (forge-dashboard-triage--advance)
    (forge-visit-topic topic)))

(defun forge-dashboard-triage-browse ()
  "Browse the current topic and advance the triage queue."
  (interactive)
  (let ((topic (forge-dashboard-triage--current-topic)))
    (forge-dashboard-triage--advance)
    (forge-browse-topic topic)))

(defun forge-dashboard-triage-skip ()
  "Skip the current triage item."
  (interactive)
  (forge-dashboard-triage--advance))

(defun forge-dashboard-triage-quit ()
  "Quit the triage buffer."
  (interactive)
  (quit-window t))

(defvar-keymap forge-dashboard-triage-mode-map
  :doc "Keymap for the linear Forge dashboard triage flow."
  "M" #'forge-dashboard-merge
  "RET" #'forge-dashboard-triage-visit
  "<return>" #'forge-dashboard-triage-visit
  "b" #'forge-dashboard-triage-browse
  "c" #'forge-dashboard-checkout
  "C" #'forge-dashboard-nudge
  "z" #'forge-dashboard-snooze
  "d" #'forge-dashboard-done
  "x" #'forge-dashboard-close-topic
  "SPC" #'forge-dashboard-triage-skip
  "q" #'forge-dashboard-triage-quit)

(define-derived-mode forge-dashboard-triage-mode special-mode "Forge Triage"
  "Major mode for one-item-at-a-time Forge dashboard triage.")

(defun forge-dashboard-triage--render ()
  "Render the current item in a triage buffer."
  (let ((inhibit-read-only t)
        (item (nth forge-dashboard-triage--position
                   forge-dashboard-triage--items)))
    (erase-buffer)
    (if (not item)
        (insert "Triage complete.  Press q to quit.\n")
      (let* ((topic (plist-get item :topic))
             (data (plist-get item :data))
             (ci (plist-get data :ci)))
        (insert (format "Item %d/%d\n\n"
                        (1+ forge-dashboard-triage--position)
                        (length forge-dashboard-triage--items))
                (format "%s #%s  %s\n"
                        (or (plist-get data :repo) "unknown")
                        (forge-dashboard-triage--slot topic 'number)
                        (forge-dashboard-triage--slot topic 'title))
                (format "State: %s  approvals: %s  CI: %s  last activity: %dd\n\n"
                        (plist-get item :state)
                        (or (plist-get data :approvals) "?")
                        (pcase ci ('success "✓") ('failed "✗") (_ "?"))
                        (plist-get item :age))
                "M merge  RET visit  b browse  c checkout  C nudge\n"
                "z snooze  d done  x close  SPC skip  q quit\n")))
    (goto-char (point-min))))

(defun forge-dashboard-triage-start (items &optional source-buffer)
  "Start linear triage over urgency-sorted ITEMS from SOURCE-BUFFER."
  (interactive)
  (let ((buffer (get-buffer-create "*Forge Dashboard Triage*")))
    (with-current-buffer buffer
      (forge-dashboard-triage-mode)
      (setq forge-dashboard-triage--items
            (forge-dashboard-sort-attention items)
            forge-dashboard-triage--position 0
            forge-dashboard-triage--source-buffer source-buffer)
      (forge-dashboard-triage--render))
    (pop-to-buffer buffer)))

(provide 'forge-dashboard-triage)
;;; forge-dashboard-triage.el ends here
