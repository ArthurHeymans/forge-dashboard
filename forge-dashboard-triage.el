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
(declare-function forge-dashboard--owned-owner-p "forge-dashboard")

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
      (make-directory (file-name-directory file) t))
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
`:mine', `:owned', `:kind', `:approvals', `:review-states', `:draft',
`:merge-conflict', `:last-comment-mine', `:status', `:review-requested',
`:reviewed-by-me', `:activity-age', and `:review-age'."
  (let* ((mine (plist-get data :mine))
         (pullreq (eq (plist-get data :kind) 'pullreq))
         (reviews (plist-get data :review-states))
         (changes (and reviews (memq 'changes-requested reviews))))
    (cond
     ((plist-get data :snoozed) 'snoozed)
     ((and mine pullreq changes) 'changes-requested)
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
     (max 0 (or age 0))))

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
  (and (slot-exists-p object slot)
       (slot-boundp object slot)
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
  (when-let* ((raw (forge-dashboard-triage--field review :state)))
    (pcase (upcase (format "%s" raw))
      ("APPROVED" 'approved)
      ((or "CHANGES_REQUESTED" "CHANGES-REQUESTED") 'changes-requested)
      (_ 'commented))))

(defun forge-dashboard-triage--review-author (review)
  "Return the login associated with REVIEW."
  (or (forge-dashboard-triage--field review :author)
      (forge-dashboard-triage--field review :login)))

(defun forge-dashboard-triage--login (assignee)
  "Return login string for ASSIGNEE."
  (or (and (stringp assignee) assignee)
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
    (list :id (forge-dashboard-triage--slot topic 'id)
          :kind (if (forge-pullreq-p topic) 'pullreq 'topic)
          :mine (and me (equal me (forge-dashboard-triage--slot topic 'author)))
          :owned (and repo (forge-dashboard--owned-owner-p
                            (forge-dashboard-triage--slot repo 'owner)))
          :approvals (and review-states (seq-count
                                         (lambda (state) (eq state 'approved))
                                         review-states))
          :review-states review-states
          :draft (forge-dashboard-triage--slot topic 'draft-p)
          ;; Forge 0.5.x persists neither mergeability nor CI.  Their absence
          ;; intentionally prevents readiness while keeping CI out of the gate.
          :ci nil
          :last-comment-mine
          (and last-post me
               (equal me (forge-dashboard-triage--slot last-post 'author)))
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
          :repo (and repo (forge-dashboard-triage--slot repo 'slug)))))

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

(provide 'forge-dashboard-triage)
;;; forge-dashboard-triage.el ends here
