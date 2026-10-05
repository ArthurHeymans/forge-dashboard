;;; forge-dashboard-test.el --- Tests for forge-dashboard  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'forge-dashboard)
(require 'forge-dashboard-triage)
(require 'forge-discussion)
(require 'forge-issue)
(require 'forge-pullreq)
(require 'forge-github)

(defclass forge-dashboard-test-database (forge-database) ())

(defclass forge-dashboard-test-repository ()
  ((id :initarg :id)
   (owner :initarg :owner)
   (name :initarg :name)
   (slug :initarg :slug)
   (selective-p :initarg :selective-p :initform nil)))

(defun forge-dashboard-test--repository (name &optional selective)
  "Make a synthetic repository named NAME with SELECTIVE pull behavior."
  (forge-dashboard-test-repository
   :id (format "repo-%s" name)
   :owner "owner" :name name :slug (format "owner/%s" name)
   :selective-p selective))

(defun forge-dashboard-test--issue (&rest slots)
  "Make a synthetic issue initialized with SLOTS."
  (apply #'forge-issue
         :id "issue-id" :repository "repo-id" :number 12 :slug "#12" :state 'open
         :author "octocat" :title "Triage this" :created "2025-01-01T00:00:00Z"
         :updated "2025-01-10T00:00:00Z" :status 'done
         slots))

(ert-deftest forge-dashboard-repo-login-uses-ghub-repository-method ()
  (let ((repo (forge-dashboard-test--repository "login")))
    (cl-letf (((symbol-function 'ghub--username)
               (lambda (object)
                 (should (eq object repo))
                 "octocat")))
      (should (equal (forge-dashboard--repo-login repo) "octocat")))))

(ert-deftest forge-dashboard-topic-row-is-pure-data ()
  (let* ((topic (forge-dashboard-test--issue :status 'unread))
         (now (date-to-time "2025-01-20T00:00:00Z"))
         (row (forge-dashboard--topic-row topic now)))
    (should (= (plist-get row :number) 12))
    (should (equal (plist-get row :title) "Triage this"))
    (should (equal (plist-get row :type) "issue"))
    (should (eq (plist-get row :status) 'unread))
    (should (= (plist-get row :age) 10))))

(ert-deftest forge-dashboard-topic-row-recognizes-discussion ()
  (let ((topic (forge-discussion
                :id "discussion-id" :repository "repo-id" :number 3
                :state 'open :author "hubot" :title "What do you think?"
                :created "2025-01-01T00:00:00Z"
                :updated "2025-01-03T00:00:00Z" :status 'unread)))
    (should (equal (plist-get (forge-dashboard--topic-row topic) :type)
                   "discussion"))))

(ert-deftest forge-dashboard-repo-data-includes-discussions ()
  (let* ((discussion (forge-discussion
                      :id "discussion-id" :repository "repo-id" :number 3
                      :state 'open :author "hubot" :title "Discuss"
                      :created "2025-01-01T00:00:00Z"
                      :updated "2025-01-12T00:00:00Z" :status 'unread))
         (issue (forge-dashboard-test--issue :updated "2025-01-11T00:00:00Z"))
         (pullreq (forge-pullreq
                   :id "pr-id" :repository "repo-id" :number 7 :state 'open
                   :author "hubot" :title "Ship it"
                   :created "2025-01-01T00:00:00Z"
                   :updated "2025-01-10T00:00:00Z" :status 'done))
         (forge-dashboard-topic-type 'all)
         calls)
    (cl-letf (((symbol-function 'forge--list-topics)
               (lambda (spec _repo type)
                 (push (cons (oref spec type) type) calls)
                 (pcase type
                   ('discussion (list discussion))
                   ('issue (list issue))
                   ('pullreq (list pullreq))))))
      (let ((data (forge-dashboard--repo-data issue)))
        (should (equal (plist-get data :topics)
                       (list discussion issue pullreq)))
        (should (= (plist-get data :open-topics) 3))
        (should (= (plist-get data :open-issues) 1))
        (should (= (plist-get data :open-pullreqs) 1))
        (should (= (plist-get data :unread) 1))
        (should (equal (nreverse calls)
                       '((discussion . discussion)
                         (issue . issue)
                         (pullreq . pullreq))))))))

(ert-deftest forge-dashboard-topic-row-recognizes-pull-request ()
  (let ((topic (forge-pullreq
                :id "pr-id" :repository "repo-id" :number 7 :state 'open
                :author "hubot" :title "Ship it" :created "2025-01-01T00:00:00Z"
                :updated "2025-01-02T00:00:00Z" :status 'pending)))
    (should (equal (plist-get (forge-dashboard--topic-row topic) :type) "PR"))))

(ert-deftest forge-dashboard-ci-badge-marks-unavailable-data ()
  (should (equal (forge-dashboard--ci-badge nil) "CI n/a"))
  (should (equal (forge-dashboard--ci-badge 'success) "CI ✓"))
  (should (equal (forge-dashboard--ci-badge 'failed) "CI ✗")))

(ert-deftest forge-dashboard-unread-face-is-red-and-bold ()
  (should (eq (face-attribute 'forge-dashboard-unread :inherit nil t)
              'forge-topic-slug-unread))
  (should (equal (face-attribute 'forge-dashboard-unread :foreground nil t)
                 "red")))

(ert-deftest forge-dashboard-unread-badge-is-explicit-and-aligned ()
  (let ((unread (forge-dashboard--unread-badge 'unread))
        (done (forge-dashboard--unread-badge 'done)))
    (should (equal (substring-no-properties unread) "[NEW] "))
    (should (eq (get-text-property 0 'font-lock-face unread)
                'forge-dashboard-unread))
    (should (= (string-width unread) (string-width done)))))

(ert-deftest forge-dashboard-age-ramp-boundaries ()
  (let ((forge-dashboard-stale-after 14))
    (should (eq (forge-dashboard--age-face 7) 'forge-dashboard-age-fresh))
    (should (eq (forge-dashboard--age-face 8) 'forge-dashboard-age-aging))
    (should (eq (forge-dashboard--age-face 14) 'forge-dashboard-age-aging))
    (should (eq (forge-dashboard--age-face 15) 'forge-dashboard-age-stale))))

(ert-deftest forge-dashboard-age-handles-future-and-missing-dates ()
  (let ((now (date-to-time "2025-01-01T00:00:00Z")))
    (should (= (forge-dashboard--age-days nil now) 0))
    (should (= (forge-dashboard--age-days "2025-01-02T00:00:00Z" now) 0))))

(ert-deftest forge-dashboard-updated-label-is-grammatical ()
  (cl-letf (((symbol-function 'forge-dashboard--latest-update)
             (lambda () (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                             (current-time) t))))
    (should (equal (forge-dashboard--updated-label) "<1d ago"))))

(ert-deftest forge-dashboard-classification-derives-from-login ()
  ;; Login match wins without any configuration.
  (should (eq (forge-dashboard--classification "me" "me" nil nil nil) 'owned))
  ;; Assignability alone makes a repository a member repository.
  (should (eq (forge-dashboard--classification "org" "me" nil nil t) 'member))
  ;; Unrelated repositories stay external.
  (should-not (forge-dashboard--classification "other" "me" nil nil nil))
  ;; A missing login degrades to the explicit overrides only.
  (should-not (forge-dashboard--classification "me" nil nil nil nil)))

(ert-deftest forge-dashboard-classification-cache-uses-repository-id ()
  (let ((forge-dashboard--classification-cache
         (make-hash-table :test #'equal))
        (calls 0))
    (cl-letf (((symbol-function 'forge-dashboard--classify-uncached)
               (lambda (repo)
                 (cl-incf calls)
                 (and (equal (oref repo name) "owned") 'owned))))
      ;; Forge reconstructs a fresh repository object for each topic.
      (should (eq (forge-dashboard--classify
                   (forge-dashboard-test--repository "owned")) 'owned))
      (should (eq (forge-dashboard--classify
                   (forge-dashboard-test--repository "owned")) 'owned))
      (should-not (forge-dashboard--classify
                   (forge-dashboard-test--repository "external")))
      (should-not (forge-dashboard--classify
                   (forge-dashboard-test--repository "external")))
      (should (= calls 2)))))

(ert-deftest forge-dashboard-classification-honors-overrides ()
  ;; `forge-owned-accounts' still forces ownership.
  (should (eq (forge-dashboard--classification "mine" nil '("mine") nil nil)
              'owned))
  ;; Configured organizations count without assignee data.
  (should (eq (forge-dashboard--classification "org" "me" nil '("org") nil)
              'member))
  ;; Owned wins over member when both match.
  (should (eq (forge-dashboard--classification "me" "me" nil '("me") t)
              'owned)))

(ert-deftest forge-dashboard-active-repo-data-hides-empty-repositories ()
  (let ((active (forge-dashboard-test--repository "active"))
        (empty (forge-dashboard-test--repository "empty")))
    (cl-letf (((symbol-function 'forge-dashboard--repo-data)
               (lambda (repo)
                 (list :repo repo :open-topics
                       (if (equal (oref repo name) "active") 1 0)))))
      (should (equal (forge-dashboard--active-repo-data (list active empty))
                     (list (list :repo active :open-topics 1)))))))

(ert-deftest forge-dashboard-repository-data-is-cached-per-refresh ()
  (let* ((repo (forge-dashboard-test--repository "cached"))
         (forge-dashboard--repo-data-cache nil)
         (forge-dashboard-topic-type 'all)
         (calls 0)
         (data (list :repo repo :open-topics 1)))
    (cl-letf (((symbol-function 'forge-dashboard--repo-data)
               (lambda (_repo) (cl-incf calls) data)))
      (should (eq (forge-dashboard--cached-repo-data repo) data))
      (should (eq (forge-dashboard--cached-repo-data repo) data))
      (should (= calls 1))
      (let ((forge-dashboard-topic-type 'pr))
        (should (eq (forge-dashboard--cached-repo-data repo) data))
        (should (= calls 2)))
      (should (eq (forge-dashboard--cached-repo-data repo) data))
      (should (= calls 2)))))

(ert-deftest forge-dashboard-repository-heading-hides-zero-counts ()
  (let* ((repo (forge-dashboard-test--repository "colorful"))
         (plain (substring-no-properties
                 (forge-dashboard--repository-heading
                  (list :repo repo :open-pullreqs 0 :open-issues 0 :unread 0))))
         (active (substring-no-properties
                  (forge-dashboard--repository-heading
                   (list :repo repo :open-pullreqs 2 :open-issues 0 :unread 1)))))
    (should (equal plain "owner/colorful"))
    (should (eq (get-text-property
                 0 'font-lock-face
                 (forge-dashboard--repository-heading
                  (list :repo repo :open-pullreqs 0 :open-issues 0 :unread 0)))
                'bold))
    (should (equal active "owner/colorful  2 PR  1 unread"))))

(ert-deftest forge-dashboard-repositories-are-collapsed-with-all-topics ()
  (let ((repo (forge-dashboard-test--repository "compact"))
        (topic (forge-dashboard-test--issue))
        (forge-dashboard-topics-per-repo nil))
    (with-temp-buffer
      (forge-dashboard-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (forge-dashboard-test-root)
          (forge-dashboard--insert-repository
           (list :repo repo :topics (list topic)
                 :open-pullreqs 0 :open-issues 1 :unread 0)
           1)))
      (goto-char (point-min))
      (let ((section (magit-current-section)))
        (should (eq (oref section type) 'forge-repo))
        (should (oref section hidden))
        (should (string-match-p
                 "^  owner/compact  1 issue$"
                 (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (string-match-p
                     "Triage this"
                     (buffer-substring-no-properties (point-min) (point-max))))
        (magit-section-show section)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "^          #12.*Triage this" text))
          (should-not (string-match-p "more" text)))))))

(ert-deftest forge-dashboard-remaps-prompting-browse-commands ()
  (with-temp-buffer
    (forge-dashboard-mode)
    (dolist (command '(magit-browse-thing forge-browse-topic
                       forge-browse-discussion forge-browse-issue
                       forge-browse-pullreq))
      (should (eq (command-remapping command) 'forge-dashboard-browse)))
    (should (eq (key-binding (kbd "b")) 'forge-dashboard-browse))
    (should (eq (key-binding (kbd "o")) 'forge-dashboard-browse))))

(ert-deftest forge-dashboard-topic-section-dispatches-actions ()
  (let ((topic (forge-dashboard-test--issue))
        visited browsed copied)
    (with-temp-buffer
      (forge-dashboard-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (forge-dashboard-test-root)
          (forge-dashboard--insert-topic topic)))
      (goto-char (point-min))
      (should (eq (oref (magit-current-section) type) 'issue))
      (should (eq (forge-topic-at-point) topic))
      (cl-letf (((symbol-function 'forge-visit-topic)
                 (lambda (object) (setq visited object)))
                ((symbol-function 'forge-browse-topic)
                 (lambda (object) (setq browsed object)))
                ((symbol-function 'forge-get-url)
                 (lambda (_object) "https://example.test/topic"))
                ((symbol-function 'kill-new)
                 (lambda (text &optional _replace) (setq copied text)))
                ((symbol-function 'message) #'ignore))
        (forge-dashboard-visit)
        (forge-dashboard-browse)
        (forge-dashboard-copy-url)))
    (should (eq visited topic))
    (should (eq browsed topic))
    (should (equal copied "https://example.test/topic"))))

(ert-deftest forge-dashboard-selective-pull-waits-for-storage-and-continues ()
  (let* ((first (forge-dashboard-test--repository "first" t))
         (second (forge-dashboard-test--repository "second"))
         calls callback
         (refreshes 0))
    (cl-letf (((symbol-function 'forge-dashboard--dashboard-repositories)
               (lambda () (list first second)))
              ((symbol-function 'forge--pull)
               (lambda (repo &optional supplied-callback &rest _)
                 (push repo calls)
                 ;; Match Forge: selective GitHub/GitLab pulls omit CALLBACK.
                 (unless (oref repo selective-p)
                   (setq callback supplied-callback))))
              ((symbol-function 'magit-refresh)
               (lambda () (cl-incf refreshes))))
      (forge-dashboard-pull)
      (should (equal (reverse calls) (list first)))
      (should-not callback)
      (should (zerop refreshes))
      (forge--msg first nil t "Storing REPO")
      (should (equal (reverse calls) (list first second)))
      (should callback)
      (should (zerop refreshes))
      (funcall callback)
      (should (= refreshes 1)))))

(ert-deftest forge-dashboard-attention-classifies-each-state ()
  (let ((forge-dashboard-stale-after 14)
        (forge-dashboard-awaiting-review-after 7))
    (should (eq (forge-dashboard-attention-state
                 '(:kind pullreq :mine t :approvals 1
                   :review-states (approved) :draft nil
                   :merge-conflict nil :ci failed :activity-age 1))
                'ready-to-merge))
    (should (eq (forge-dashboard-attention-state
                 '(:kind pullreq :mine t
                   :review-states (changes-requested) :activity-age 1))
                'changes-requested))
    (should (eq (forge-dashboard-attention-state
                 '(:mine t :last-comment-mine nil :status unread
                   :activity-age 1))
                'they-replied))
    (should (eq (forge-dashboard-attention-state
                 '(:kind pullreq :review-requested t :reviewed-by-me nil
                   :activity-age 1))
                'review-requested))
    (should (eq (forge-dashboard-attention-state
                 '(:kind pullreq :mine t :review-age 7 :activity-age 7))
                'awaiting-review))
    (should (eq (forge-dashboard-attention-state '(:activity-age 15))
                'stale))
    (should (eq (forge-dashboard-attention-state '(:snoozed t)) 'snoozed))))

(ert-deftest forge-dashboard-ready-ignores-ci-but-degrades-missing-data ()
  (let ((base '(:kind pullreq :mine t :approvals 1
                :review-states (approved) :draft nil
                :merge-conflict nil :activity-age 1)))
    (should (eq (forge-dashboard-attention-state
                 (append base '(:ci failed)))
                'ready-to-merge))
    (should (eq (forge-dashboard-attention-state base) 'ready-to-merge))
    ;; Another reviewer's later approval cannot clear a changes request.
    (should-not (forge-dashboard-attention-state
                 '(:kind pullreq :mine t :approvals 1
                   :review-states (changes-requested approved)
                   :latest-review approved :draft nil
                   :merge-conflict nil :activity-age 1)))
    (should-not (forge-dashboard-attention-state
                 '(:kind pullreq :mine t :approvals 1 :draft nil
                   :merge-conflict nil :activity-age 1)))
    (should-not (forge-dashboard-attention-state
                 '(:kind pullreq :mine t :approvals 1
                   :review-states (approved) :draft nil :activity-age 1)))
    (should-not (forge-dashboard-attention-state
                 '(:kind pullreq :mine t :activity-age 8)))))

(ert-deftest forge-dashboard-merge-refuses-unresolved-changes-request ()
  "The merge command must not reach Forge for a blocked pull request."
  (let ((topic (forge-pullreq :id "pr-id" :repository "repo-id"))
        (forge-dashboard-triage-file ":memory:")
        (merge-calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'forge-dashboard-triage--current-topic)
                   (lambda () topic))
                  ((symbol-function 'forge-dashboard-triage-topic-data)
                   (lambda (&rest _)
                     '(:id "pr-id" :kind pullreq :mine t :approvals 1
                       :review-states (changes-requested approved)
                       :latest-review approved :draft nil
                       :merge-conflict nil :activity-age 1)))
                  ((symbol-function 'forge-merge)
                   (lambda (&rest _) (cl-incf merge-calls))))
          (should-error (forge-dashboard-merge) :type 'user-error)
          (should (zerop merge-calls)))
      (forge-dashboard-triage-close-store))))

(ert-deftest forge-dashboard-triage-raw-assignee-row-uses-login-column ()
  ;; `closql-dref' uses `forge-sql-cdr', which drops the repository column.
  (should (equal (forge-dashboard-triage--login
                  '("assignee-id" "octocat" "Octo Cat" "forge-id"))
                 "octocat")))

(ert-deftest forge-dashboard-triage-recognizes-requested-reviewer ()
  (let ((topic (forge-pullreq
                :id "pr-id" :repository "repo-id" :number 7 :state 'open
                :author "someone" :title "Review me"
                :created "2025-01-01T00:00:00Z"
                :updated "2025-01-20T00:00:00Z" :status 'pending)))
    (oset topic review-requests
          '(("assignee-id" "me" "My Display Name" "forge-id")))
    (cl-letf (((symbol-function 'forge-get-repository)
               (lambda (_topic) (forge-dashboard-test--repository "repo")))
              ((symbol-function 'ghub--username) (lambda (_repo) "me"))
              ((symbol-function 'forge-dashboard--classify)
               (lambda (_repo) 'member)))
      (let ((data (forge-dashboard-triage-topic-data topic)))
        (should (plist-get data :review-requested))
        (should (eq (forge-dashboard-attention-state data)
                    'review-requested))))))

(ert-deftest forge-dashboard-renders-review-request-from-forge-database ()
  "Read a real Forge relation through triage and render its dashboard row."
  (let* ((directory (make-temp-file "forge-dashboard-test-" t))
         (forge-database-file (expand-file-name "forge.sqlite" directory))
         (forge-dashboard-triage-file ":memory:")
         (repo (forge-github-repository
                :id "repo-id" :owner "other" :name "project"
                :apihost "api.github.com" :githost "github.com"))
         (topic (forge-pullreq
                 :id "pr-id" :repository "repo-id" :number 7 :state 'open
                 :author "someone" :title "Review me"
                 :created "2025-01-01T00:00:00Z"
                 :updated "2025-01-20T00:00:00Z" :status 'pending
                 :draft-p nil))
         (classifications 0)
         (classify (symbol-function 'forge-dashboard--classify-uncached)))
    (unwind-protect
        (cl-letf (((symbol-function 'forge-db)
                   (lambda (&optional livep)
                     (closql-db 'forge-dashboard-test-database livep)))
                  ((symbol-function 'ghub--username) (lambda (_repo) "me"))
                  ((symbol-function 'forge-dashboard--classify-uncached)
                   (lambda (repository)
                     (cl-incf classifications)
                     (funcall classify repository))))
          (oset repo condition :tracked)
          (closql-insert (forge-db) repo)
          (closql-insert (forge-db) topic)
          (forge-sql [:insert-into assignee :values $v1]
                     (vector "repo-id" "user-id" "me" "My Name" "host-id"))
          (forge-sql [:insert-into pullreq-review-request :values $v1]
                     (vector "pr-id" "user-id"))
          (should (equal (forge-dashboard--latest-update)
                         "2025-01-20T00:00:00Z"))
          (let* ((stored-repo (car (forge-dashboard--tracked-repositories)))
                 (stored-topic (car (plist-get
                                     (forge-dashboard--repo-data stored-repo)
                                     :topics))))
            (should (equal (oref stored-topic review-requests)
                           '(("user-id" "me" "My Name" "host-id")))))
          (with-temp-buffer
            (forge-dashboard-mode)
            (setq forge-dashboard--snapshot (forge-dashboard--collect-snapshot))
            (let ((inhibit-read-only t)
                  (forge-dashboard--render-only t))
              (magit-insert-section (forge-dashboard-test-root)
                (forge-dashboard-refresh-buffer)))
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "review requested" text))
              (should (string-match-p "Review me" text))
              (should-not (string-match-p "Nothing needs attention" text))))
          ;; The topic's repository was reconstructed separately by Forge.
          (should (= classifications 1))
          (should (eq (plist-get
                       (car (forge-dashboard--attention-items
                             (forge-dashboard--dashboard-repositories)))
                       :state)
                      'review-requested))
          (forge-dashboard-triage-snooze
           "pr-id" (time-add (current-time) (days-to-time 1)))
          (with-temp-buffer
            (forge-dashboard-mode)
            (setq forge-dashboard--snapshot (forge-dashboard--collect-snapshot))
            (let ((inhibit-read-only t)
                  (forge-dashboard--render-only t))
              (magit-insert-section (forge-dashboard-test-root)
                (forge-dashboard-refresh-buffer)))
            (should (string-match-p
                     "Nothing needs attention"
                     (buffer-substring-no-properties (point-min) (point-max))))))
      (forge-dashboard-triage-close-store)
      (when-let* ((db (closql-db 'forge-dashboard-test-database t)))
        (emacsql-close db))
      (delete-directory directory t))))

(ert-deftest forge-dashboard-triage-degrades-with-unbound-reviews ()
  (let ((topic (forge-pullreq
                :id "pr-id" :repository "repo-id" :number 7 :state 'open
                :author "me" :title "Needs review" :created "2025-01-01T00:00:00Z"
                :updated "2025-01-20T00:00:00Z" :status 'pending)))
    (cl-letf (((symbol-function 'forge-get-repository)
               (lambda (_topic) (forge-dashboard-test--repository "repo")))
              ((symbol-function 'ghub--username) (lambda (_repo) "me"))
              ((symbol-function 'forge-dashboard--classify)
               (lambda (_repo) 'member)))
      (let ((data (forge-dashboard-triage-topic-data
                   topic (date-to-time "2025-01-21T00:00:00Z"))))
        (should-not (plist-get data :review-age))
        (should-not (plist-get data :approvals))
        (should-not (plist-get data :review-requested))
        (should-not (plist-get data :reviewed-by-me))))))

(ert-deftest forge-dashboard-review-timestamp-is-not-topic-age ()
  (let ((reviews '(((state . changes-requested)
                    (updated . "2025-01-01T00:00:00Z"))
                   ((state . approved)
                    (updated . "2025-01-19T00:00:00Z"))))
        (now (date-to-time "2025-01-20T00:00:00Z")))
    (should (= (forge-dashboard--age-days
                (forge-dashboard-triage--latest-review-updated reviews)
                now)
               1))
    (should-not (forge-dashboard-attention-state
                 '(:kind pullreq :mine t :review-age 1 :activity-age 8)))))

(ert-deftest forge-dashboard-urgency-orders-state-then-recency ()
  (let* ((stale '(:state stale :age 100))
         (blocked-recent '(:state review-requested :age 1))
         (blocked-old '(:state changes-requested :age 10))
         (ready '(:state ready-to-merge :age 0)))
    (should (equal (forge-dashboard-sort-attention
                    (list stale blocked-recent ready blocked-old))
                   (list ready blocked-recent blocked-old stale)))))

(ert-deftest forge-dashboard-snooze-store-round-trip ()
  (let ((forge-dashboard-triage-file ":memory:")
        (now (date-to-time "2025-01-01T00:00:00Z")))
    (unwind-protect
        (progn
          (forge-dashboard-triage-open-store ":memory:")
          (forge-dashboard-triage-snooze
           "topic" (time-add now (days-to-time 1)))
          (should (forge-dashboard-triage-snoozed-p "topic" now))
          (should-not (forge-dashboard-triage-snoozed-p
                       "topic" (time-add now (days-to-time 2))))
          (forge-dashboard-triage-done "topic" "2025-01-02T00:00:00Z")
          (should (forge-dashboard-triage-done-p
                   "topic" "2025-01-02T00:00:00Z"))
          (should-not (forge-dashboard-triage-done-p
                       "topic" "2025-01-03T00:00:00Z")))
      (forge-dashboard-triage-close-store))))

(ert-deftest forge-dashboard-pull-all-uses-every-tracked-repository ()
  (let ((repos (list (forge-dashboard-test--repository "first")
                     (forge-dashboard-test--repository "second")))
        pulled buffer)
    (with-temp-buffer
      (cl-letf (((symbol-function 'forge-dashboard--tracked-repositories)
                 (lambda () repos))
                ((symbol-function 'forge-dashboard--pull-repositories)
                 (lambda (selected target)
                   (setq pulled selected buffer target))))
        (forge-dashboard-pull-all)
        (should (eq buffer (current-buffer)))))
    (should (equal pulled repos))))

(ert-deftest forge-dashboard-pulls-sequentially-before-refresh ()
  (let* ((first (forge-dashboard-test--repository "first"))
         (second (forge-dashboard-test--repository "second"))
         calls callbacks
         (refreshes 0))
    (cl-letf (((symbol-function 'forge-dashboard--dashboard-repositories)
               (lambda () (list first second)))
              ((symbol-function 'forge--pull)
               (lambda (repo callback &rest _)
                 (setq calls (append calls (list repo)))
                 (setq callbacks (append callbacks (list callback)))))
              ((symbol-function 'magit-refresh)
               (lambda () (cl-incf refreshes))))
      (forge-dashboard-pull)
      (should (equal calls (list first)))
      (should (zerop refreshes))
      (funcall (pop callbacks) first)
      (should (equal calls (list first second)))
      (should (zerop refreshes))
      (funcall (pop callbacks) second)
      (should (= refreshes 1)))))

(ert-deftest forge-dashboard-urgency-weights-are-customizable ()
  (let ((forge-dashboard-urgency-weights '((stale . 5) (ready-to-merge . 1))))
    (should (> (forge-dashboard-urgency-score 'stale 100)
               (forge-dashboard-urgency-score 'ready-to-merge 0)))
    (should (= (forge-dashboard-urgency-score 'unknown-state 3) -3))))

(ert-deftest forge-dashboard-triage-pages-filter-by-group ()
  (let ((forge-dashboard-attention-groups
         '(("On me" changes-requested) ("Rest" stale)))
        (items (list '(:state ready-to-merge :age 1)
                     '(:state changes-requested :age 1)
                     '(:state stale :age 1))))
    (should (equal (forge-dashboard-triage-page-names)
                   '("All" "Ready to merge" "On me" "Rest")))
    (should (= (length (forge-dashboard-triage-page-items items "All")) 3))
    (should (equal (forge-dashboard-triage-page-items items nil) items))
    (should (equal (forge-dashboard-triage-page-items items "Ready to merge")
                   '((:state ready-to-merge :age 1))))
    (should (equal (forge-dashboard-triage-page-items items "On me")
                   '((:state changes-requested :age 1))))
    (should-not (forge-dashboard-triage-page-items items "Nope"))))

(ert-deftest forge-dashboard-triage-start-selects-page ()
  (let ((items (list '(:state ready-to-merge :age 1)
                     '(:state stale :age 2))))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (forge-dashboard-triage-start items nil "Ready to merge")
      (unwind-protect
          (with-current-buffer "*Forge Dashboard Triage*"
            (should (equal forge-dashboard-triage--page "Ready to merge"))
            (should (= (length forge-dashboard-triage--all-items) 2))
            (should (= (length forge-dashboard-triage--items) 1))
            (should (eq (plist-get (car forge-dashboard-triage--items)
                                   :state)
                        'ready-to-merge))
            (forge-dashboard-triage-next-page)
            (should (equal forge-dashboard-triage--page "On me")))
        (kill-buffer "*Forge Dashboard Triage*")))))

(ert-deftest forge-dashboard-attention-groups-are-customizable ()
  (let* ((forge-dashboard-attention-groups '(("Mine" changes-requested)))
         (topic (forge-dashboard-test--issue))
         (items (list (list :topic topic
                            :data (list :repo "owner/repo" :approvals 0
                                        :ci nil)
                            :state 'changes-requested :age 2))))
    (with-temp-buffer
      (forge-dashboard-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (forge-dashboard-test-root)
          (forge-dashboard--insert-attention items)))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Mine" text))
        (should-not (string-match-p "Nudge" text))))))

(ert-deftest forge-dashboard-snapshot-round-trip-detaches-connections ()
  (let ((topic (forge-dashboard-test--issue)))
    (closql--oset topic 'closql-database "unprintable-live-connection")
    (let* ((printed (prin1-to-string (forge-dashboard--encode topic)))
           (restored (forge-dashboard--decode (read printed) "parent-connection")))
      (should-not (string-match-p "unprintable-live-connection" printed))
      (should (forge-issue-p restored))
      (should (equal (oref restored title) "Triage this"))
      (should (equal (oref restored closql-database) "parent-connection"))
      (should (eq (closql--oref restored 'posts) eieio--unbound)))))

(ert-deftest forge-dashboard-opens-cache-without-scanning-and-reuses-buffer ()
  (let* ((file (make-temp-file "forge-dashboard-cache-test-"))
         (forge-dashboard-cache-file file)
         (repo (forge-dashboard-test--repository "cached"))
         (topic (forge-dashboard-test--issue))
         (snapshot
          (forge-dashboard--decode
           (forge-dashboard--encode
            (list :repos (list repo) :classes (list (cons (oref repo id) 'owned))
                  :data (list (list :repo repo :open-topics 1 :topics (list topic)
                                    :open-pullreqs 0 :open-issues 1 :unread 0))
                  :updated "yesterday"))))
         (reads 0) (starts 0))
    (unwind-protect
        (cl-letf (((symbol-function 'forge-dashboard--read-snapshot)
                   (lambda (&rest _) (cl-incf reads) snapshot))
                  ((symbol-function 'forge-dashboard--start-refresh)
                   (lambda () (cl-incf starts)))
                  ((symbol-function 'forge-dashboard-triage--load-state-cache)
                   (lambda () (make-hash-table :test #'equal)))
                  ((symbol-function 'forge-dashboard--repo-data)
                   (lambda (&rest _) (ert-fail "Scanned database on display")))
                  ((symbol-function 'forge-dashboard--tracked-repositories)
                   (lambda () (ert-fail "Scanned repositories on display")))
                  ((symbol-function 'magit-display-buffer) #'ignore))
          (with-current-buffer (forge-dashboard)
            (should (string-match-p "owner/cached" (buffer-string)))
            (goto-char (point-min))
            (search-forward "owner/cached")
            (magit-section-show (magit-current-section))
            (setq forge-dashboard-topic-type 'issue)
            (let ((position (point)) (root magit-root-section))
              (forge-dashboard)
              (should (= position (point)))
              (should (eq root magit-root-section))
              (should (eq forge-dashboard-topic-type 'issue))
              ;; A background result redraw preserves expanded sections and point.
              (let ((forge-dashboard--render-only t)) (magit-refresh-buffer))
              (should (= position (point)))
              (should-not (oref (magit-current-section) hidden))))
          (should (= reads 1))
          (should (= starts 2)))
      (when (get-buffer "*forge-dashboard*") (kill-buffer "*forge-dashboard*"))
      (delete-file file))))

(ert-deftest forge-dashboard-refresh-worker-round-trip ()
  "Exercise an actual subprocess, its snapshot, and the parent sentinel."
  (let* ((directory (make-temp-file "forge-dashboard-worker-test-" t))
         (forge-database-file (expand-file-name "forge.sqlite" directory))
         (forge-dashboard-triage-file (expand-file-name "triage.sqlite" directory))
         (forge-dashboard-cache-file (expand-file-name "cache.el" directory))
         (forge-owned-accounts '(("owner")))
         (repo (forge-github-repository
                :id "repo-id" :owner "owner" :name "project"
                :apihost "api.github.com" :githost "github.com"))
         (topic (forge-dashboard-test--issue)))
    (unwind-protect
        (progn
          ;; Section navigation in an earlier test may have opened Forge's db.
          (when-let* ((db (forge-db t))) (emacsql-close db))
          (should (equal (oref (oref (forge-db) connection) file)
                         forge-database-file))
          (oset repo condition :tracked)
          (closql-insert (forge-db) repo)
          (closql-insert (forge-db) topic)
          (with-temp-buffer
            (forge-dashboard-mode)
            (cl-letf (((symbol-function 'forge-dashboard--tracked-repositories)
                       (lambda () (ert-fail "Parent scanned database")))
                      ((symbol-function 'forge-dashboard--repo-data)
                       (lambda (&rest _) (ert-fail "Parent queried topics"))))
              (forge-dashboard--start-refresh)
              (let ((process forge-dashboard--refresh-process)
                    (deadline (+ (float-time) 15)))
                (forge-dashboard--start-refresh)
                (should (eq process forge-dashboard--refresh-process))
                (while (and (process-live-p process) (< (float-time) deadline))
                  (accept-process-output process 0.1))
                (should-not (process-live-p process))
                (should-not forge-dashboard--refresh-process)
                (should forge-dashboard--snapshot)
                (should (file-exists-p forge-dashboard-cache-file))
                (should (= (file-modes forge-dashboard-cache-file) #o600))
                (should (string-match-p "owner/project" (buffer-string)))
                (let* ((data (car (plist-get forge-dashboard--snapshot :data)))
                       (restored (car (plist-get data :topics))))
                  (should (forge-issue-p restored))
                  (should (equal (oref restored id) (oref topic id)))
                  (should (eq (oref restored closql-database)
                              (oref (forge-db) connection))))
                (should-not (forge-dashboard--read-snapshot
                             forge-dashboard-cache-file '((different settings))))
                ;; Cancelling a replacement scan leaves the last result intact.
                (let ((snapshot forge-dashboard--snapshot))
                  (forge-dashboard--start-refresh)
                  (forge-dashboard--cancel-refresh)
                  (should-not forge-dashboard--refresh-process)
                  (should (eq snapshot forge-dashboard--snapshot))
                  ;; A failed worker must not replace the result or disk cache.
                  (let* ((false (executable-find "false"))
                         (invocation-directory (file-name-directory false))
                         (invocation-name (file-name-nondirectory false))
                         (deadline (+ (float-time) 5)))
                    (forge-dashboard--start-refresh)
                    (let ((failed forge-dashboard--refresh-process))
                      (while (and (process-live-p failed)
                                  (< (float-time) deadline))
                        (accept-process-output failed 0.1))
                      (should-not (process-live-p failed)))
                    (should-not forge-dashboard--refresh-process)
                    (should (eq snapshot forge-dashboard--snapshot))
                    (should (forge-dashboard--read-snapshot
                             forge-dashboard-cache-file
                             (forge-dashboard--worker-settings)))))))))
      (forge-dashboard-triage-close-store)
      (when-let* ((db (forge-db t))) (emacsql-close db))
      (delete-directory directory t))))

(provide 'forge-dashboard-test)
;;; forge-dashboard-test.el ends here
