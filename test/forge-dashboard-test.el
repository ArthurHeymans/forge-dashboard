;;; forge-dashboard-test.el --- Tests for forge-dashboard  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'forge-dashboard)
(require 'forge-discussion)
(require 'forge-issue)
(require 'forge-pullreq)

(defclass forge-dashboard-test-repository ()
  ((owner :initarg :owner)
   (name :initarg :name)
   (selective-p :initarg :selective-p :initform nil)))

(defun forge-dashboard-test--repository (name &optional selective)
  "Make a synthetic repository named NAME with SELECTIVE pull behavior."
  (forge-dashboard-test-repository
   :owner "owner" :name name :selective-p selective))

(defun forge-dashboard-test--issue (&rest slots)
  "Make a synthetic issue initialized with SLOTS."
  (apply #'forge-issue
         :id "issue-id" :repository "repo-id" :number 12 :state 'open
         :author "octocat" :title "Triage this" :created "2025-01-01T00:00:00Z"
         :updated "2025-01-10T00:00:00Z" :status 'done
         slots))

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

(ert-deftest forge-dashboard-unread-face-is-red-and-bold ()
  (should (eq (face-attribute 'forge-dashboard-unread :inherit nil t)
              'forge-topic-slug-unread))
  (should (equal (face-attribute 'forge-dashboard-unread :foreground nil t)
                 "red")))

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

(ert-deftest forge-dashboard-owned-account-shape-matches-forge ()
  (let ((forge-owned-accounts '(("mine" . (:remote-name "fork"))
                                 ("also-mine" . nil))))
    (should (forge-dashboard--owned-owner-p "mine"))
    (should (forge-dashboard--owned-owner-p "also-mine"))
    (should-not (forge-dashboard--owned-owner-p "someone-else"))))

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
      (funcall (pop callbacks))
      (should (equal calls (list first second)))
      (should (zerop refreshes))
      (funcall (pop callbacks))
      (should (= refreshes 1)))))

(provide 'forge-dashboard-test)
;;; forge-dashboard-test.el ends here
