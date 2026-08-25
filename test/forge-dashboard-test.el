;;; forge-dashboard-test.el --- Tests for forge-dashboard  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'forge-dashboard)
(require 'forge-issue)
(require 'forge-pullreq)

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

(ert-deftest forge-dashboard-topic-row-recognizes-pull-request ()
  (let ((topic (forge-pullreq
                :id "pr-id" :repository "repo-id" :number 7 :state 'open
                :author "hubot" :title "Ship it" :created "2025-01-01T00:00:00Z"
                :updated "2025-01-02T00:00:00Z" :status 'pending)))
    (should (equal (plist-get (forge-dashboard--topic-row topic) :type) "PR"))))

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

(ert-deftest forge-dashboard-owned-account-shape-matches-forge ()
  (let ((forge-owned-accounts '(("mine" . (:remote-name "fork"))
                                 ("also-mine" . nil))))
    (should (forge-dashboard--owned-owner-p "mine"))
    (should (forge-dashboard--owned-owner-p "also-mine"))
    (should-not (forge-dashboard--owned-owner-p "someone-else"))))

(provide 'forge-dashboard-test)
;;; forge-dashboard-test.el ends here
