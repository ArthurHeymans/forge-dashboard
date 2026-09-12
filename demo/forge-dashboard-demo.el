;;; forge-dashboard-demo.el --- Synthetic demo data for screenshots  -*- lexical-binding: t; -*-

;; This file builds a self-contained, reproducible demo of
;; `forge-dashboard' from synthetic Forge objects.  It exists so the
;; README screenshot can be regenerated without network access or a
;; real Forge database:
;;
;;   emacs -Q -L . -L ../forge/lisp <deps...> \
;;     -l demo/forge-dashboard-demo.el \
;;     --eval '(forge-dashboard-demo-show)'
;;
;; See README.md ("Regenerating the screenshot") for the full recipe,
;; including the headless Wayland compositor used for capture.

;;; Code:

(require 'cl-lib)
(require 'forge-dashboard)
(require 'forge-github)
(require 'forge-issue)
(require 'forge-pullreq)

(defvar forge-dashboard-demo--repos nil
  "Synthetic repositories shown by the demo.")

(defvar forge-dashboard-demo--repo-data nil
  "Alist mapping synthetic repositories to their dashboard data plists.")

(defvar forge-dashboard-demo--triage-data nil
  "Alist mapping synthetic topic ids to normalized triage data plists.")

(defvar forge-dashboard-demo--triage-file nil
  "Temporary triage store used by the demo.")

(defun forge-dashboard-demo--days-ago (days &optional hours)
  "Return an ISO-8601 timestamp DAYS days and HOURS hours ago."
  (format-time-string
   "%Y-%m-%dT%H:%M:%SZ"
   (time-subtract (current-time)
                  (seconds-to-time (+ (* days 86400) (* (or hours 2) 3600))))
   t))

(defun forge-dashboard-demo--make ()
  "Build synthetic repositories, topics, and triage data.
Timestamps stay relative to the current time so age faces and
labels look the same whenever the screenshot is regenerated."
  (let* ((dashboard (forge-github-repository
                     :id "demo:octocat/forge-dashboard"
                     :owner "octocat" :name "forge-dashboard"))
         (dotfiles (forge-github-repository
                    :id "demo:octocat/dotfiles"
                    :owner "octocat" :name "dotfiles"))
         (webapp (forge-github-repository
                  :id "demo:acme/webapp"
                  :owner "acme" :name "webapp"))
         (pr-48 (forge-pullreq
                 :id "demo:pr-48" :repository "demo:octocat/forge-dashboard"
                 :number 48 :slug "#48" :state 'open :status 'unread
                 :author "octocat" :title "Add dashboard triage queue"
                 :created (forge-dashboard-demo--days-ago 6)
                 :updated (forge-dashboard-demo--days-ago 1)
                 :draft-p nil))
         (pr-47 (forge-pullreq
                 :id "demo:pr-47" :repository "demo:octocat/forge-dashboard"
                 :number 47 :slug "#47" :state 'open :status 'pending
                 :author "octocat" :title "Fix stale age ramp"
                 :created (forge-dashboard-demo--days-ago 9)
                 :updated (forge-dashboard-demo--days-ago 3)
                 :draft-p nil))
         (pr-44 (forge-pullreq
                 :id "demo:pr-44" :repository "demo:octocat/forge-dashboard"
                 :number 44 :slug "#44" :state 'open :status 'done
                 :author "octocat" :title "Nudge: docs build is green again"
                 :created (forge-dashboard-demo--days-ago 12)
                 :updated (forge-dashboard-demo--days-ago 9)
                 :draft-p nil))
         (issue-41 (forge-issue
                    :id "demo:issue-41" :repository "demo:octocat/forge-dashboard"
                    :number 41 :slug "#41" :state 'open :status 'unread
                    :author "octocat" :title "Dashboard hides empty repositories"
                    :created (forge-dashboard-demo--days-ago 5)
                    :updated (forge-dashboard-demo--days-ago 2)))
         (issue-39 (forge-issue
                    :id "demo:issue-39" :repository "demo:octocat/forge-dashboard"
                    :number 39 :slug "#39" :state 'open :status 'done
                    :author "hubot" :title "Consider showing discussions too"
                    :created (forge-dashboard-demo--days-ago 30)
                    :updated (forge-dashboard-demo--days-ago 21)))
         (issue-7 (forge-issue
                   :id "demo:issue-7" :repository "demo:octocat/dotfiles"
                   :number 7 :slug "#7" :state 'open :status 'done
                   :author "octocat" :title "Pin nerd font version"
                   :created (forge-dashboard-demo--days-ago 2)
                   :updated (forge-dashboard-demo--days-ago 0 3)))
         (pr-231 (forge-pullreq
                  :id "demo:pr-231" :repository "demo:acme/webapp"
                  :number 231 :slug "#231" :state 'open :status 'unread
                  :author "hubot" :title "Migrate sidebar to new router"
                  :created (forge-dashboard-demo--days-ago 4)
                  :updated (forge-dashboard-demo--days-ago 2)
                  :draft-p nil))
         (pr-228 (forge-pullreq
                  :id "demo:pr-228" :repository "demo:acme/webapp"
                  :number 228 :slug "#228" :state 'open :status 'done
                  :author "hubot" :title "WIP: streaming SSR prototype"
                  :created (forge-dashboard-demo--days-ago 6)
                  :updated (forge-dashboard-demo--days-ago 4)
                  :draft-p t))
         (issue-199 (forge-issue
                     :id "demo:issue-199" :repository "demo:acme/webapp"
                     :number 199 :slug "#199" :state 'open :status 'done
                     :author "hubot" :title "Flaky login redirect on Safari"
                     :created (forge-dashboard-demo--days-ago 45)
                     :updated (forge-dashboard-demo--days-ago 30))))
    (setq forge-dashboard-demo--repos (list dashboard dotfiles webapp))
    (setq forge-dashboard-demo--repo-data
          (list (cons dashboard
                      (list :repo dashboard
                            :open-topics 5 :open-pullreqs 3 :open-issues 2
                            :unread 2
                            :topics (list pr-48 issue-41 pr-47 pr-44 issue-39)))
                (cons dotfiles
                      (list :repo dotfiles
                            :open-topics 1 :open-pullreqs 0 :open-issues 1
                            :unread 0
                            :topics (list issue-7)))
                (cons webapp
                      (list :repo webapp
                            :open-topics 3 :open-pullreqs 2 :open-issues 1
                            :unread 1
                            :topics (list pr-231 pr-228 issue-199)))))
    (setq forge-dashboard-demo--triage-data
          (list
           (cons "demo:pr-48"
                 (list :id "demo:pr-48" :kind 'pullreq
                       :mine t :owned t :approvals 2
                       :review-states '(approved approved)
                       :latest-review 'approved
                       :draft nil :merge-conflict nil :ci 'success
                       :status 'unread :activity-age 1 :review-age 1
                       :repo "octocat/forge-dashboard"))
           (cons "demo:pr-47"
                 (list :id "demo:pr-47" :kind 'pullreq
                       :mine t :owned t :approvals 1
                       :review-states '(approved changes-requested)
                       :latest-review 'changes-requested
                       :draft nil :merge-conflict nil :ci nil
                       :status 'pending :activity-age 3 :review-age 3
                       :repo "octocat/forge-dashboard"))
           (cons "demo:issue-41"
                 (list :id "demo:issue-41" :kind 'topic
                       :mine t :owned t :status 'unread
                       :last-comment-mine nil
                       :activity-age 2
                       :repo "octocat/forge-dashboard"))
           (cons "demo:pr-231"
                 (list :id "demo:pr-231" :kind 'pullreq
                       :mine nil :owned nil :approvals 0
                       :review-states nil :latest-review nil
                       :draft nil :merge-conflict nil :ci nil
                       :status 'unread
                       :review-requested t :reviewed-by-me nil
                       :activity-age 2 :review-age 2
                       :repo "acme/webapp"))
           (cons "demo:pr-44"
                 (list :id "demo:pr-44" :kind 'pullreq
                       :mine t :owned t :approvals 0
                       :review-states nil :latest-review nil
                       :draft nil :merge-conflict nil :ci nil
                       :status 'done
                       :activity-age 9 :review-age 9
                       :repo "octocat/forge-dashboard"))
           (cons "demo:issue-39"
                 (list :id "demo:issue-39" :kind 'topic
                       :mine nil :owned t :status 'done
                       :activity-age 21
                       :repo "octocat/forge-dashboard"))
           (cons "demo:issue-199"
                 (list :id "demo:issue-199" :kind 'topic
                       :mine nil :owned nil :status 'done
                       :activity-age 30
                       :repo "acme/webapp"))))))

(defun forge-dashboard-demo--tracked-repositories ()
  "Return the synthetic demo repositories."
  forge-dashboard-demo--repos)

(defun forge-dashboard-demo--classify (repo)
  "Classify demo REPO as `owned', `member', or nil."
  (cond ((equal (oref repo owner) "octocat") 'owned)
        ((equal (oref repo owner) "acme") 'member)))

(defun forge-dashboard-demo--repo-data (repo)
  "Return canned dashboard data for demo REPO, honoring the type filter."
  (let ((data (alist-get repo forge-dashboard-demo--repo-data nil nil #'eq)))
    (pcase forge-dashboard-topic-type
      ('pr (plist-put (copy-sequence data)
                      :topics (seq-filter #'forge-pullreq-p
                                          (plist-get data :topics))))
      ('issue (plist-put (copy-sequence data)
                         :topics (seq-filter #'forge-issue-p
                                             (plist-get data :topics))))
      (_ data))))

(defun forge-dashboard-demo--latest-update ()
  "Return a recent timestamp so the header reads \"updated <1d ago\"."
  (forge-dashboard-demo--days-ago 0 2))

(defun forge-dashboard-demo--triage-topic-data (topic &optional _now)
  "Return canned triage data for demo TOPIC, or nil for quiet topics."
  (alist-get (and (eieio-object-p topic)
                  (slot-exists-p topic 'id)
                  (ignore-errors (oref topic id)))
             forge-dashboard-demo--triage-data nil nil #'equal))

(defun forge-dashboard-demo-setup ()
  "Install the synthetic demo backend."
  (interactive)
  (forge-dashboard-demo--make)
  (setq forge-dashboard-demo--triage-file
        (make-temp-file "forge-dashboard-demo-triage" nil ".sqlite"))
  (setq forge-dashboard-triage-file forge-dashboard-demo--triage-file)
  (advice-add 'forge-dashboard--tracked-repositories
              :override #'forge-dashboard-demo--tracked-repositories)
  (advice-add 'forge-dashboard--classify
              :override #'forge-dashboard-demo--classify)
  (advice-add 'forge-dashboard--repo-data
              :override #'forge-dashboard-demo--repo-data)
  (advice-add 'forge-dashboard--latest-update
              :override #'forge-dashboard-demo--latest-update)
  (advice-add 'forge-dashboard-triage-topic-data
              :override #'forge-dashboard-demo--triage-topic-data))

(defun forge-dashboard-demo-teardown ()
  "Remove the synthetic demo backend."
  (interactive)
  (advice-remove 'forge-dashboard--tracked-repositories
                 #'forge-dashboard-demo--tracked-repositories)
  (advice-remove 'forge-dashboard--classify
                 #'forge-dashboard-demo--classify)
  (advice-remove 'forge-dashboard--repo-data
                 #'forge-dashboard-demo--repo-data)
  (advice-remove 'forge-dashboard--latest-update
                 #'forge-dashboard-demo--latest-update)
  (advice-remove 'forge-dashboard-triage-topic-data
                 #'forge-dashboard-demo--triage-topic-data)
  (forge-dashboard-triage-close-store)
  (when (and forge-dashboard-demo--triage-file
             (file-exists-p forge-dashboard-demo--triage-file))
    (delete-file forge-dashboard-demo--triage-file))
  (setq forge-dashboard-demo--triage-file nil))

(defun forge-dashboard-demo--expand-repos (section)
  "Expand every repository SECTION so topics are visible in screenshots."
  (when (eq (oref section type) 'forge-repo)
    (magit-section-show section))
  (dolist (child (oref section children))
    (forge-dashboard-demo--expand-repos child)))

;;;###autoload
(defun forge-dashboard-demo-show ()
  "Render the demo dashboard in a clean frame for screenshots."
  (interactive)
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (set-face-attribute 'default nil :family "CaskaydiaCove NFM" :height 130)
  (load-theme 'modus-vivendi t)
  (setq forge-dashboard-topics-per-repo nil)
  (forge-dashboard-demo-setup)
  (forge-dashboard)
  (switch-to-buffer "*forge-dashboard*")
  (delete-other-windows)
  (forge-dashboard-demo--expand-repos magit-root-section)
  (goto-char (point-min))
  (when (re-search-forward "#48" nil t)
    (beginning-of-line))
  (setq-local cursor-type nil)
  (message ""))

(defun forge-dashboard-demo-print ()
  "Print the demo dashboard rendering for batch verification."
  (forge-dashboard-demo-setup)
  (with-temp-buffer
    (forge-dashboard-mode)
    (let ((inhibit-read-only t))
      (forge-dashboard-refresh-buffer))
    (princ (buffer-substring-no-properties (point-min) (point-max))))
  (forge-dashboard-demo-teardown))

(provide 'forge-dashboard-demo)
;;; forge-dashboard-demo.el ends here
