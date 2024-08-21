;;; hoagie-vc.el --- vc helper funcs -*- lexical-binding: t -*-

;;; Commentary:
;;; from a blog and reddit post:
;;; https://site.sebasmonia.com/posts/2024-08-15-emacs-vc-mode-tutorial.html
;;; https://old.reddit.com/r/emacs/comments/1ew4467/i_wrote_a_short_vcmode_tutorial/

(defvar hoagie-vc-git-emails
  '("miketz@users.noreply.github.com")
  "List of email addresses that can be associated with a repository")

(defun hoagie-vc-git-clone (repository-url local-dir)
  "Run \"git clone REPOSITORY-URL\" to LOCAL-DIR.
It also prompts what email to use in the directory, from the
values in `hoagie-vc-git-emails'.
Executes `vc-dir' in the newly cloned directory."
  (interactive
   (let* ((url (read-string "Repository URL: "))
          (dir (file-name-base url)))
     (list url (read-string "Target directory: " dir))))
  (vc-git-command nil 0 nil "clone" repository-url local-dir)
  (let ((default-directory (file-name-concat default-directory local-dir)))
    (vc-git-command nil 0 nil "config" "user.email"
                    (completing-read "Email for this repo: "
                                     hoagie-vc-git-emails))
    (vc-dir default-directory)))


(defun hoagie-vc-git-show-branches (&optional arg)
    "Show in a buffer the list of branches in the current repository.
With prefix ARG show the remote branches."
    (interactive "P")
    ;; TODO: this is a mix of vc-git stuff and project.el stuff...
    (let* ((default-directory (project-root (project-current t)))
           (buffer-name (project-prefixed-buffer-name (if arg
                                                          "git remote branches"
                                                        "git local branches"))))
      (vc-git-command buffer-name
                      0
                      nil
                      "branch"
                      (when arg "-r"))
      (pop-to-buffer buffer-name)
      (goto-char (point-min))
      (special-mode)))


(defun hoagie-vc-dir-reset (&optional arg)
    "Runs \"git reset\" to unstage all changes.
With prefix arg, does a hard reset (thus it asks for confirmation)."
    (interactive "P")
    (if arg
        (when (y-or-n-p "Perform a hard reset? ")
          (vc-git-command nil 0 nil "reset" "--hard")
          (message "Completed. All pending changes are lost."))
      (vc-git-command nil 0 nil "reset")
      (message "All changes are unstaged."))
    (vc-dir-refresh))


(provide 'hoagie-vc)
