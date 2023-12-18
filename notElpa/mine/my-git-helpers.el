;;; my-git-helpers.el --- helper funcs for git -*- lexical-binding: t -*-

;;; Code:

(defun my-is-in-gitrepo ()
  "Returns t if the current directory is in a git repo."
  (interactive)
  (string= "true\n" ;there seems to be a newline char so include it
           (shell-command-to-string "git rev-parse --is-inside-work-tree")))

(cl-defun my-is-in-git-submodule ()
  "Returns t if the current directory is in a git submodule."
  (interactive)

  ;; GUARD: if curr dir is not a git repo it can't be a git submodule
  (when (not (my-is-in-gitrepo))
    (cl-return-from my-is-in-git-submodule nil))

  (let ((output (shell-command-to-string "git rev-parse --show-superproject-working-tree")))
    (> (length output) 0)))



(provide 'my-git-helpers)

;;; my-git-helpers.el ends here