;;; my-find-replace.el --- find/replace functionality -*- lexical-binding: t -*-

(require 'projectile)

;;;###autoload
(defun my-find-replace ()
  "Find/replace in selected dir and sub-dirs.
Calls `projectile-replace-regexp' with a prefix arg to force manual folder
selection."
  (interactive)
  ;; set `current-prefix-arg' to trigger manual folder selection.
  ;; ie don't assume project root.
  (let ((current-prefix-arg '(4)))
    (call-interactively #'projectile-replace-regexp)))


;;; my-find-replace.el ends here
