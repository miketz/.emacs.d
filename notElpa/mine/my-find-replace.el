;;; my-find-replace.el --- find/replace functionality -*- lexical-binding: t -*-

(require 'projectile)
(require 'my-select-folder)

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


;;;###autoload
(defun my-find-replace-folder ()
  "Find/replace in selected dir and sub-dirs.
Like `my-find-replace' but with an elaborate folder selection.
Choose current folder, project root, git-submodule proj root, or custom folder."
  (interactive)
  ;; using the code of `projectile-replace-regexp' but with my custom directory
  ;; selection.
  (let* ((directory (my-select-folder)) ; ### my edit here
         (old-text (read-string
                    (projectile-prepend-project-name "Replace regexp: ")
                    (projectile-symbol-or-selection-at-point)))
         (new-text (read-string
                    (projectile-prepend-project-name
                     (format "Replace regexp %s with: " old-text))))
         (files
          ;; We have to reject directories as a workaround to work with git submodules.
          ;;
          ;; We can't narrow the list of files with
          ;; `projectile-files-with-string' because those regexp tools
          ;; don't support Emacs regular expressions.
          (cl-remove-if
           #'file-directory-p
           (mapcar #'(lambda (file) (expand-file-name file directory))
                   (projectile-dir-files directory)))))
    ;; FIXME: Probably would fail on Emacs 27+, fourth argument is gone.
    (with-no-warnings (tags-query-replace old-text new-text nil (cons 'list files)))))


;;; my-find-replace.el ends here
