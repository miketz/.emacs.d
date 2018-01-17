;;; my-grep.el --- grep helper functions -*- lexical-binding: t -*-

(require 'vc-git)
(require 'vc-hooks) ;; for `vc-backend'
(require 'grep)
(require 'cl-lib) ;; for `cl-return-from'

;; ways to detect if in a git repo
;; (eq 'Git (vc-backend "~/.emacs.d/init.el"))
;; (my-is-in-gitrepo)

;; TODO: eliminate duplicate definition of this fn.
(defun my-is-in-gitrepo ()
  "Returns t if the current directory is in a git repo."
  (interactive)
  (string= "true\n" ;there seems to be a newline char so include it
           (shell-command-to-string "git rev-parse --is-inside-work-tree")))


;; git grep command sample:
;; git --no-pager grep --color --extended-regexp --no-index --ignore-case -n -e "foo" -- *.el

(cl-defun my-grep-dwim (regexp &optional files dir search-all-p)
  (interactive
   (progn
     (grep-compute-defaults)
     (let* ((regexp (grep-read-regexp))
            (files (grep-read-files regexp))
            (dir (read-directory-name "In directory: "
                                      nil default-directory t))
            (search-all-p (if current-prefix-arg
                              (string-equal "y"
                                            (completing-read "Search all? "
                                                             '("y" "n")
                                                             nil t "y"))
                            nil)))
       ;; list values plug into the function args.
       (list regexp files dir search-all-p))))

  ;; validate
  (when (not (and (stringp regexp)
                  (> (length regexp) 0)))
    (cl-return-from my-grep-dwim))

  (let ((command regexp))
    (if (null files)
        ;; validate
        (when (string= command "git grep")
          (cl-return-from my-grep-dwim))
      ;; else
      (setq dir (file-name-as-directory (expand-file-name dir)))
      (print dir)
      (print (format "here is %s" (symbol-name (vc-backend dir))))
      (let ((gitrepo-p (my-is-in-gitrepo)))
        (setq command
              (grep-expand-template
               ;; "git --no-pager grep --color --no-index -Ein -e <R> -- <F>"
               (concat
                "git --no-pager grep --color "
                (when (not gitrepo-p) "--no-index ")
                (cond
                 ;;NOTE: git grep options for using .gitignore (or not) require different
                 ;;combos of options (or no option at all if default) depending on whether you're in a git repo or not.
                 ((and gitrepo-p search-all-p) "--untracked --no-exclude-standard ")
                 ;;--exclude-standard so it honors the .gitignore file when not in a git repo.
                 ((and (not gitrepo-p) (not search-all-p)) "--exclude-standard "))
                " -Ein -e <R> -- <F>")
               regexp files)))
      (when command
        (setq command
              (read-from-minibuffer "Confirm: "
                                    command nil nil 'grep-history))))
    (when command
      (let ((default-directory dir)
            (compilation-environment (cons "PAGER=" compilation-environment)))
        ;; Setting process-setup-function makes exit-message-function work
        ;; even when async processes aren't supported.
        (compilation-start command 'grep-mode))
      (if (eq next-error-last-buffer (current-buffer))
          (setq default-directory dir)))))


(provide 'my-grep)

;;; my-grep.el ends here
