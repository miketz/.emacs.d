;;; -*- lexical-binding: t -*-

(require 'vc-git)
(require 'grep)
(require 'cl-lib)

(defun my-is-in-gitrepo ()
  "Returns t if the current directory is in a git repo."
  (interactive)
  (string= "true\n" ;there seems to be a newline char so include it
           (shell-command-to-string "git rev-parse --is-inside-work-tree")))

(defun my-git-grep-make-param (pat)
  "Make git-grep work with helm patters of ^, !, $"
  (let ((val (concat " -e \"" pat "\"")))
    (cond
     ((my-str-starts-with-p pat "!")
      (concat " --not -e \"" (substring pat 1 (length pat)) "\""))
     ((my-str-starts-with-p pat "^")
      val);we get helm-style "start with" ^ implemented for free in the default git-grep regex.
                                        ;TODO: make it work when there is leading whitspace on the line.
     ((my-str-ends-with-p pat "$")
      val);TODO: implement helm-style "ends with" $.
     (t val))))

(defun my-git-grep-make-cmd (input)
  ;;git --no-pager grep --color --no-index --ignore-case -n -e "preview" --and -e "print" -- *.cs
  (interactive)
  (let ((patterns (split-string input " "))
        (git-pat ""))
    (setq git-pat (my-git-grep-make-param (cl-first patterns)))
    (dolist (p (cl-rest patterns))
      (setq git-pat (concat git-pat " --and " (my-git-grep-make-param p))))
                                        ;(concat "git --no-pager grep --no-index --ignore-case -n " git-pat)
    (let ((in-gitrepo   (my-is-in-gitrepo))
          (search-all-p current-prefix-arg));if they typed C-u then search all
      (concat "git --no-pager grep --color --extended-regexp "
              (when (not in-gitrepo) "--no-index ")
              (cond
               ;;NOTE: git grep options for using .gitignore (or not) require different
               ;;combos of options (or no option at all if default) depending on whether you're in a git repo or not.
               ((and in-gitrepo search-all-p) "--untracked --no-exclude-standard ")
               ;;--exclude-standard so it honors the .gitignore file when not in a git repo.
               ((and (not in-gitrepo) (not search-all-p)) "--exclude-standard "))
              " --ignore-case -n "
              git-pat))))

;; (defun my-git-grep ()
;;   (interactive)
;;   (let* ((input (read-string "search: "))
;;          (results (shell-command-to-string (my-git-grep-make-cmd input))))
;;     (insert results)))


(defun my-vc-git-grep (regexp &optional files dir)
  "Same as the normal vc-git-grep except I split the search string on spaces and pass
each value as a separate parameter to git grep. Making it work like helm filtering."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
                                   nil nil 'grep-history)
             nil))
      (t (let* ((regexp (grep-read-regexp))
                (files (grep-read-files regexp))
                (dir (read-directory-name "In directory: "
                                          nil default-directory t)))
           (list regexp files dir))))))
  ;; (require 'grep) ;moving to top of file
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (if (null files)
          (if (string= command "git grep")
              (setq command nil))
        (setq dir (file-name-as-directory (expand-file-name dir)))
        (setq command
              (concat (my-git-grep-make-cmd regexp) " -- " files)
              ;; (grep-expand-template "git --no-pager grep -n -e <R> -- <F>"
              ;;                       regexp files)
              )
        (when command
          (if (equal current-prefix-arg '(4))
              (setq command
                    (read-from-minibuffer "Confirm: "
                                          command nil nil 'grep-history))
            (add-to-history 'grep-history command))))
      (when command
        (let ((default-directory dir)
              (compilation-environment (cons "PAGER=" compilation-environment)))
          ;; Setting process-setup-function makes exit-message-function work
          ;; even when async processes aren't supported.
          (compilation-start command 'grep-mode))
        (if (eq next-error-last-buffer (current-buffer))
            (setq default-directory dir))))))
