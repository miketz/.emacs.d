;;; my-find-file.el --- find file by name in dir -*- lexical-binding: t -*-

(require 'ivy)
(require 'counsel)
(require 'my-select-folder)

(defvar my-fd-installed-p (executable-find "fd"))

;; MS windows specific command. Doesn't seem to work well.
(defun my-find-file-by-name-windows ()
    "Find files by name starting in current directory."
    (interactive)
    (let ((compile-command "dir /b/s "))
      ;; #'shell-command
      ;; #'grep
      (call-interactively #'compile)))

;; (require 'my-grep) ; for `my-is-in-gitrepo'
(defvar completing-read-function)
(defvar ivy-re-builders-alist)
(defvar ivy-height)
;;;###autoload
(defun my-find-file-omni ()
  "Find files by name.
First try `counsel-git', powered by git ls-files.
Fall back to `counsel-fd-file-jump' if fd is installed.
Fall back to `my-find-file-by-name-windows' if on MS-Windows.
Fall back to `counsel-file-jump' if on GNU/linux."
  (interactive)
  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window
        (ivy-height (- (window-height) 4)))
    ;; Try `counsel-git' first. If it errors out due to not being in a git
    ;; repo, then fall back to other commands. Erroring out seems to be faster
    ;; than checking for presence of a git repo first on ms-win.
    (unless (ignore-errors (counsel-git))
      (let ((fn (cond
                 (my-fd-installed-p #'counsel-fd-file-jump)
                 ((eq system-type 'windows-nt) #'my-find-file-by-name-windows)
                 (t #'counsel-file-jump
                  ;; (lambda ()
                  ;;   (interactive)
                  ;;   (call-interactively #'find-name-dired))
                  ))))
        (funcall fn)))))


;;;###autoload
(defun my-find-file-fd ()
  "Find files by name. Using fd and emacs wrapper fn `counsel-fd-file-jump'."
  (interactive)
  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window
        (ivy-height (- (window-height) 4)))
    (counsel-fd-file-jump nil (my-select-folder2))))


(provide 'my-find-file)

;;; my-find-file.el ends here