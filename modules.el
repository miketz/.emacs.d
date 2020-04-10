;;; modules.el --- Package handling -*- lexical-binding: t -*-

;;; Commentary:
;;; Tracking/handling elisp packages.  Mostly as git submodules, but other
;;; ways too if the package is not a git repo.
;;; Avoiding the pacakge manager package.el and elpa/melpa/etc.

;;; Code:
(require 'cl-lib)

(defvar my-module-folder "~/.emacs.d/notElpa/")

(cl-defstruct module
  "Structure to hold useful info about an elisp module (ie package).
Some info may be acted on by an automated process.
Some info may be purely for informational/doc purposes."
  (name nil)
  (folder nil)
  (upstream nil)
  (source-control nil) ; git svn
  (submodule-p nil)
  (depend-hard '())
  (depend-soft '()))

(defvar my-modules
  `(,(make-module
      :name 'paredit
      :folder (concat my-module-folder "paredit")
      :upstream "https://mumble.net/~campbell/git/paredit.git"
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'mode-on-region
      :folder (concat my-module-folder "mine/mor")
      :upstream "https://github.com/miketz/mor"
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())))

(defun my-byte-compile-modules ()
  "Byte compile .el files of all modules."
  (interactive)
  (cl-loop for mod in my-modules
           do
           (byte-recompile-directory
            (module-folder mod)
            0  ;; 0 means compile .el files if .elc is missing.
            t) ;; t means force re-compile even if the .elc is up-to-date. May
               ;; be useful if the Emacs version changed and should have an
               ;; .elc compiled again to be compatible.
           ))

(provide 'my-modules)

;;; modules.el ends here
