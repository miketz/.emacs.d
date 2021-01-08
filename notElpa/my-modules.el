;;; my-modules.el --- Package handling -*- lexical-binding: t -*-

;;; Commentary:
;;; Tracking elisp packages.  Mostly as git submodules, but other
;;; ways too if the package is not a git repo.
;;; Avoiding the pacakge manager package.el and elpa/melpa/etc.
;;;
;;; This file is more about documenting info about pacakges than handling the
;;; packages. Using git features to manually handle packages. This file is just
;;; info that may be useful. Maybe a few automated things will be added like
;;; byte compiling elisp files.

;;; Code:
(require 'cl-lib)
(require 's)

(defvar my-module-folder "~/.emacs.d/notElpa/")

(cl-defstruct module
  "Structure to hold useful info about an elisp module (ie package).
Some info may be acted on by an automated process.
Some info may be purely for informational/doc purposes."
  (name nil)
  (comment nil)
  (folder nil)
  (remote-mine-url nil)
  (remote-mine-alias nil) ; assume "origin" if empty string
  (remote-upstream-url nil)
  (remote-upstream-alias nil) ; assume "upstream" if emtpy string
  (source-control nil) ; git svn
  (submodule-p nil)
  (use-branch nil) ; master, mine
  (depend-hard '())
  (depend-soft '()))

(defvar my-modules
  `(,(make-module
      :name 'paredit
      :folder (concat my-module-folder "paredit")
      :remote-mine-url "https://github.com/miketz/paredit"
      :remote-upstream-url "https://mumble.net/~campbell/git/paredit.git"
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'swiper
      :folder (concat my-module-folder "swiper")
      :remote-mine-url "https://github.com/miketz/swiper"
      :remote-upstream-url "https://github.com/abo-abo/swiper"
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'lispy
      :folder (concat my-module-folder "lispy")
      :remote-mine-url "https://github.com/miketz/lispy"
      :remote-upstream-url "https://github.com/abo-abo/lispy"
      :source-control 'git
      :submodule-p t
      :depend-hard '((emacs "24.3")
                     (ace-window "0.9.0")
                     (iedit "0.9.9")
                     (counsel "0.11.0") ; swiper package
                     (hydra "0.14.0")
                     (zoutline "0.1.0"))
      :depend-soft '())
    ,(make-module
      :name 'evil
      :folder (concat my-module-folder "evil")
      :remote-mine-url "https://github.com/miketz/evil"
      :remote-upstream-url "https://github.com/emacs-evil/evil"
      :source-control 'git
      :submodule-p t
      :depend-hard '((emacs "24.1")
                     (undo-tree "0.7.4")
                     (goto-chg "1.6")
                     (cl-lib "0.5"))
      :depend-soft '())
    ,(make-module
      :name 'evil-leader
      :folder (concat my-module-folder "evil-leader")
      :remote-mine-url "https://github.com/miketz/evil-leader"
      :remote-upstream-url "https://github.com/cofi/evil-leader"
      :source-control 'git
      :submodule-p t
      :depend-hard '((evil "0"))
      :depend-soft '())
    ,(make-module
      :name 'expand-region
      :folder (concat my-module-folder "expand-region.el")
      :remote-mine-url "https://github.com/miketz/expand-region.el"
      :remote-upstream-url "https://github.com/magnars/expand-region.el"
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 's
      :folder (concat my-module-folder "s.el")
      :remote-mine-url "https://github.com/miketz/s.el"
      :remote-upstream-url "https://github.com/magnars/s.el"
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'magit
      :folder (concat my-module-folder "magit")
      :remote-mine-url "https://github.com/miketz/magit"
      :remote-upstream-url "https://github.com/magit/magit"
      :source-control 'git
      :submodule-p t
      :depend-hard '((emacs "25.1")
                     (async "20180527")
                     (dash "20180910")
                     ;; git-commit is a file within magit! not really a
                     ;; dependency, but it is a separate melp package so
                     ;; keep it here for doc purposes.
                     (git-commit "20181104")
                     (transient "20190812")
                     (with-editor "20181103"))
      :depend-soft '((libgit "???")))
    ,(make-module
      :name 'libgit
      :folder (concat my-module-folder "libegit2")
      :remote-mine-url "https://github.com/miketz/libegit2"
      :remote-upstream-url "https://github.com/magit/libegit2"
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'csharp-mode
      :folder (concat my-module-folder "csharp-mode")
      :remote-mine-url "https://github.com/miketz/csharp-mode"
      :remote-upstream-url "https://github.com/josteink/csharp-mode"
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'spinner
      :folder (concat my-module-folder "spinner.el")
      :remote-mine-url "https://github.com/miketz/spinner.el"
      :remote-upstream-url "https://github.com/Malabarba/spinner.el"
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'ggtags
      :folder (concat my-module-folder "ggtags")
      :remote-mine-url "https://github.com/miketz/ggtags"
      :remote-upstream-url "https://github.com/leoliu/ggtags"
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'mode-on-region
      :folder (concat my-module-folder "mine/mor")
      :remote-mine-url "https://github.com/miketz/mor"
      :remote-upstream-url nil ; my own project, so no 3rd party upstream
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'iedit
      :folder (concat my-module-folder "iedit")
      :remote-mine-url "https://github.com/miketz/iedit"
      :remote-upstream-url "https://github.com/victorhge/iedit"
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'ido-grid
      :folder (concat my-module-folder "ido-grid.el")
      :remote-mine-url "https://github.com/miketz/ido-grid.el"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/larkery/ido-grid.el"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'ov
      :folder (concat my-module-folder "ov")
      :remote-mine-url "https://github.com/miketz/ov"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/emacsorphanage/ov"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'sallet
      :folder (concat my-module-folder "sallet")
      :remote-mine-url nil ; TODO: use my own fork
      :remote-mine-alias nil
      :remote-upstream-url "https://github.com/Fuco1/sallet"
      :remote-upstream-alias "origin"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((dash) (s) (async) (flx) (ov) (f) (shut-up))
      :depend-soft '())
    ,(make-module
      :name 'eros
      :comment "Package to display eval'd results in an overlay. Implementation
scraped out of cider. I was able to make eros work for common lisp evals."
      :folder (concat my-module-folder "eros")
      :remote-mine-url "https://github.com/miketz/eros"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/xiongtx/eros"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'rg
      :folder (concat my-module-folder "rg.el")
      :remote-mine-url "https://github.com/miketz/rg.el"
      :remote-upstream-url "https://github.com/dajva/rg.el"
      :source-control 'git
      :submodule-p t
      :depend-hard '((emacs "25.1")
                     (transient "0.1.0")
                     (wgrep "2.1.10"))
      :depend-soft '())))

(defun my-byte-compile-all-modules ()
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

(defun my-byte-compile-module ()
  "Byte compile .el files for the selected module."
  (interactive)
  (let* ((mod-name (intern (completing-read "module: "
                                            (mapcar (lambda (m)
                                                      (module-name m))
                                                    my-modules))))
         (mod (car (cl-member mod-name
                              my-modules
                              :test (lambda (sym mod)
                                      (eq sym (module-name mod)))))))
    (byte-recompile-directory
     (module-folder mod)
     0 ;; 0 means compile .el files if .elc is missing.
     t)))


(defun my-folder-p (f)
  (cl-second f))

(defun my-byte-compile-all-notElpa-folders ()
  "Byte compile .el files in every folder under /notElpa."
  (interactive)
  (let* ((dir-infos (cl-remove-if
                     (lambda (f)
                       (or (not (my-folder-p f)) ;; skip indiviudal files
                           ;; skip themes
                           (s-ends-with-p "themes" (cl-first f))
                           ;; Skip specific projects that don't ignore .elc files.
                           ;; Revist this after I fork the projects, and use a personal branch.
                           (s-ends-with-p "sunrise-commander" (cl-first f))
                           (s-ends-with-p "FlamesOfFreedom" (cl-first f))
                           (s-ends-with-p "markup-faces" (cl-first f))
                           (s-ends-with-p "sicp-info" (cl-first f))
                           (s-ends-with-p "sallet" (cl-first f))))
                     (directory-files-and-attributes my-module-folder
                                                     t "^[^.]" t)))
         (dir-names (mapcar #'cl-first dir-infos)))
    (cl-loop for dir in dir-names
             do
             (byte-recompile-directory
              dir
              0 ;; 0 means compile .el files if .elc is missing.
              t) ;; t means force re-compile even if the .elc is up-to-date. May
             ;; be useful if the Emacs version changed and should have an
             ;; .elc compiled again to be compatible.
             )))

(defun my--scrape-module-info ()
  "This is is for dev-time use only.
Generates a skeleton list for `my-modules'. With possilbly incorrect and
imcomplete info about the modules.
Saves me from typing a lot of module stuff."
  (let* ((dir-infos (cl-remove-if
                     (lambda (f)
                       (or (not (my-folder-p f)) ;; skip indiviudal files
                           ;; skip themes
                           (s-ends-with-p "themes" (cl-first f))))
                     (directory-files-and-attributes my-module-folder
                                                     nil "^[^.]" nil)))
         (dir-names (mapcar #'cl-first dir-infos)))
    (cl-loop for dir in dir-names
             do
             (insert (format ",(make-module
  :name '%s
  :folder (concat my-module-folder \"%s\")
  :remote-mine-url \"https://github.com/miketz/%s\"
  :remote-upstream-url nil
  :source-control 'git
  :submodule-p t
  :depend-hard '()
  :depend-soft '())\n"
                             dir dir dir)))))

(provide 'my-modules)

;;; my-modules.el ends here
