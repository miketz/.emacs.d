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
  (folder nil)
  (remote-mine nil)
  (remote-upstream nil)
  (source-control nil) ; git svn
  (submodule-p nil)
  (depend-hard '())
  (depend-soft '()))

(defvar my-modules
  `(,(make-module
      :name 'paredit
      :folder (concat my-module-folder "paredit")
      :remote-mine "https://github.com/miketz/paredit"
      :remote-upstream "https://mumble.net/~campbell/git/paredit.git"
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'swiper
      :folder (concat my-module-folder "swiper")
      :remote-mine "https://github.com/miketz/swiper"
      :remote-upstream "https://github.com/abo-abo/swiper"
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'lispy
      :folder (concat my-module-folder "lispy")
      :remote-mine "https://github.com/miketz/lispy"
      :remote-upstream "https://github.com/abo-abo/lispy"
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
      :remote-mine "https://github.com/miketz/evil"
      :remote-upstream "https://github.com/emacs-evil/evil"
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
      :remote-mine "https://github.com/miketz/evil-leader"
      :remote-upstream "https://github.com/cofi/evil-leader"
      :source-control 'git
      :submodule-p t
      :depend-hard '((evil "0"))
      :depend-soft '())
    ,(make-module
      :name 'expand-region
      :folder (concat my-module-folder "expand-region.el")
      :remote-mine "https://github.com/miketz/expand-region.el"
      :remote-upstream "https://github.com/magnars/expand-region.el"
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 's
      :folder (concat my-module-folder "s.el")
      :remote-mine "https://github.com/miketz/s.el"
      :remote-upstream "https://github.com/magnars/s.el"
      :source-control 'git
      :submodule-p t
      :depend-hard '()
      :depend-soft '())
    ,(make-module
      :name 'magit
      :folder (concat my-module-folder "magit")
      :remote-mine "https://github.com/miketz/magit"
      :remote-upstream "https://github.com/magit/magit"
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
      :depend-soft '())
    ,(make-module
      :name 'mode-on-region
      :folder (concat my-module-folder "mine/mor")
      :remote-mine "https://github.com/miketz/mor"
      :remote-upstream nil ; my own project, so no 3rd party upstream
      :source-control 'git
      :submodule-p t
      :depend-hard '()
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
  :remote-mine \"https://github.com/miketz/%s\"
  :remote-upstream nil
  :source-control 'git
  :submodule-p t
  :depend-hard '()
  :depend-soft '())\n"
                             dir dir dir)))))

(provide 'my-modules)

;;; my-modules.el ends here
