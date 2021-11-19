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
  ;; Choices: git svn file
  ;; file means I just copied the file(s) into my /.emacs.d/. It technically
  ;; is under git but this distinguishes the special case where it's absorbed
  ;; into my /.emacs.d/ git repo. Maybe too lazy to set up a separate git repo
  ;; or submodule. Or just a little one-off package.
  (source-control nil)
  ;; Choices: t nil
  ;; t if using a submodule within /.emacs.d/
  ;; nil if using a repo outside of /.emacs.d/
  (submodule-p nil)
  (use-branch nil) ; master, mine
  (depend-hard '()) ; requried or important dependencies.
  (depend-soft '()) ; optional dependencies. Or only needed for the tests.
  (depend-bundled '())) ; note when packages bundle dependencies.

(defvar my-modules
  `(,(make-module
      :name 'paredit
      :comment nil
      :folder (concat my-module-folder "paredit")
      :remote-mine-url "https://github.com/miketz/paredit"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://mumble.net/~campbell/git/paredit.git"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'swiper
      :comment nil
      :folder (concat my-module-folder "swiper")
      :remote-mine-url "https://github.com/miketz/swiper"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/abo-abo/swiper"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'lispy
      :comment nil
      :folder (concat my-module-folder "lispy")
      :remote-mine-url "https://github.com/miketz/lispy"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/abo-abo/lispy"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "24.3")
                     (ace-window "0.9.0")
                     (iedit "0.9.9")
                     (counsel "0.11.0") ; swiper package
                     (hydra "0.14.0")
                     (zoutline "0.1.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'evil
      :comment nil
      :folder (concat my-module-folder "evil")
      :remote-mine-url "https://github.com/miketz/evil"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/emacs-evil/evil"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "24.1")
                     (cl-lib "0.5"))
      :depend-soft '()
      :depend-bundled '((;; (undo-tree "0.7.4") ; no longer a dependency
                         goto-chg "1.6")))
    ,(make-module
      :name 'evil-leader
      :comment nil
      :folder (concat my-module-folder "evil-leader")
      :remote-mine-url "https://github.com/miketz/evil-leader"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/cofi/evil-leader"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((evil "0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'expand-region
      :comment nil
      :folder (concat my-module-folder "expand-region.el")
      :remote-mine-url "https://github.com/miketz/expand-region.el"
      :remote-mine-alias "orign"
      :remote-upstream-url "https://github.com/magnars/expand-region.el"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 's
      :comment nil
      :folder (concat my-module-folder "s.el")
      :remote-mine-url "https://github.com/miketz/s.el"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/magnars/s.el"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'dash
      :comment nil
      :folder (concat my-module-folder "dash.el")
      :remote-mine-url "https://github.com/miketz/dash.el"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/magnars/dash.el"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "24"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'transient
      :comment nil
      :folder (concat my-module-folder "transient")
      :remote-mine-url "https://github.com/miketz/transient"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/magit/transient"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "25.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'with-editor
      :comment nil
      :folder (concat my-module-folder "with-editor")
      :remote-mine-url "https://github.com/miketz/with-editor"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/magit/with-editor"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "24.4"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'magit
      :comment nil
      :folder (concat my-module-folder "magit")
      :remote-mine-url "https://github.com/miketz/magit"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/magit/magit"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "25.1")
                     ;; async not used anymore? (async "20180527")
                     (dash "20200524")
                     (transient "20200601")
                     (with-editor "20200522"))
      :depend-soft '((libgit "0")
                     ;; magit-libgit needs emacs 26.1
                     (emacs "26.1"))
      :depend-bundled '((git-commit "20200516")))
    ,(make-module
      :name 'libgit
      :comment "An elisp package with C code bundled."
      :folder (concat my-module-folder "libegit2")
      :remote-mine-url "https://github.com/miketz/libegit2"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/magit/libegit2"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "25.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'csharp-mode
      :comment nil
      :folder (concat my-module-folder "csharp-mode")
      :remote-mine-url "https://github.com/miketz/csharp-mode"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/josteink/csharp-mode"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'spinner
      :comment nil
      :folder (concat my-module-folder "spinner.el")
      :remote-mine-url "https://github.com/miketz/spinner.el"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/Malabarba/spinner.el"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'ggtags
      :comment nil
      :folder (concat my-module-folder "ggtags")
      :remote-mine-url "https://github.com/miketz/ggtags"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/leoliu/ggtags"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'mode-on-region
      :comment nil
      :folder (concat my-module-folder "mine/mor")
      :remote-mine-url "https://github.com/miketz/mor"
      :remote-mine-alias "origin"
      :remote-upstream-url nil ; my own project, so no 3rd party upstream
      :remote-upstream-alias nil
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "24.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'iedit
      :comment nil
      :folder (concat my-module-folder "iedit")
      :remote-mine-url "https://github.com/miketz/iedit"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/victorhge/iedit"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'ido-grid
      :comment nil
      :folder (concat my-module-folder "ido-grid.el")
      :remote-mine-url "https://github.com/miketz/ido-grid.el"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/larkery/ido-grid.el"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'ov
      :comment nil
      :folder (concat my-module-folder "ov")
      :remote-mine-url "https://github.com/miketz/ov"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/emacsorphanage/ov"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'deferred
      :comment nil
      :folder (concat my-module-folder "emacs-deferred")
      :remote-mine-url "https://github.com/miketz/emacs-deferred"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/kiwanami/emacs-deferred"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '((undercover)))
    ,(make-module
      :name 'sallet
      :comment nil
      :folder (concat my-module-folder "sallet")
      :remote-mine-url "https://github.com/miketz/sallet"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/Fuco1/sallet"
      ;; NOTE: original upstream url was https://github.com/tetracat/sallet
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '((dash) (s) (async) (flx) (ov) (f) (shut-up) (deferred))
      :depend-soft '()
      :depend-bundled '())
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
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'slime
      :comment "Interactive mode for common lisp."
      :folder (concat my-module-folder "slime")
      :remote-mine-url "https://github.com/miketz/slime"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/slime/slime"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '((macrostep)))
    ,(make-module
      :name 'sly
      :comment "Fork of SLIME. Seems to make text entry in a lisp file
sluggish. Not currently using."
      :folder (concat my-module-folder "sly")
      :remote-mine-url "https://github.com/miketz/sly"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/joaotavora/sly"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'eglot
      :comment "A mode for LSP. Nice due to no external dependencies for the
mode itself. External language servers are required to use it of course."
      :folder (concat my-module-folder "eglot")
      :remote-mine-url "https://github.com/miketz/eglot"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/joaotavora/eglot"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'lsp-mode
      :comment "A mode for LSP. More fancy features?"
      :folder (concat my-module-folder "lsp-mode")
      :remote-mine-url "https://github.com/miketz/lsp-mode"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/emacs-lsp/lsp-mode"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "26.1")
                     (dash "2.14.1")
                     (dash-functional "2.14.1")
                     (f "0.20.0")
                     (ht "2.0")
                     (spinner "1.7.3")
                     (markdown-mode "2.3")
                     (lv "0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'avy
      :comment "Jump to positions in buffers."
      :folder (concat my-module-folder "avy")
      :remote-mine-url "https://github.com/miketz/avy"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/abo-abo/avy"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'rust-mode
      :comment nil
      :folder (concat my-module-folder "rust-mode")
      :remote-mine-url "https://github.com/miketz/rust-mode"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/rust-lang/rust-mode"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'racer
      :comment nil
      :folder (concat my-module-folder "emacs-racer")
      :remote-mine-url "https://github.com/miketz/emacs-racer"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/racer-rust/emacs-racer"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '((emacs "25.1")
                     (rust-mode "0.2.0")
                     (dash "2.13.0")
                     (s "1.10.0")
                     (f "0.18.2")
                     (pos-tip "0.4.6"))
      :depend-soft '((undercover)) ; undercover is used for tests.
      :depend-bundled '())
    ,(make-module
      :name 'helm
      :comment nil
      :folder (concat my-module-folder "helm")
      :remote-mine-url "https://github.com/miketz/helm"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/emacs-helm/helm"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "25.1")
                     (async "1.9.4")
                     (popup "0.5.3"))
      :depend-soft '()
      :depend-bundled '((helm-core)))
    ,(make-module
      :name 'rg
      :comment "Has a UI that more closely resembles the built-in emacs grep
style. More importantly it avoids spamming rg as you type or prematurely."
      :folder (concat my-module-folder "rg.el")
      :remote-mine-url "https://github.com/miketz/rg.el"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/dajva/rg.el"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "25.1")
                     (transient "0.1.0")
                     (wgrep "2.1.10"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'rainbow-delimiters
      :comment nil
      :folder (concat my-module-folder "rainbow-delimiters")
      :remote-mine-url "https://github.com/miketz/rainbow-delimiters"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/Fanael/rainbow-delimiters"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'js2-mode
      :comment nil
      :folder (concat my-module-folder "js2-mode")
      :remote-mine-url "https://github.com/miketz/js2-mode"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/mooz/js2-mode"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'leerzeichen
      :comment nil
      :folder (concat my-module-folder "leerzeichen.el")
      :remote-mine-url "https://github.com/miketz/leerzeichen.el"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/fgeller/leerzeichen.el"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'citre
      :comment "ctags IDE"
      :folder (concat my-module-folder "citre")
      :remote-mine-url "https://github.com/miketz/citre"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/universal-ctags/citre"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '((company) (clue))
      :depend-bundled '())
    ,(make-module
      :name 'wgrep
      :comment nil
      :folder (concat my-module-folder "Emacs-wgrep")
      :remote-mine-url "https://github.com/miketz/Emacs-wgrep"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/mhayashi1120/Emacs-wgrep"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'projectile
      :comment nil
      :folder (concat my-module-folder "projectile")
      :remote-mine-url "https://github.com/miketz/projectile"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/bbatsov/projectile"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "25.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'swift-mode
      :comment nil
      :folder (concat my-module-folder "swift-mode")
      :remote-mine-url "https://github.com/miketz/swift-mode"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/swift-emacs/swift-mode"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'dank-mode
      :comment "reddit viewer"
      :folder (concat my-module-folder "dank-mode")
      :remote-mine-url "https://github.com/miketz/dank-mode"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/john2x/dank-mode"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'smex
      :comment nil
      :folder (concat my-module-folder "smex")
      :remote-mine-url "https://github.com/miketz/smex"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/nonsequitur/smex"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'pkg-info
      :comment nil
      :folder (concat my-module-folder "pkg-info")
      :remote-mine-url "https://github.com/miketz/pkg-info"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/emacsorphanage/pkg-info"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'flycheck
      :comment nil
      :folder (concat my-module-folder "flycheck")
      :remote-mine-url "https://github.com/miketz/flycheck"
      :remote-mine-alias "origin"
      :remote-upstream-url "https://github.com/flycheck/flycheck"
      :remote-upstream-alias "upstream"
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((dash "2.12.1")
                     (pkg-info "0.4")
                     (let-alist "1.0.4")
                     (seq "1.11")
                     (emacs "24.3"))
      :depend-soft '()
      :depend-bundled '())))

(defun my-byte-compile-all-modules ()
  "Byte compile .el files of all modules."
  (interactive)
  (cl-loop for mod in my-modules
           do
           (my-delete-elc-files (module-folder mod))

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
    (my-delete-elc-files (module-folder mod))

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
                           (s-ends-with-p "sallet" (cl-first f))
                           (s-ends-with-p "libegit2" (cl-first f))))
                     (directory-files-and-attributes my-module-folder
                                                     t "^[^.]" t)))
         (dir-names (mapcar #'cl-first dir-infos)))
    (cl-loop for dir in dir-names
             do
             (my-delete-elc-files dir)
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
  :comment nil
  :folder (concat my-module-folder \"%s\")
  :remote-mine-url \"https://github.com/miketz/%s\"
  :remote-mine-alias \"origin\"
  :remote-upstream-url nil
  :remote-upstream-alias \"upstream\"
  :source-control 'git
  :submodule-p t
  :use-branch \"master\"
  :depend-hard '()
  :depend-soft '()
  :depend-bundled '())\n"
                             dir dir dir)))))

(provide 'my-modules)

;;; my-modules.el ends here
