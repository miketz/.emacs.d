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
  (file-single nil) ;; for single file elisp packages embdedded into my git repo.
  ;; remote format '(name :url "foo.com/package-name"
  ;;                      :alias "origin")
  ;; The names 'mine and 'upstream have special meaning. Where 'upstream is the
  ;; official repo of the package. And 'mine is my fork. Other names can be
  ;; anything but shoudl be descriptive.
  (remotes '())
  (remote-default nil) ; default remote to pull/push
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
  ;; git SHA (or other vc equiv) to use. Alternative to branch as branch means
  ;; you are following latest tip of that branch. A specific commit is more
  ;; exact. However this will be rarely used as the moment I start developing
  ;; my own code I'll need a branch to avoid a detached HEAD state. This is
  ;; more for if I want *this* config to control the state of the package
  ;; rather than the git branch itself.
  (use-commit nil)
  (depend-hard '()) ; requried or important dependencies.
  (depend-soft '()) ; optional dependencies. Or only needed for the tests.
  ;; Note when packages bundle dependencies. For informational purposes so I
  ;; don't try to install something when I dont' need to.
  (depend-bundled '()))

(defvar my-modules
  `(,(make-module
      :name 'paredit
      :comment nil
      :folder (concat my-module-folder "paredit")
      :remotes '((mine :url "https://github.com/miketz/paredit"
                       :alias "origin")
                 (upstream :url "https://mumble.net/~campbell/git/paredit.git"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/swiper"
                       :alias "origin")
                 (upstream :url "https://github.com/abo-abo/swiper"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'ivy-explorer
      :comment nil
      :folder (concat my-module-folder "ivy-explorer")
      :remotes '((mine :url "https://github.com/miketz/ivy-explorer"
                       :alias "origin")
                 (upstream :url "https://github.com/clemera/ivy-explorer"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "25") (ivy "0.10.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'iedit
      :comment nil
      :folder (concat my-module-folder "iedit")
      :remotes '((mine :url "https://github.com/miketz/iedit"
                       :alias "origin")
                 (upstream :url "https://github.com/victorhge/iedit"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://https://github.com/miketz/lispy"
                       :alias "origin")
                 (upstream :url "https://github.com/abo-abo/lispy"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/evil"
                       :alias "origin")
                 (upstream :url "https://github.com/emacs-evil/evil"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/evil-leader"
                       :alias "origin")
                 (upstream :url "https://github.com/cofi/evil-leader"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/expand-region.el"
                       :alias "origin")
                 (upstream :url "https://github.com/magnars/expand-region.el"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/s.el"
                       :alias "origin")
                 (upstream :url "https://github.com/magnars/s.el"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/dash.el"
                       :alias "origin")
                 (upstream :url "https://github.com/magnars/dash.el"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/transient"
                       :alias "origin")
                 (upstream :url "https://github.com/magit/transient"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://https://github.com/miketz/with-editor"
                       :alias "origin")
                 (upstream :url "https://github.com/magit/with-editor"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/magit"
                       :alias "origin")
                 (upstream :url "https://github.com/magit/magit"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "25.1")
                     ;; async not used anymore? (async "20180527")
                     (dash "2.19.1")
                     (transient "0.3.6")
                     (with-editor "3.0.5"))
      :depend-soft '((libgit "0")
                     (compat "28.1.1.2")
                     ;; magit-libgit needs emacs 26.1
                     (emacs "26.1"))
      :depend-bundled '((git-commit "3.3.0")
                        (magit-section "3.3.0")))
    ,(make-module
      :name 'libgit
      :comment "An elisp package with C code bundled."
      :folder (concat my-module-folder "libegit2")
      :remotes '((mine :url "https://github.com/miketz/libegit2"
                       :alias "origin")
                 (upstream :url "https://github.com/magit/libegit2"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/csharp-mode"
                       :alias "origin")
                 (upstream :url "https://github.com/josteink/csharp-mode"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/spinner.el"
                       :alias "origin")
                 (upstream :url "https://github.com/Malabarba/spinner.el"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://https://github.com/miketz/ggtags"
                       :alias "origin")
                 (upstream :url "https://github.com/leoliu/ggtags"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/mor"
                       :alias "origin")) ; my own project, so no 3rd party upstream
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/iedit"
                       :alias "origin")
                 (upstream :url "https://github.com/victorhge/iedit"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/ido-grid.el"
                       :alias "origin")
                 (upstream :url "https://github.com/larkery/ido-grid.el"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/ov"
                       :alias "origin")
                 (upstream :url "https://github.com/emacsorphanage/ov"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/emacs-deferred"
                       :alias "origin")
                 (upstream :url "https://github.com/kiwanami/emacs-deferred"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '((undercover)))
    ,(make-module
      :name 'sallet
      :comment nil
      :folder (concat my-module-folder "sallet")
      :remotes '((mine :url "https://github.com/miketz/sallet"
                       :alias "origin")
                 ;; NOTE: original upstream url was https://github.com/tetracat/sallet
                 (upstream :url "https://github.com/Fuco1/sallet"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/eros"
                       :alias "origin")
                 (upstream :url "https://github.com/xiongtx/eros"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/slime"
                       :alias "origin")
                 (upstream :url "https://github.com/slime/slime"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/sly"
                       :alias "origin")
                 (upstream :url "https://github.com/joaotavora/sly"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/eglot"
                       :alias "origin")
                 (upstream :url "https://github.com/joaotavora/eglot"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/lsp-mode"
                       :alias "origin")
                 (upstream :url "https://github.com/emacs-lsp/lsp-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "26.1")
                     (dash "2.14.1")
                     (dash-functional "2.14.1") ; bundled in dash pacakge
                     (f "0.20.0")
                     (ht "2.0")
                     (spinner "1.7.3")
                     (markdown-mode "2.3")
                     (lv "0")) ; lv is in the hydra package
      :depend-soft '((use-package))
      :depend-bundled '())
    ,(make-module
      :name 'f
      :comment nil
      :folder (concat my-module-folder "f.el")
      :remotes '((mine :url "https://github.com/miketz/f.el"
                       :alias "origin")
                 (upstream :url "https://github.com/rejeep/f.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '((s "1.7.0")
                     (dash "2.2.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'ht
      :comment nil
      :folder (concat my-module-folder "ht.el")
      :remotes '((mine :url "https://github.com/miketz/ht.el"
                       :alias "origin")
                 (upstream :url "https://github.com/Wilfred/ht.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((dash "2.12.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'markdown-mode
      :comment nil
      :folder (concat my-module-folder "markdown-mode")
      :remotes '((mine :url "https://github.com/miketz/markdown-mode"
                       :alias "origin")
                 (upstream :url "https://github.com/jrblevin/markdown-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "25.1"))
      :depend-soft '((edit-indirect))
      :depend-bundled '())
    ,(make-module
      :name 'avy
      :comment "Jump to positions in buffers."
      :folder (concat my-module-folder "avy")
      :remotes '((mine :url "https://github.com/miketz/avy"
                       :alias "origin")
                 (upstream :url "https://github.com/abo-abo/avy"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/rust-mode"
                       :alias "origin")
                 (upstream :url "https://github.com/rust-lang/rust-mode"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/emacs-racer"
                       :alias "origin")
                 (upstream :url "https://github.com/racer-rust/emacs-racer"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/helm"
                       :alias "origin")
                 (upstream :url "https://github.com/emacs-helm/helm"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/rg.el"
                       :alias "origin")
                 (upstream :url "https://github.com/dajva/rg.el"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/rainbow-delimiters"
                       :alias "origin")
                 (upstream :url "https://github.com/Fanael/rainbow-delimiters"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/js2-mode"
                       :alias "origin")
                 (upstream :url "https://github.com/mooz/js2-mode"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/leerzeichen.el"
                       :alias "origin")
                 (upstream :url "https://github.com/fgeller/leerzeichen.el"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/citre"
                       :alias "origin")
                 (upstream :url "https://github.com/universal-ctags/citre"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/Emacs-wgrep"
                       :alias "origin")
                 (upstream :url "https://github.com/mhayashi1120/Emacs-wgrep"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/projectile"
                       :alias "origin")
                 (upstream :url "https://github.com/bbatsov/projectile"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/swift-mode"
                       :alias "origin")
                 (upstream :url "https://github.com/swift-emacs/swift-mode"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/dank-mode"
                       :alias "origin")
                 (upstream :url "https://github.com/john2x/dank-mode"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/smex"
                       :alias "origin")
                 (upstream :url "https://github.com/nonsequitur/smex"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/pkg-info"
                       :alias "origin")
                 (upstream :url "https://github.com/emacsorphanage/pkg-info"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((epl "0.8"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'epl
      :comment nil
      :folder (concat my-module-folder "epl")
      :remotes '((mine :url "https://github.com/miketz/epl"
                       :alias "origin")
                 (upstream :url "https://github.com/cask/epl"
                           :alias "upstream"))
      :remote-default 'mine
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
      :remotes '((mine :url "https://github.com/miketz/flycheck"
                       :alias "origin")
                 (upstream :url "https://github.com/flycheck/flycheck"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((dash "2.12.1")
                     (pkg-info "0.4")
                     (let-alist "1.0.4")
                     (seq "1.11")
                     (emacs "24.3"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'smart-tabs-mode
      :comment nil
      :folder (concat my-module-folder "smarttabs")
      :remotes '((mine :url "https://github.com/miketz/smarttabs"
                       :alias "origin")
                 (upstream :url "https://github.com/jcsalomon/smarttabs"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'web-mode
      :comment nil
      :folder (concat my-module-folder "web-mode")
      :remotes '((mine :url "https://github.com/miketz/web-mode"
                       :alias "origin")
                 (upstream :url "https://github.com/fxbois/web-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'puni
      :comment "Structural editing for any lang. Similar to paredit."
      :folder (concat my-module-folder "puni")
      :remotes '((mine :url "https://github.com/miketz/puni"
                       :alias "origin")
                 (upstream :url "https://github.com/AmaiKinono/puni"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "26.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'ace-link
      :comment nil
      :folder (concat my-module-folder "ace-link")
      :remotes '((mine :url "https://github.com/miketz/ace-link"
                       :alias "origin")
                 (upstream :url "https://github.com/abo-abo/ace-link"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((avy "0.4.0"))
      :depend-soft '((counsel) (w3m))
      :depend-bundled '())
    ,(make-module
      :name 'ace-window
      :comment nil
      :folder (concat my-module-folder "ace-window")
      :remotes '((mine :url "https://github.com/miketz/ace-window"
                       :alias "origin")
                 (upstream :url "https://github.com/abo-abo/ace-window"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((avy "0.5.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'hydra
      :comment nil
      :folder (concat my-module-folder "hydra")
      :remotes '((mine :url "https://github.com/miketz/hydra"
                       :alias "origin")
                 (upstream :url "https://github.com/abo-abo/hydra"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '((lv "0")))
    ,(make-module
      :name 'flames-of-freedom
      :comment nil
      :folder (concat my-module-folder "FlamesOfFreedom")
      :remotes '((upstream :url "https://github.com/wiz21b/FlamesOfFreedom" ; just a fun pacakge so no fork.
                           :alias "origin"))
      :remote-default 'upstream
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "25.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'posframe
      :comment nil
      :folder (concat my-module-folder "posframe")
      :remotes '((mine :url "https://github.com/miketz/posframe"
                       :alias "origin")
                 (upstream :url "https://github.com/tumashu/posframe"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '((emacs "26.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'ivy-posframe
      :comment nil
      :folder (concat my-module-folder "ivy-posframe")
      :remotes '((mine :url "https://github.com/miketz/ivy-posframe"
                       :alias "origin")
                 (upstream :url "https://github.com/tumashu/ivy-posframe"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '((emacs "26.0") (posframe "1.0.0") (ivy "0.13.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'autothemer
      :comment nil
      :folder (concat my-module-folder "autothemer")
      :remotes '((mine :url "https://github.com/miketz/autothemer"
                       :alias "origin")
                 (upstream :url "https://github.com/jasonm23/autothemer"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '((dash "2.10.0") (emacs "24") (cl-lib "0.5"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'company
      :comment nil
      :folder (concat my-module-folder "company-mode")
      :remotes '((mine :url "https://github.com/miketz/company-mode"
                       :alias "origin")
                 (upstream :url "https://github.com/company-mode/company-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "24.3"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'company-web
      :comment nil
      :folder (concat my-module-folder "company-web")
      :remotes '((mine :url "https://github.com/miketz/company-web"
                       :alias "origin")
                 (upstream :url "https://github.com/osv/company-web"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '((company "0.8.0") (dash "2.8.0") (cl-lib "0.5.0")
                     (web-completion-data "0.1.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'company-lsp
      :comment nil
      :folder (concat my-module-folder "company-lsp")
      :remotes '((mine :url "https://github.com/miketz/company-lsp"
                       :alias "origin")
                 (upstream :url "https://github.com/tigersoldier/company-lsp"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "25.1") (lsp-mode "6.0")
                     (company "0.9.0") (s "1.2.0") (dash "2.11.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'web-completion-data
      :comment nil
      :folder (concat my-module-folder "web-completion-data")
      :remotes '((mine :url "https://github.com/miketz/web-completion-data"
                       :alias "origin")
                 (upstream :url "https://github.com/osv/web-completion-data"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'visual-indentation-mode
      :comment "Alternative to highlight-indent-guides."
      :folder my-module-folder
      :remotes '((upstream :url "https://github.com/skeeto/visual-indentation-mode"
                           :alias "upstream"))
      :remote-default 'upstream
      :source-control 'git
      :submodule-p nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'exec-path-from-shell
      :comment "Set environemnt vars on mac."
      :folder my-module-folder
      :remotes '((upstream :url "https://github.com/purcell/exec-path-from-shell"
                           :alias "upstream"))
      :remote-default 'upstream
      :source-control 'git
      :submodule-p nil
      :use-branch nil
      :depend-hard '((emacs "24.1") (cl-lib "0.6"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'vc-fossil
      :comment nil
      :folder my-module-folder
      :remotes '((upstream :url "https://chiselapp.com/user/venks/repository/emacs-fossil"
                           :alias "upstream"
                           :type fossil?)
                 (mirror :url "https://github.com/venks1/emacs-fossil/"
                           :alias "mirror"))
      :remote-default 'mirror
      :source-control 'git
      :submodule-p nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'lusty-explorer
      :comment nil
      :folder my-module-folder
      :remotes '((upstream :url "https://github.com/sjbach/lusty-emacs"
                           :alias "upstream"))
      :remote-default 'upstream
      :source-control 'git
      :submodule-p nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'fennel-mode
      :comment nil
      :folder (concat my-module-folder "fennel-mode")
      :remotes '((mine :url "https://github.com/miketz/fennel-mode"
                       :alias "origin")
                 (upstream :url "https://git.sr.ht/~technomancy/fennel-mode"
                           :alias "upstream")
                 ;; used this mirror for my "mine" fork on github. But
                 ;; completly unused.
                 (mirror :url "https://github.com/emacsmirror/fennel-mode"
                         :alias "mirror"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "mine"
      :depend-hard '((emacs "26.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'vertico
      :comment nil
      :folder (concat my-module-folder "vertico")
      :remotes '((mine :url "https://github.com/miketz/vertico"
                       :alias "origin")
                 (upstream :url "https://github.com/minad/vertico"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "main"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'typescript-mode
      :comment nil
      :folder (concat my-module-folder "typescript.el")
      :remotes '((mine :url "https://github.com/miketz/typescript.el"
                       :alias "origin")
                 (upstream :url "https://github.com/emacs-typescript/typescript.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'tide
      :comment nil
      :folder (concat my-module-folder "tide")
      :remotes '((mine :url "https://github.com/miketz/tide"
                       :alias "origin")
                 (upstream :url "https://github.com/ananthakumaran/tide"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "25.1")
                     (dash "2.10.0")
                     (s "1.11.0")
                     (flycheck "27")
                     (typescript-mode "0.1")
                     (cl-lib "0.5"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'compat
      :comment nil
      :folder (concat my-module-folder "compat.el")
      :remotes '((mine :url "https://github.com/miketz/compat.el"
                       :alias "origin")
                 (mirror :url "https://github.com/phikal/compat.el"
                         :alias "mirror"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'slime-volleyball
      :comment nil
      :folder (concat my-module-folder "slime-volleyball")
      :remotes '((mine :url "https://github.com/miketz/slime-volleyball"
                       :alias "origin")
                 (upstream :url "https://github.com/fitzsim/slime-volleyball"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "mine" ;; ignores .elc files
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'macrostep
      :comment nil
      :folder (concat my-module-folder "macrostep")
      :remotes '((mine :url "https://github.com/miketz/macrostep"
                       :alias "origin")
                 (upstream :url "https://github.com/joddie/macrostep"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'sx
      :comment nil
      :folder (concat my-module-folder "sx.el")
      :remotes '((mine :url "https://github.com/miketz/sx.el"
                       :alias "origin")
                 (upstream :url "https://github.com/vermiculus/sx.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '((emacs "24.1")
                     (cl-lib "0.5")
                     (json "1.3")
                     (markdown-mode "2.0")
                     (let-alist "1.0.3"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'sunrise
      :comment nil
      :folder (concat my-module-folder "sunrise-commander")
      :remotes '((upstream :url "https://github.com/sunrise-commander/sunrise-commander"
                           :alias "upstream"))
      :remote-default 'upstream
      :source-control 'git
      :submodule-p t
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'jsonian
      :comment nil
      :folder my-module-folder
      :file-single "jsonian.el"
      :remotes '((upstream :url "https://github.com/iwahbe/jsonian"
                           :alias "upstream"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'highlight-tail
      :comment nil
      :folder my-module-folder
      :file-single "highlight-tail.el"
      :remotes '((upstream :url "https://www.emacswiki.org/emacs/highlight-tail.el"
                           :alias "upstream")
                 (mirror :url "https://github.com/ahungry/emacswiki-mirror/blob/master/highlight-tail.el"
                         :alias "mirror"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'key-chord
      :comment nil
      :folder my-module-folder
      :file-single "key-chord.el"
      :remotes '((wiki :url "https://www.emacswiki.org/emacs/KeyChord"
                       :alias "wiki")
                 ;; orphanage is used by MELPA
                 (orphanage :url "https://github.com/emacsorphanage/key-chord"
                            :alias "orphanage")
                 (mirror :url "https://github.com/emacsmirror/emacswiki.org/blob/master/key-chord.el"
                         :alias "mirror"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :use-branch nil
      :depend-hard '()
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
