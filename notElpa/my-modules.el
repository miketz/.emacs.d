;;; my-modules.el --- Package handling -*- lexical-binding: t -*-

;;; Commentary:
;;; Tracking elisp packages.  Mostly as git submodules, but other
;;; ways too if the package is not a git repo.
;;; Avoiding the package manager package.el and elpa/melpa/etc.
;;;
;;; This file is more about documenting info about packages than handling the
;;; packages.  Using git features to manually handle packages.  This file is
;;; just info that may be useful.  Maybe a few automated things will be added like
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
  (file-single nil) ;; for single file elisp packages embedded into my git repo.
  ;; remote format '(name :url "foo.com/package-name"
  ;;                      :alias "origin")
  ;; The names 'mine and 'upstream have special meaning. Where 'upstream is the
  ;; official repo of the package. And 'mine is my fork. Other names can be
  ;; anything but should be descriptive. Each remote will be a plist of the
  ;; form '(:sym mine :url "https://someurl" :alias "origin")
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
  ;; The canonical branch used by the upstream. Usually "master" or "main".
  (main-branch nil) ; master
  ;; Usually the same as `main-branch' but sometimes a private "mine" branch
  ;; with a few odd tweaks. This is the branch I use locally on my side.
  (use-branch nil) ; master, mine
  ;; git SHA (or other vc equiv) to use. Alternative to branch as branch means
  ;; you are following latest tip of that branch. A specific commit is more
  ;; exact. However this will be rarely used as the moment I start developing
  ;; my own code I'll need a branch to avoid a detached HEAD state. This is
  ;; more for if I want *this* config to control the state of the package
  ;; rather than the git branch itself.
  (use-commit nil)
  (depend-hard '()) ; required or important dependencies.
  (depend-soft '()) ; optional dependencies. Or only needed for the tests.
  ;; Note when packages bundle dependencies. For informational purposes so I
  ;; don't try to install something when I don't need to.
  (depend-bundled '()))

(defvar my-modules
  `(,(make-module
      :name 'paredit
      :comment nil
      :folder (concat my-module-folder "paredit")
      :remotes '((:sym mine :url "https://github.com/miketz/paredit"
                       :alias "origin")
                 (:sym upstream :url "https://paredit.org/paredit.git"
                       ;;:url "https://mumble.net/~campbell/git/paredit.git"
                       :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'combobulate
      :comment nil
      :folder (concat my-module-folder "combobulate")
      :remotes '((:sym mine :url "https://github.com/miketz/combobulate"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/mickeynp/combobulate"
                       :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "development" ; all the action seems to be going on here so treat it as the main branch.
      :use-branch "development"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'treesitter-context
      :comment nil
      :folder (concat my-module-folder "treesitter-context.el")
      :remotes '((:sym mine :url "https://github.com/miketz/treesitter-context.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/zbelial/treesitter-context.el"
                       :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "29.1")
                     (posframe "1.4.2"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'buttercup
      :comment "Testing library. Used by several packages."
      :folder (concat my-module-folder "emacs-buttercup")
      :remotes '((:sym mine :url "https://github.com/miketz/emacs-buttercup"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/jorgenschaefer/emacs-buttercup"
                       :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'swiper
      :comment nil
      :folder (concat my-module-folder "swiper")
      :remotes '((:sym mine :url "https://github.com/miketz/swiper"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/abo-abo/swiper"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'ivy-explorer
      :comment nil
      :folder (concat my-module-folder "ivy-explorer")
      :remotes '((:sym mine :url "https://github.com/miketz/ivy-explorer"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/clemera/ivy-explorer"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "25") (ivy "0.10.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'visual-fill-column
      :comment nil
      :folder (concat my-module-folder "visual-fill-column")
      :remotes '((:sym mine :url "https://github.com/miketz/visual-fill-column"
                       :alias "origin")
                 (:sym upstream :url "https://codeberg.org/joostkremers/visual-fill-column"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "main"
      :use-branch "main"
      :depend-hard '((emacs "25.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'klondike
      :comment nil
      :folder (concat my-module-folder "Emacs-Klondike")
      :remotes '((:sym mine :url "https://github.com/miketz/Emacs-Klondike"
                       :alias "origin")
                 (:sym upstream :url "https://codeberg.org/WammKD/Emacs-Klondike"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "primary"
      :use-branch "primary"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'stem-reading-mode
      :comment nil
      :folder (concat my-module-folder "stem-reading-mode.el")
      :remotes '((:sym mine :url "https://github.com/miketz/stem-reading-mode.el"
                       :alias "origin")
                 (:sym upstream :url "https://gitlab.com/wavexx/stem-reading-mode.el.git"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "25.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'clojure-mode
      :comment nil
      :folder (concat my-module-folder "clojure-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/clojure-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/clojure-emacs/clojure-mode"
                       :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "25.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'iedit
      :comment nil
      :folder (concat my-module-folder "iedit")
      :remotes '((:sym mine :url "https://github.com/miketz/iedit"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/victorhge/iedit"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'lispy
      :comment nil
      :folder (concat my-module-folder "lispy")
      :remotes '((:sym mine :url "https://https://github.com/miketz/lispy"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/abo-abo/lispy"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
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
      :remotes '((:sym mine :url "https://github.com/miketz/evil"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/emacs-evil/evil"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "24.1")
                     (cl-lib "0.5"))
      :depend-soft '()
      :depend-bundled '(;; (undo-tree "0.7.4") ;; no longer a dependency
                        ;; (goto-chg "1.6") ;; no longer a dependency
                        ))
    ,(make-module
      :name 'evil-leader
      :comment nil
      :folder (concat my-module-folder "evil-leader")
      :remotes '((:sym mine :url "https://github.com/miketz/evil-leader"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/cofi/evil-leader"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((evil "0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'evil-escape
      :comment nil
      :folder my-module-folder
      :file-single "evil-escape.el"
      :remotes '((:sym upstream :url "https://github.com/syl20bnr/evil-escape"
                           :alias "upstream"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '((emacs "24") (evil "1.0.9") (cl-lib "0.5"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'expand-region
      :comment nil
      :folder (concat my-module-folder "expand-region.el")
      :remotes '((:sym mine :url "https://github.com/miketz/expand-region.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/magnars/expand-region.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 's
      :comment nil
      :folder (concat my-module-folder "s.el")
      :remotes '((:sym mine :url "https://github.com/miketz/s.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/magnars/s.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'dash
      :comment nil
      :folder (concat my-module-folder "dash.el")
      :remotes '((:sym mine :url "https://github.com/miketz/dash.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/magnars/dash.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "24"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'transient
      :comment nil
      :folder (concat my-module-folder "transient")
      :remotes '((:sym mine :url "https://github.com/miketz/transient"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/magit/transient"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "main"
      :use-branch "main"
      :depend-hard '((emacs "25.1") (compat "28.1.1.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'with-editor
      :comment nil
      :folder (concat my-module-folder "with-editor")
      :remotes '((:sym mine :url "https://https://github.com/miketz/with-editor"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/magit/with-editor"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "main"
      :use-branch "main"
      :depend-hard '((emacs "24.4"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'magit
      :comment nil
      :folder (concat my-module-folder "magit")
      :remotes '((:sym mine :url "https://github.com/miketz/magit"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/magit/magit"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "main"
      :use-branch "mine" ;; ignore *.elc files
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
      :remotes '((:sym mine :url "https://github.com/miketz/libegit2"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/magit/libegit2"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "main"
      :use-branch "mine" ;; ignore .elc files
      :depend-hard '((emacs "25.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      ;; NOTE: as of 11-19-2022 this is moved into emacs core. But keep this
      ;; submodule to support older Emacs versions.
      :name 'csharp-mode
      :comment nil
      :folder (concat my-module-folder "csharp-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/csharp-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/josteink/csharp-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'ctrlf
      :comment nil
      :folder (concat my-module-folder "ctrlf")
      :remotes '((:sym upstream :url "https://github.com/radian-software/ctrlf"
                           :alias "origin"))
      :remote-default 'upstream
      :source-control 'git
      :submodule-p t
      :main-branch "main"
      :use-branch "main"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'spinner
      :comment nil
      :folder (concat my-module-folder "spinner.el")
      :remotes '((:sym mine :url "https://github.com/miketz/spinner.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/Malabarba/spinner.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'ggtags
      :comment nil
      :folder (concat my-module-folder "ggtags")
      :remotes '((:sym mine :url "https://https://github.com/miketz/ggtags"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/leoliu/ggtags"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      ;; NOTE: this package is is no longer bundled/vendored with evil.
      :name 'goto-chg
      :comment nil
      :folder (concat my-module-folder "goto-chg")
      :remotes '((:sym mine :url "https://github.com/miketz/goto-chg"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/emacs-evil/goto-chg"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'mode-on-region
      :comment nil
      :folder (concat my-module-folder "mine/mor")
      :remotes '((:sym mine :url "https://github.com/miketz/mor"
                       :alias "origin")) ; my own project, so no 3rd party upstream
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "24.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'rsvp
      :comment nil
      :folder (concat my-module-folder "mine/rapid-serial-visual-presentation")
      :remotes '((:sym mine :url "https://github.com/miketz/rapid-serial-visual-presentation"
                       :alias "origin")
                 ;; my own project, so no 3rd party upstream
                 (:sym upstream :url "https://github.com/miketz/rapid-serial-visual-presentation"
                       :alias "origin"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "24.4"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'ido-grid
      :comment nil
      :folder (concat my-module-folder "ido-grid.el")
      :remotes '((:sym mine :url "https://github.com/miketz/ido-grid.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/larkery/ido-grid.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'ov
      :comment nil
      :folder (concat my-module-folder "ov")
      :remotes '((:sym mine :url "https://github.com/miketz/ov"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/emacsorphanage/ov"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'deferred
      :comment nil
      :folder (concat my-module-folder "emacs-deferred")
      :remotes '((:sym mine :url "https://github.com/miketz/emacs-deferred"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/kiwanami/emacs-deferred"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '((undercover))
      :depend-bundled '())
    ,(make-module
      :name 'flx
      :comment nil
      :folder (concat my-module-folder "flx")
      :remotes '((:sym mine :url "https://github.com/miketz/flx"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/lewang/flx"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ;; NOTE: selectrum author has chosen to deprecate the package in favor of
    ;;       vertico
    ;; ,(make-module
    ;;   :name 'selectrum
    ;;   :comment nil
    ;;   :folder (concat my-module-folder "selectrum")
    ;;   :remotes '((:sym upstream :url "https://github.com/radian-software/selectrum"
    ;;                        :alias "upstream"))
    ;;   :remote-default 'upstream
    ;;   :source-control 'git
    ;;   :submodule-p t
    ;;   :main-branch "master"
    ;;   :use-branch "master"
    ;;   :depend-hard '()
    ;;   :depend-soft '()
    ;;   :depend-bundled '())
    ,(make-module
      :name 'sallet
      :comment nil
      :folder (concat my-module-folder "sallet")
      :remotes '((:sym mine :url "https://github.com/miketz/sallet"
                       :alias "origin")
                 ;; NOTE: original upstream url was https://github.com/tetracat/sallet
                 (:sym upstream :url "https://github.com/Fuco1/sallet"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '((dash) (s) (async) (flx) (ov) (f) (shut-up) (deferred))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'eros
      :comment "Package to display eval'd results in an overlay. Implementation
scraped out of cider. I was able to make eros work for common lisp evals."
      :folder (concat my-module-folder "eros")
      :remotes '((:sym mine :url "https://github.com/miketz/eros"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/xiongtx/eros"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'elisp-slime-nav
      :comment nil
      :folder (concat my-module-folder "elisp-slime-nav")
      :remotes '((:sym mine :url "https://github.com/miketz/elisp-slime-nav"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/purcell/elisp-slime-nav"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'async
      :comment nil
      :folder (concat my-module-folder "emacs-async")
      :remotes '((:sym mine :url "https://github.com/miketz/emacs-async"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/jwiegley/emacs-async"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'lua-mode
      :comment nil
      :folder (concat my-module-folder "lua-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/lua-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/immerrr/lua-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'slime
      :comment "Interactive mode for common lisp."
      :folder (concat my-module-folder "slime")
      :remotes '((:sym mine :url "https://github.com/miketz/slime"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/slime/slime"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '((macrostep)))
    ,(make-module
      :name 'slime-company
      :comment nil
      :folder (concat my-module-folder "slime-company")
      :remotes '((:sym mine :url "https://github.com/miketz/slime-company"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/anwyn/slime-company"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '((emacs "24.4") (slime "2.13") (company "0.9.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'sly
      :comment "Fork of SLIME. Seems to make text entry in a lisp file
sluggish. Not currently using."
      :folder (concat my-module-folder "sly")
      :remotes '((:sym mine :url "https://github.com/miketz/sly"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/joaotavora/sly"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'eglot
      :comment "A mode for LSP. Nice due to no external dependencies for the
mode itself. External language servers are required to use it of course."
      :folder (concat my-module-folder "eglot")
      :remotes '((:sym mine :url "https://github.com/miketz/eglot"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/joaotavora/eglot"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "works" ;; using an old version of eglot that works with Emacs 28.2. naming branch "works".
      :depend-hard '((emacs "26.3")
                     (jsonrpc "1.0.16")
                     (flymake "1.2.1")
                     (project "0.9.8")
                     (xref "1.6.2")
                     (eldoc "1.11.0")
                     (seq "2.23")
                     (external-completion "0.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'lsp-mode
      :comment "A mode for LSP. More fancy features?"
      :folder (concat my-module-folder "lsp-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/lsp-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/emacs-lsp/lsp-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "26.3")
                     (dash "2.18.0")
                     (f "0.20.0")
                     (ht "2.3")
                     (spinner "1.7.3")
                     (markdown-mode "2.3")
                     (lv "0") ; lv is in the hydra package
                     (eldoc "1.11"))
      :depend-soft '((use-package))
      :depend-bundled '())
    ,(make-module
      :name 'f
      :comment nil
      :folder (concat my-module-folder "f.el")
      :remotes '((:sym mine :url "https://github.com/miketz/f.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/rejeep/f.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((s "1.7.0")
                     (dash "2.2.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'ht
      :comment nil
      :folder (concat my-module-folder "ht.el")
      :remotes '((:sym mine :url "https://github.com/miketz/ht.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/Wilfred/ht.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((dash "2.12.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'markdown-mode
      :comment nil
      :folder (concat my-module-folder "markdown-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/markdown-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/jrblevin/markdown-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "25.1"))
      :depend-soft '((edit-indirect))
      :depend-bundled '())
    ,(make-module
      :name 'avy
      :comment "Jump to positions in buffers."
      :folder (concat my-module-folder "avy")
      :remotes '((:sym mine :url "https://github.com/miketz/avy"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/abo-abo/avy"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'rust-mode
      :comment nil
      :folder (concat my-module-folder "rust-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/rust-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/rust-lang/rust-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'racer
      :comment nil
      :folder (concat my-module-folder "emacs-racer")
      :remotes '((:sym mine :url "https://github.com/miketz/emacs-racer"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/racer-rust/emacs-racer"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
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
      :remotes '((:sym mine :url "https://github.com/miketz/helm"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/emacs-helm/helm"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
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
      :remotes '((:sym mine :url "https://github.com/miketz/rg.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/dajva/rg.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "25.1")
                     (transient "0.3.0")
                     (wgrep "2.1.10"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'rainbow-delimiters
      :comment nil
      :folder (concat my-module-folder "rainbow-delimiters")
      :remotes '((:sym mine :url "https://github.com/miketz/rainbow-delimiters"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/Fanael/rainbow-delimiters"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'js2-mode
      :comment nil
      :folder (concat my-module-folder "js2-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/js2-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/mooz/js2-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'js2-highlight-vars
      :comment nil
      :folder (concat my-module-folder "js2-highlight-vars.el")
      :remotes '((:sym mine :url "https://github.com/miketz/js2-highlight-vars.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/unhammer/js2-highlight-vars.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '((emacs "24.4") (js2-mode "20150908"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'json-mode
      :comment nil
      :folder (concat my-module-folder "json-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/json-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/joshwnj/json-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '((json-snatcher "1.0.0") (emacs "24.4"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'json-snatcher
      :comment nil
      :folder (concat my-module-folder "json-snatcher")
      :remotes '((:sym mine :url "https://github.com/miketz/json-snatcher"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/Sterlingg/json-snatcher"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'leerzeichen
      :comment nil
      :folder (concat my-module-folder "leerzeichen.el")
      :remotes '((:sym mine :url "https://github.com/miketz/leerzeichen.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/fgeller/leerzeichen.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'citre
      :comment "ctags IDE"
      :folder (concat my-module-folder "citre")
      :remotes '((:sym mine :url "https://github.com/miketz/citre"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/universal-ctags/citre"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '((company) (clue))
      :depend-bundled '())
    ,(make-module
      :name 'haskell-mode
      :comment nil
      :folder (concat my-module-folder "haskell-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/haskell-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/haskell/haskell-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'wgrep
      :comment nil
      :folder (concat my-module-folder "Emacs-wgrep")
      :remotes '((:sym mine :url "https://github.com/miketz/Emacs-wgrep"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/mhayashi1120/Emacs-wgrep"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'projectile
      :comment nil
      :folder (concat my-module-folder "projectile")
      :remotes '((:sym mine :url "https://github.com/miketz/projectile"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/bbatsov/projectile"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "25.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'swift-mode
      :comment nil
      :folder (concat my-module-folder "swift-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/swift-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/swift-emacs/swift-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'dank-mode
      :comment "reddit viewer"
      :folder (concat my-module-folder "dank-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/dank-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/john2x/dank-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'darkroom
      :comment nil
      :folder (concat my-module-folder "darkroom")
      :remotes '((:sym mine :url "https://github.com/miketz/darkroom"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/joaotavora/darkroom"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'smex
      :comment nil
      :folder (concat my-module-folder "smex")
      :remotes '((:sym mine :url "https://github.com/miketz/smex"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/nonsequitur/smex"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'pkg-info
      :comment nil
      :folder (concat my-module-folder "pkg-info")
      :remotes '((:sym mine :url "https://github.com/miketz/pkg-info"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/emacsorphanage/pkg-info"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((epl "0.8"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'epl
      :comment nil
      :folder (concat my-module-folder "epl")
      :remotes '((:sym mine :url "https://github.com/miketz/epl"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/cask/epl"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'erc-hl-nicks
      :comment nil
      :folder (concat my-module-folder "erc-hl-nicks")
      :remotes '((:sym mine :url "https://github.com/miketz/erc-hl-nicks"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/leathekd/erc-hl-nicks"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'esxml
      :comment nil
      :folder (concat my-module-folder "esxml")
      :remotes '((:sym mine :url "https://github.com/miketz/esxml"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/tali713/esxml"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'flycheck
      :comment nil
      :folder (concat my-module-folder "flycheck")
      :remotes '((:sym mine :url "https://github.com/miketz/flycheck"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/flycheck/flycheck"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
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
      :remotes '((:sym mine :url "https://github.com/miketz/smarttabs"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/jcsalomon/smarttabs"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine" ;; eager macro expansion bug fix on emacs 29+
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'web-mode
      :comment nil
      :folder (concat my-module-folder "web-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/web-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/fxbois/web-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'puni
      :comment "Structural editing for any lang. Similar to paredit."
      :folder (concat my-module-folder "puni")
      :remotes '((:sym mine :url "https://github.com/miketz/puni"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/AmaiKinono/puni"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "26.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'ace-link
      :comment nil
      :folder (concat my-module-folder "ace-link")
      :remotes '((:sym mine :url "https://github.com/miketz/ace-link"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/abo-abo/ace-link"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((avy "0.4.0"))
      :depend-soft '((counsel) (w3m))
      :depend-bundled '())
    ,(make-module
      :name 'ace-window
      :comment nil
      :folder (concat my-module-folder "ace-window")
      :remotes '((:sym mine :url "https://github.com/miketz/ace-window"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/abo-abo/ace-window"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '((avy "0.5.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'adoc-mode
      :comment nil
      :folder (concat my-module-folder "adoc-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/adoc-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/sensorflo/adoc-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((markup-faces "1.0.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'markup-faces
      :comment nil
      :folder (concat my-module-folder "markup-faces")
      :remotes '((:sym mine :url "https://github.com/miketz/markup-faces"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/sensorflo/markup-faces"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine" ;; ignore .elc files
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'align-let
      :comment nil
      :folder my-module-folder
      :file-single "align-let.el"
      :remotes '((:sym upstream :url "http://user42.tuxfamily.org/align-let/index.html"
                           :alias "upstream")
                 (:sym wiki :url "https://www.emacswiki.org/emacs/AlignLet"
                            :alias "wiki"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'csv-stuff
      :comment nil
      :folder my-module-folder
      :file-single "csv-stuff.el"
      :remotes nil ;; single fn copied from reddit.
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'obvious
      :comment nil
      :folder my-module-folder
      :file-single "obvious.el"
      :remotes nil
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module ;; NOTE: not using this. Just for info
      :name 'color-mode
      :comment nil
      :folder my-module-folder
      :file-single "color-mode.el"
      :remotes '((:sym upstream :url "https://www-cs-faculty.stanford.edu/~knuth/programs/color-mode.el"
                           :alias "upstream"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module ;; NOTE: see the "eros" package which was inspired by this.
      :name 'cider-style-overlays
      :comment "Eval overlay experiment. Scrapping code from a blog."
      :folder my-module-folder
      :file-single "cider-style-overlays.el"
      :remotes '((:sym blog :url "endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html"
                       :alias "blog"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      ;; NOTE: not currently used
      ;; NOTE: See blog https://amitp.blogspot.com/2013/05/emacs-highlight-active-buffer.html
      ;; NOTE: author recommends using https://github.com/mina86/auto-dim-other-buffers.el instead.
      :name 'highlight-focus
      :comment nil
      :folder my-module-folder
      :file-single "highlight-focus.el"
      :remotes '((:sym upstream :url "https://github.com/kriyative/highlight-focus"
                           :alias "upstream"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'lolcat
      :comment "Applies rainbow colors to the fonts in a buffer."
      :folder my-module-folder
      :file-single "lolcat.el"
      :remotes '((:sym upstream :url "https://github.com/xuchunyang/lolcat.el"
                           :alias "upstream"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      ;; NOTE: random code from #emacs irc.
      :name 'mosue-stuff
      :comment nil
      :folder my-module-folder
      :file-single "mouse-stuff.el"
      :remotes nil
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      ;; NOTE: random code from #emacs irc.
      :name 'keramida
      :comment "window behavior"
      :folder my-module-folder
      :file-single "keramida.el"
      :remotes nil
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'malyon
      :comment "Z-machine interpreter for playing text-based adventure games."
      :folder my-module-folder
      :file-single "malyon.el"
      :remotes '((:sym upstream :url "https://github.com/speedenator/malyon"
                           :alias "upstream"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'minesweeper
      :comment nil
      :folder my-module-folder
      :file-single "minesweeper.el"
      :remotes '((:sym upstream :url "https://hg.sr.ht/~zck/minesweeper"
                           :alias "upstream"))
      :remote-default nil
      :source-control 'git ;; upstream uses hg (mercurial)
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'snow
      :comment "Fun snow scene package."
      :folder my-module-folder
      :file-single "snow.el"
      :remotes '((:sym upstream :url "https://github.com/alphapapa/snow.el"
                           :alias "upstream"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'white-christmas
      :comment "Fun snow scene package."
      :folder my-module-folder
      :file-single "white-christmas.el"
      :remotes '((:sym blog :url "https://with-emacs.com/posts/fun/white-christmas-in-emacs/"
                       :alias "blog"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'devil
      :comment nil
      :folder my-module-folder
      :file-single "devil.el"
      :remotes '((:sym upstream :url "https://github.com/susam/devil"
                       :alias "upstream"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'counsel-fd
      :comment nil
      :folder my-module-folder
      :file-single "counsel-fd.el"
      :remotes '((:sym upstream :url "https://github.com/CsBigDataHub/counsel-fd"
                       :alias "upstream"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '(counsel)
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      ;; code found of stackoverflow.
      :name 'twelve-m-calendar
      :comment nil
      :folder my-module-folder
      :file-single "twelve-m-calendar.el"
      :remotes '((:sym upstream :url "https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months"
                           :alias "upstream"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'zone-nyan
      :comment nil
      :folder my-module-folder
      :file-single "zone-nyan.el"
      :remotes '((:sym upstream :url "https://depp.brause.cc/zone-nyan/"
                           :alias "upstream"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '((emacs "24.3") (esxml "0.3.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'zone-rainbow
      :comment nil
      :folder my-module-folder
      :file-single "zone-rainbow.el"
      :remotes '((:sym upstream :url "https://github.com/kawabata/zone-rainbow"
                           :alias "upstream"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '((emacs "24.3"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'zone-sl
      :comment nil
      :folder my-module-folder
      :file-single "zone-sl.el"
      :remotes '((:sym upstream :url "https://github.com/kawabata/zone-sl"
                           :alias "upstream"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '((emacs "24.3"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'hydra
      :comment nil
      :folder (concat my-module-folder "hydra")
      :remotes '((:sym mine :url "https://github.com/miketz/hydra"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/abo-abo/hydra"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '((lv "0")))
    ,(make-module
      ;; see web page https://depp.brause.cc/nov.el/
      ;; TODO: point to the new upstream.
      :name 'nov
      :comment "epub reader"
      :folder (concat my-module-folder "nov.el")
      :remotes '((:sym mine :url "https://github.com/miketz/nov.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/wasamasa/nov.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '((dash "2.12.0") (esxml "0.3.3") (emacs "24.4"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'num3-mode
      :comment nil
      :folder (concat my-module-folder "num3-mode")
      ;; Upstream not wired up because it's in elpa. I think elpa combines
      ;; multiple projects in 1 repo so it's difficult to use an an "upstream".
      ;; The "mine" fork is just a copy/paste of the code then upload to github.
      :remotes '((:sym mine :url "https://github.com/miketz/num3-mode"
                       :alias "origin"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'nyan-mode
      :comment nil
      :folder (concat my-module-folder "nyan-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/nyan-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/TeMPOraL/nyan-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'php-mode
      :comment nil
      :folder (concat my-module-folder "php-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/php-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/emacs-php/php-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'reformatter
      :comment nil
      :folder (concat my-module-folder "emacs-reformatter")
      :remotes '((:sym mine :url "https://github.com/miketz/emacs-reformatter"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/purcell/emacs-reformatter"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "24.3"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'zig-mode
      :comment nil
      :folder (concat my-module-folder "zig-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/zig-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/ziglang/zig-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "24.3") (reformatter "0.6"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'go-mode
      :comment nil
      :folder (concat my-module-folder "go-mode.el")
      :remotes '((:sym mine :url "https://github.com/miketz/go-mode.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/dominikh/go-mode.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "26.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'zoutline
      :comment nil
      :folder (concat my-module-folder "zoutline")
      :remotes '((:sym mine :url "https://github.com/miketz/zoutline"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/abo-abo/zoutline"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'yasnippet
      :comment nil
      :folder (concat my-module-folder "yasnippet")
      :remotes '((:sym mine :url "https://github.com/miketz/yasnippet"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/joaotavora/yasnippet"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'yaml-mode
      :comment nil
      :folder (concat my-module-folder "yaml-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/yaml-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/yoshiki/yaml-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'vimrc-mode
      :comment nil
      :folder (concat my-module-folder "vimrc-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/vimrc-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/mcandre/vimrc-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'unkillable-scratch
      :comment nil
      :folder (concat my-module-folder "unkillable-scratch")
      :remotes '((:sym mine :url "https://github.com/miketz/unkillable-scratch"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/EricCrosson/unkillable-scratch"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'sicp
      :comment "The SICP book in emacs info format."
      :folder (concat my-module-folder "sicp-info")
      :remotes '((:sym mine :url "https://github.com/miketz/sicp-info"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/webframp/sicp-info"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine" ;; ignore *.elc files
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'prescient
      :comment nil
      :folder (concat my-module-folder "prescient.el")
      :remotes '((:sym upstream :url "https://github.com/radian-software/prescient.el"
                           :alias "origin"))
      :remote-default 'upstream
      :source-control 'git
      :submodule-p t
      :main-branch "main"
      :use-branch "main"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'pos-tip
      :comment nil
      :folder (concat my-module-folder "pos-tip")
      :remotes '((:sym mine :url "https://github.com/miketz/pos-tip"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/pitkali/pos-tip"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'powershell
      :comment nil
      :folder (concat my-module-folder "powershell.el")
      :remotes '((:sym mine :url "https://github.com/miketz/powershell.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/jschaf/powershell.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'highlight-indent-guides
      :comment nil
      :folder (concat my-module-folder "highlight-indent-guides")
      :remotes '((:sym mine :url "https://github.com/miketz/highlight-indent-guides"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/DarthFennec/highlight-indent-guides"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'icicles
      :comment nil
      :folder (concat my-module-folder "icicles")
      :remotes '((:sym mine :url "https://github.com/miketz/icicles"
                       :alias "origin")
                 ;; this is actually a mirror. The real upstream is emacs wiki.
                 (:sym upstream :url "https://github.com/emacsmirror/icicles"
                       :alias "mirror"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      ;; NOTE: not an elisp package. documentation package for common lisp from
      ;; NOTE: Cannot modify this package due to license. Must be kept in it's full unmodified form.
      :name 'hyperspec
      :comment nil
      :folder (concat my-module-folder "hyperspec")
      :remotes '((:sym mine :url "https://github.com/miketz/hyperspec"
                       :alias "origin"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'flames-of-freedom
      :comment nil
      :folder (concat my-module-folder "FlamesOfFreedom")
      :remotes '((:sym upstream :url "https://github.com/wiz21b/FlamesOfFreedom" ; just a fun package so no fork.
                           :alias "origin"))
      :remote-default 'upstream
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "25.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module ;; not using this. delete git submodule later
      :name 'indium
      :comment nil
      :folder (concat my-module-folder "Indium")
      :remotes '((:sym upstream :url "https://github.com/NicolasPetton/Indium"
                           :alias "origin"))
      :remote-default 'upstream
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "25")
                     (seq "2.16")
                     (js2-mode "20140114")
                     (js2-refactor "0.9.0")
                     (company "0.9.0")
                     (json-process-client "0.2.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'posframe
      :comment nil
      :folder (concat my-module-folder "posframe")
      :remotes '((:sym mine :url "https://github.com/miketz/posframe"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/tumashu/posframe"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "26.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'ivy-posframe
      :comment nil
      :folder (concat my-module-folder "ivy-posframe")
      :remotes '((:sym mine :url "https://github.com/miketz/ivy-posframe"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/tumashu/ivy-posframe"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '((emacs "26.0") (posframe "1.0.0") (ivy "0.13.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'autothemer
      :comment nil
      :folder (concat my-module-folder "autothemer")
      :remotes '((:sym mine :url "https://github.com/miketz/autothemer"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/jasonm23/autothemer"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '((dash "2.10.0") (emacs "26.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'company
      :comment nil
      :folder (concat my-module-folder "company-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/company-mode"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/company-mode/company-mode"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "24.3"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'company-web
      :comment nil
      :folder (concat my-module-folder "company-web")
      :remotes '((:sym mine :url "https://github.com/miketz/company-web"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/osv/company-web"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '((company "0.8.0") (dash "2.8.0") (cl-lib "0.5.0")
                     (web-completion-data "0.1.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'company-lsp
      :comment nil
      :folder (concat my-module-folder "company-lsp")
      :remotes '((:sym mine :url "https://github.com/miketz/company-lsp"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/tigersoldier/company-lsp"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "25.1") (lsp-mode "6.0")
                     (company "0.9.0") (s "1.2.0") (dash "2.11.0"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'web-completion-data
      :comment nil
      :folder (concat my-module-folder "web-completion-data")
      :remotes '((:sym mine :url "https://github.com/miketz/web-completion-data"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/osv/web-completion-data"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'visual-indentation-mode
      :comment "Alternative to highlight-indent-guides."
      :folder my-module-folder
      :file-single "visual-indentation-mode.el"
      :remotes '((:sym upstream :url "https://github.com/skeeto/visual-indentation-mode"
                           :alias "upstream"))
      :remote-default 'upstream
      :source-control 'git
      :submodule-p nil
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'exec-path-from-shell
      :comment "Set environment vars on mac."
      :folder my-module-folder
      :file-single "exec-path-from-shell.el"
      :remotes '((:sym upstream :url "https://github.com/purcell/exec-path-from-shell"
                           :alias "upstream"))
      :remote-default 'upstream
      :source-control 'git
      :submodule-p nil
      :main-branch nil
      :use-branch nil
      :depend-hard '((emacs "24.1") (cl-lib "0.6"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'vc-fossil
      :comment nil
      :folder my-module-folder
      :file-single "vc-fossil.el"
                                     ;; new upstream referenced in mirror readme
      :remotes '((:sym upstream :url "https://tumbleweed.nu/r/vc-fossil/doc/tip/README.md"
                           :alias "upstream"
                           :type fossil?)
                 (:sym upstream-old :url "https://chiselapp.com/user/venks/repository/emacs-fossil"
                           :alias "upstream-old"
                           :type fossil?)
                 (:sym mirror :url "https://github.com/venks1/emacs-fossil/"
                           :alias "mirror"))
      :remote-default 'mirror
      :source-control 'git
      :submodule-p nil
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'lusty-explorer
      :comment nil
      :folder my-module-folder
      :file-single "lusty-explorer.el"
      :remotes '((:sym upstream :url "https://github.com/sjbach/lusty-emacs"
                           :alias "upstream"))
      :remote-default 'upstream
      :source-control 'git
      :submodule-p nil
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module ;; TODO: fork this and make it a submodule because I already made a modification.
      :name 'feebleline
      :comment nil
      :folder my-module-folder
      :file-single "feebleline.el"
      :remotes '((:sym upstream :url "https://github.com/tautologyclub/feebleline"
                           :alias "upstream"))
      :remote-default 'upstream
      :source-control 'git
      :submodule-p nil
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'fennel-mode
      :comment nil
      :folder (concat my-module-folder "fennel-mode")
      :remotes '((:sym mine :url "https://github.com/miketz/fennel-mode"
                       :alias "origin")
                 (:sym upstream :url "https://git.sr.ht/~technomancy/fennel-mode"
                           :alias "upstream")
                 ;; used this mirror for my "mine" fork on github. But
                 ;; completely unused.
                 (:sym mirror :url "https://github.com/emacsmirror/fennel-mode"
                         :alias "mirror"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "main"
      :use-branch "main"
      :depend-hard '((emacs "26.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'vertico
      :comment nil
      :folder (concat my-module-folder "vertico")
      :remotes '((:sym mine :url "https://github.com/miketz/vertico"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/minad/vertico"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "main"
      :use-branch "main"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'consult
      :comment "Specialized completion modes. Similar to counsel in the swiper/ivy package. But used with vertico."
      :folder (concat my-module-folder "consult")
      :remotes '((:sym mine :url "https://github.com/miketz/consult"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/minad/consult"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "main"
      :use-branch "main"
      :depend-hard '((emacs "27.1") (compat "29.1.3.4"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'bug-hunter
      :comment nil
      :folder (concat my-module-folder "elisp-bug-hunter")
      :remotes '((:sym mine :url "https://github.com/miketz/elisp-bug-hunter"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/Malabarba/elisp-bug-hunter"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'typescript-mode
      :comment nil
      :folder (concat my-module-folder "typescript.el")
      :remotes '((:sym mine :url "https://github.com/miketz/typescript.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/emacs-typescript/typescript.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'tide
      :comment nil
      :folder (concat my-module-folder "tide")
      :remotes '((:sym mine :url "https://github.com/miketz/tide"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/ananthakumaran/tide"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
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
      :remotes '((:sym mine :url "https://github.com/miketz/compat.el"
                       :alias "origin")
                 ;; this is really a mirror, the the root upstream
                 (:sym upstream :url "https://github.com/phikal/compat.el"
                         :alias "mirror"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "main"
      :use-branch "main"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'slime-volleyball
      :comment nil
      :folder (concat my-module-folder "slime-volleyball")
      :remotes '((:sym mine :url "https://github.com/miketz/slime-volleyball"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/fitzsim/slime-volleyball"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "mine" ;; ignores .elc files
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'macrostep
      :comment nil
      :folder (concat my-module-folder "macrostep")
      :remotes '((:sym mine :url "https://github.com/miketz/macrostep"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/emacsorphanage/macrostep"
                           :alias "upstream")
                 (:sym upstreamOrig :url "https://github.com/joddie/macrostep"
                           :alias "upstreamOrig"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'sx
      :comment nil
      :folder (concat my-module-folder "sx.el")
      :remotes '((:sym mine :url "https://github.com/miketz/sx.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/vermiculus/sx.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '((emacs "24.1")
                     (cl-lib "0.5")
                     (json "1.3")
                     (markdown-mode "2.0")
                     (let-alist "1.0.3"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'indent-bars
      :comment nil
      :folder (concat my-module-folder "indent-bars")
      :remotes '((:sym mine :url "https://github.com/miketz/indent-bars"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/jdtsmith/indent-bars"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "main"
      :use-branch "main"
      :depend-hard '((emacs "27.1")
                     (compat "29.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'devdocs
      :comment nil
      :folder (concat my-module-folder "devdocs.el")
      :remotes '((:sym mine :url "https://github.com/miketz/devdocs.el"
                       :alias "origin")
                 (:sym upstream :url "https://github.com/astoff/devdocs.el"
                           :alias "upstream"))
      :remote-default 'mine
      :source-control 'git
      :submodule-p t
      :main-branch "main"
      :use-branch "main"
      :depend-hard '((emacs "27.1"))
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'sunrise
      :comment nil
      :folder (concat my-module-folder "sunrise-commander")
      :remotes '((:sym upstream :url "https://github.com/sunrise-commander/sunrise-commander"
                           :alias "upstream"))
      :remote-default 'upstream
      :source-control 'git
      :submodule-p t
      :main-branch "master"
      :use-branch "master"
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'jsonian
      :comment nil
      :folder my-module-folder
      :file-single "jsonian.el"
      :remotes '((:sym upstream :url "https://github.com/iwahbe/jsonian"
                           :alias "upstream"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'highlight-tail
      :comment nil
      :folder my-module-folder
      :file-single "highlight-tail.el"
      :remotes '((:sym upstream :url "https://www.emacswiki.org/emacs/highlight-tail.el"
                           :alias "upstream")
                 (:sym mirror :url "https://github.com/ahungry/emacswiki-mirror/blob/master/highlight-tail.el"
                         :alias "mirror"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'key-chord
      :comment nil
      :folder my-module-folder
      :file-single "key-chord.el"
      :remotes '((:sym wiki :url "https://www.emacswiki.org/emacs/KeyChord"
                       :alias "wiki")
                 ;; orphanage is used by MELPA
                 (:sym upstream :url "https://github.com/emacsorphanage/key-chord"
                       :alias "upstream")
                 (:sym mirror :url "https://github.com/emacsmirror/emacswiki.org/blob/master/key-chord.el"
                         :alias "mirror"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      :name 'profile-dotemacs
      :comment nil
      :folder my-module-folder
      :file-single "profile-dotemacs.el"
      :remotes '((:sym wiki :url "https://www.emacswiki.org/emacs/ProfileDotEmacs"
                       :alias "wiki"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())
    ,(make-module
      ;; NOTE: equivalent functionality is part of core Emacs starting with
      ;; version 24.4. Only using this on older Emacs versions.
      :name 'dired-details
      :comment nil
      :folder my-module-folder
      :file-single "dired-details.el"
      :remotes '((:sym wiki :url "https://www.emacswiki.org/emacs/DiredDetails"
                       :alias "wiki"))
      :remote-default nil
      :source-control 'git
      :submodule-p nil ;; embedded file into my .emacs.d
      :main-branch nil
      :use-branch nil
      :depend-hard '()
      :depend-soft '()
      :depend-bundled '())))

;; TODO: make jinx spell checker a module

(defun my-byte-compile-all-modules ()
  "Byte compile .el files of all modules."
  (interactive)

  ;; first compile git submodules. They have a dedicated folder so it makes
  ;; sense to delete all the elc files in it.
  (cl-loop for mod in (my-get-all-git-submodules)
           do
           (ignore-errors ;; don't stop if 1 package is bad
             (my-delete-elc-files (module-folder mod))

             (byte-recompile-directory
              (module-folder mod)
              0 ;; 0 means compile .el files if .elc is missing.
              t) ;; t means force re-compile even if the .elc is up-to-date. May
             ;; be useful if the Emacs version changed and should have an
             ;; .elc compiled again to be compatible.
             ))

  ;; now compile the "single file" modules.
  (cl-loop for mod in (my-get-all-single-file-modules)
           do
           (ignore-errors ;; don't stop if 1 package is bad
             (let ((file (concat (module-folder mod)
                                 (module-file-single mod))))
               (byte-compile-file file))))
  'done)

;; TODO: take module as fn arg or interactively entered by user. And correctly
;;       handle single file packages. comment out until fixed.
;; (defun my-byte-compile-module ()
;;   "Byte compile .el files for the selected module."
;;   (interactive)
;;   (let* ((mod-name (intern (completing-read "module: "
;;                                             (mapcar (lambda (m)
;;                                                       (module-name m))
;;                                                     my-modules))))
;;          (mod (car (cl-member mod-name
;;                               my-modules
;;                               :test (lambda (sym mod)
;;                                       (eq sym (module-name mod)))))))
;;     (my-delete-elc-files (module-folder mod))

;;     (byte-recompile-directory
;;      (module-folder mod)
;;      0 ;; 0 means compile .el files if .elc is missing.
;;      t)))


(defun my-folder-p (f)
  "Quick function to determine if F is a folder.
Where F is from fn `directory-files-and-attributes'."
  (cl-second f))




(defun my-get-module-by-symbol (mod-symbol)
  "Return module by MOD-SYMBOL.
This is usually the symbol the package uses for (provide) and (require)."
  (cl-find mod-symbol my-modules :test (lambda (sym mod)
                                         (eq (module-name mod) sym))))

(defun my-get-remote (mod remote-sym)
  "For module struct MOD, return the git remote info via REMOTE-SYM.
A module can have multiple remotes.
REMOTE-SYM will most often be `mine' or `upstream' by convention."
  (let* ((remotes (module-remotes mod)))
    (cl-find remote-sym
             remotes
             :test (lambda (sym rem)
                     (eq (cl-getf rem :sym) sym)))))

;; NOTE: now that remotes are using plists there may not be much value in this
;; "info-extraction" fn as plists are already easy to extract info from.
(defun my-get-remote-info (remote)
  "For REMOTE extract the git remote info.
As a list of strings of the form (URL GIT-ALIAS)."
  (let ((url (cl-getf remote :url))
        (alias (cl-getf remote :alias)))
    `(,url ,alias)))

(cl-defun my-git-remote-setup-p (mod remote-sym)
  "Return T if the git remote for MOD is wired up on the git side.
This means a matching alias and url.
REMOTE-SYM will usually be `mine' or `upstream'."
  ;; GUARD: must pass in a good module
  (when (null mod)
    (cl-return-from my-git-remote-setup-p nil))

  (let* ((remote (my-get-remote mod remote-sym)))
    ;; GUARD: remote must be configured in `my-modules'
    (when (null remote)
      (cl-return-from my-git-remote-setup-p nil))

    (let* ((url (cl-getf remote :url))
           (alias (cl-getf remote :alias))
           ;; important that we shadow `default-directory' so the git command
           ;; runs against the correct git folder
           (output (let ((default-directory (module-folder mod)))
                     (shell-command-to-string "git remote"))))
      ;; GUARD: if 0 remotes from "git remote" command return nil
      (when (or (null output)
                (= 0 (length (s-trim output))))
        (cl-return-from my-git-remote-setup-p nil))

      ;; split the raw shell output to a list of alias strings
      (let* ((git-remote-aliases (s-split-words (s-trim output))))
        ;; GUARD: the alias we are looking for must be in the git output
        (when (null (cl-find alias git-remote-aliases :test #'string-equal))
          (cl-return-from my-git-remote-setup-p nil))

        ;; finally, check the url for this alias.
        (let* ((output-url (let ((default-directory (module-folder mod)))
                             (shell-command-to-string (concat "git remote get-url " alias)))))
          (string-equal url (s-trim output-url)))))))

(cl-defun my-git-remote-create (mod remote-sym)
  "For MOD, create the configured REMOTE-SYM as a remote on the git side."
  ;; GUARD: mod must be provided
  (when (null mod)
    (cl-return-from my-git-remote-create nil))

  (let* ((remote (my-get-remote mod remote-sym)))
    ;; GUARD: remote-sym must be configured in `my-modules'
    (when (null remote)
      (cl-return-from my-git-remote-create 'remote-not-configured-in-my-modules))

    ;; GUARD: don't create the remote if it's already setup
    (when (my-git-remote-setup-p mod remote-sym)
      (cl-return-from my-git-remote-create 'already-created))

    ;; OK, now it's safe to create the remote.
    (let* ((default-directory (module-folder mod))
           (remote (my-get-remote mod remote-sym))
           ;; creating the remote here
           (shell-output (shell-command-to-string (concat "git remote add "
                                                          (cl-getf remote :alias) " "
                                                          (cl-getf remote :url)))))
      ;; TODO: find a better way of detecting error. They could change the error message to
      ;; not start with "error" and that would break this code.
      (if (s-starts-with-p "error" shell-output)
          ;; just return the error msg itself. This string is inconsistent with
          ;; the symbol return types, but it should be OK as it's just a report
          ;; of what happened. No real processing on it.
          (s-trim shell-output)
          ;; else SUCCESS
          'remote-created))))

(defun my-get-all-git-submodules ()
  "Return the subset of `my-modules' that are git submodules.
Some operations only make sense for git submodules."
  (cl-remove-if (lambda (m)
                  (not (module-submodule-p m)))
                my-modules))

(defun my-get-all-single-file-modules ()
  "Return the subset of `my-modules' that are single files.
Embedded into my .emacs.d/ config.  Not git submodules.
Some operations only make sense for these single-file packages."
  (cl-remove-if (lambda (m)
                  (or (module-submodule-p m)
                      (null (module-file-single m))))
                my-modules))

(defun my-setup-all-upstream-remotes-if-missing ()
  "For all git submodules set the upstream remote if it is missing."
  (interactive)
  (let ((git-submodules (my-get-all-git-submodules))
        (statuses '()))
    (cl-loop for m in git-submodules
             do
             ;; by convention the upstream remote is called "upstream".
             ;; when we first clone from git it is from my fork. Git does not track remotes, so the
             ;; upstream remotes are "lost". We are creating them now from the info in `my-modules'.
             (let ((result (my-git-remote-create m 'upstream)))
               ;; track the results
               (push `(,(module-name m) ,result) statuses)))
    ;; return the results for informational purposes.
    statuses))


(defun my--create-buff-gitFetchHelper ()
  (let ((buff (get-buffer-create "*gitFetchHelper*")))
    (with-current-buffer buff
      ;; turn on sh-mode for # comment support.
      (sh-mode))
    buff))


(defun my--branch-checkout-complete (p msg)
  (when (memq (process-status p) '(exit signal))
    ;;(message (concat (process-name p) " - " msg))
    (let ((buff (process-buffer p)))
      (unless (eq buff (current-buffer))
        (switch-to-buffer-other-window buff))
      (goto-char (point-max)) ;; end of buffer
      ;; (insert output-str) ;; this is done already by `start-process-shell-command'.
      (insert "\n--------------------------\n"))
    (message "branch switching complete")))

(defun my-checkout-branches-golang ()
  "Call an external Go program to checkout the UseBranch for each git submodule.
Calls git commands concurrently for each git submodule.

Assumes go build has been run on ~/.emacs.d/notElpa/gitFetchHelper.

Git does not keep track of multiple remotes so I track this in `my-modules' and
also in gitFetchHelper."
  (interactive)
  (let* ((cmd (concat (expand-file-name "~/.emacs.d/notElpa/gitFetchHelper/gitFetchHelper")
                      " init2"))
         (buff (my--create-buff-gitFetchHelper))
         ;; shadow so repos.json can be found
         (default-directory "~/.emacs.d/notElpa/gitFetchHelper"))
    (message "checking out target branches...")
    ;; use process to avoid freezing emacs.
    (set-process-sentinel (start-process-shell-command "gitFetchHelper" buff cmd)
                          #'my--branch-checkout-complete)))

(defun my--remote-complete (p msg)
  (when (memq (process-status p) '(exit signal))
    ;;(message (concat (process-name p) " - " msg))
    (let ((buff (process-buffer p)))
      (unless (eq buff (current-buffer))
        (switch-to-buffer-other-window buff))
      (goto-char (point-max)) ;; end of buffer
      ;; (insert output-str) ;; this is done already by `start-process-shell-command'.
      (insert "\n--------------------------\n"))
    (message "upstream remote setup complete")))


(defun my-setup-all-upstream-remotes-if-missing-golang ()
  "Call an external Go program to set upstream remotes.
Calls git commands concurrently for each git submodule.

Assumes go build has been run on ~/.emacs.d/notElpa/gitFetchHelper.

Git does not keep track of multiple remotes so I track this in `my-modules' and
also in gitFetchHelper."
  (interactive)
  (let* ((cmd (concat (expand-file-name "~/.emacs.d/notElpa/gitFetchHelper/gitFetchHelper")
                      " init"))
         (buff (my--create-buff-gitFetchHelper))
         ;; shadow so repos.json can be found
         (default-directory "~/.emacs.d/notElpa/gitFetchHelper"))
    (message "setting up missing upstream remotes...")
    ;; use process to avoid freezing emacs.
    (set-process-sentinel (start-process-shell-command "gitFetchHelper" buff cmd)
                          #'my--remote-complete)))


(defun my--fetch-complete (p msg)
  (when (memq (process-status p) '(exit signal))
    ;;(message (concat (process-name p) " - " msg))
    (let ((buff (process-buffer p)))
      (unless (eq buff (current-buffer))
        (switch-to-buffer-other-window buff))
      (goto-char (point-max)) ;; end of buffer
      ;; (insert output-str) ;; this is done already by `start-process-shell-command'.
      (insert "\n--------------------------\n"))
    (message "fetch complete")))

(defun my-fetch-all-upstream-remotes-golang ()
  "Call an external Go program to fetch each upstream remote.
Concurrently fetches all upstream remotes at once for increased speed.

Assumes go build has been run on ~/.emacs.d/notElpa/gitFetchHelper.

Assumes fn `my-setup-all-upstream-remotes-if-missing' has been called
to set upstream remotes. Git does not keep track of multiple remotes
so I track this in `my-modules'."
  (interactive)
  ;; (let* (;; run the Go program
  ;;        (cmd (concat (expand-file-name "~/.emacs.d/notElpa/gitFetchHelper/gitFetchHelper")
  ;;                     " fetch"))
  ;;        (output-str (shell-command-to-string cmd))
  ;;        (buff (get-buffer-create "*gitFetchHelper*")))

  ;;   (unless (eq buff (current-buffer))
  ;;     (switch-to-buffer-other-window buff))
  ;;   (goto-char (point-max)) ;; end of buffer
  ;;   (insert output-str)
  ;;   (insert "\n--------------------------\n"))
  (let* ((cmd (concat (expand-file-name "~/.emacs.d/notElpa/gitFetchHelper/gitFetchHelper")
                      " fetch"))
         (buff (my--create-buff-gitFetchHelper))
         ;; shadow so repos.json can be found
         (default-directory "~/.emacs.d/notElpa/gitFetchHelper"))
    (message "fetching each submodule...")
    ;; use process to avoid freezing emacs.
    (set-process-sentinel (start-process-shell-command "gitFetchHelper" buff cmd)
                          #'my--fetch-complete)))

(defun my-fetch-all-upstream-remotes ()
  "Run git fetch for each upstream remote.
Collect status info for each so I'll know which to merge.
Merge will be done manually after this."
  (interactive)
  (let ((git-submodules (my-get-all-git-submodules))
        (statuses '()))
    (cl-loop for m in git-submodules
             do
             (cl-block 'loop
               (let ((rem (my-get-remote m 'upstream)))
                 ;; GUARD: upstream must be configured in `my-modules'
                 (when (null rem)
                   (push `(,(module-name m) 'upstream-not-configured-in-my-modules) statuses)
                   (cl-return-from 'loop)) ;; continue

                 ;; GUARD: don't attempt a fetch if the upstream remote is not set up on the git side
                 (when (not (my-git-remote-setup-p m 'upstream))
                   (push `(,(module-name m) 'upstream-remote-not-created-on-git-side) statuses)
                   (cl-return-from 'loop)) ;; continue

                 ;; fetch. TODO: look into using `async-shell-command' or`start-process' instead of `shell-command-to-string'. for async.
                 (let* ((default-directory (module-folder m))
                        ;; actual git fetch run is here
                        (shell-output (s-trim (shell-command-to-string
                                               (concat "git fetch " (cl-getf rem :alias))))))
                   ;; TODO: find a better way of detecting error. They could change the error message to
                   ;; not start with "error" and that would break this code.
                   (if (s-starts-with-p "error" shell-output)
                       ;; just return the error msg itself. This string is inconsistent with
                       ;; the symbol return types, but it should be OK as it's just a report
                       ;; of what happened. No real processing on it.
                       (push `(,(module-name m) shell-output) statuses)
                     ;; else SUCCESS
                     (if (= (length shell-output) 0)
                         ;; No new code fetched. Although this doesn't mean there isn't upstream code that
                         ;; still needs to be merged into the local branch from a previous fetch.
                         ;; It just means this particular fetch did not download any new code.
                         ;; for now don't push into the report as it spams it up.
                         'no-op-do-nothing ;;(push `(,(module-name m) 'fetch-success-no-new-code) statuses)
                       ;; New code fetched. Although it may only be new code in a branch we are not interested in.
                       (push `(,(module-name m) 'fetch-success-new-code) statuses)))))))
    ;; return the results for informational purposes.
    statuses))

(defun my--list-merges-complete (p msg)
  (when (memq (process-status p) '(exit signal))
    ;;(message (concat (process-name p) " - " msg))
    (let ((buff (process-buffer p)))
      (unless (eq buff (current-buffer))
        (switch-to-buffer-other-window buff))
      (goto-char (point-max)) ;; end of buffer
      ;; (insert output-str) ;; this is done already by `start-process-shell-command'.
      (insert "\n--------------------------\n"))
    (message "merge detection complete")))

(defun my-list-modules-with-upstream-code-to-merge-golang ()
  "Call an external Go program to see which modules have new code for review/merging.
Calls git diff concurrently.

Assumes go build has been run on ~/.emacs.d/notElpa/gitFetchHelper.

Assumes fn `my-setup-all-upstream-remotes-if-missing' has been called
to set upstream remotes. Git does not keep track of multiple remotes
so I track this in `my-modules'."
  (interactive)
  (let* ((cmd (concat (expand-file-name "~/.emacs.d/notElpa/gitFetchHelper/gitFetchHelper")
                      " diff"))
         (buff (my--create-buff-gitFetchHelper))
         ;; shadow so repos.json can be found
         (default-directory "~/.emacs.d/notElpa/gitFetchHelper"))
    (message "finding submodules with new upstream code to merge...")
    ;; use process to avoid freezing emacs.
    (set-process-sentinel (start-process-shell-command "gitFetchHelper" buff cmd)
                          #'my--list-merges-complete)))

(defun my-list-modules-with-upstream-code-to-merge ()
  "List modules with new upstream code not yet merged into the local branch.
This does not actually fetch, only looks at the local contents on on the disk.
So you may want to run `my-fetch-all-upstream-remotes' first to fetch the
latest upstream code.

NOTE: This fn only works properly when the module's `use-branch' is checked out.
When you first pull the git submodules they are in a detached-head state with
no branch checked out and you will get false results."
  (interactive)
  (let ((git-submodules (my-get-all-git-submodules))
        (statuses '()))
    (cl-loop for m in git-submodules
             do
             (cl-block 'loop
               (let ((rem (my-get-remote m 'upstream)))
                 ;; GUARD: upstream must be configured in `my-modules'
                 (when (null rem)
                   (push `(,(module-name m) 'upstream-not-configured-in-my-modules) statuses)
                   (cl-return-from 'loop)) ;; continue

                 ;; GUARD: don't attempt a diff if the upstream remote is not set up on the git side
                 (when (not (my-git-remote-setup-p m 'upstream))
                   (push `(,(module-name m) 'upstream-remote-not-created-on-git-side) statuses)
                   (cl-return-from 'loop)) ;; continue

                 ;; run a diff so we know if there is code to merge in.
                 (let* ((default-directory (module-folder m))
                        ;; TODO: handle case where I use private branch "mine" with irrelevant
                        ;; changes (like .gitignore). This causes the diff to always hit.
                        ;; For now just compare the local "main" branch to the upstream "main".
                        ;; This will work as long as keep both the "main" and "mine" branches
                        ;; updated.
                        (branch-main (module-main-branch m))
                        (branch-i-use (module-use-branch m))
                        (cmd (concat "git diff "
                                     ;; branch-i-use " " ;; TODO: incorporate branch-i-use
                                     branch-main " "
                                     (cl-getf rem :alias) "/" branch-main))
                        ;; actual git diff run is here
                        (shell-output (s-trim (shell-command-to-string cmd))))
                   ;; TODO: find a better way of detecting error. They could change the error message to
                   ;; not start with "error" and that would break this code.
                   (if (or (s-starts-with-p "error" shell-output)
                           (s-starts-with-p "fatal" shell-output))
                       ;; just return the error msg itself. This string is inconsistent with
                       ;; the symbol return types, but it should be OK as it's just a report
                       ;; of what happened. No real processing on it.
                       (push `(,(module-name m) ,shell-output) statuses)
                     ;; else SUCCESSFUL diff
                     (when (> (length shell-output) 0)
                       ;; changes detected! but it may be code in a branch we don't use for merging.
                       (push `(,(module-name m) 'new-code-in-upstream ,cmd) statuses)))))))
    ;; return the results for informational purposes.
    statuses))

(defun my-byte-compile-all-notElpa-folders ()
  "Byte compile .el files in every folder under /notElpa."
  (interactive)
  (let* ((dir-infos (cl-remove-if
                     (lambda (f)
                       (or (not (my-folder-p f)) ;; skip individual files
                           ;; skip themes
                           (s-ends-with-p "themes" (cl-first f))
                           ;; Skip specific projects that don't ignore .elc files.
                           ;; Revisit this after I fork the projects, and use a personal branch.
                           ;; (s-ends-with-p "sunrise-commander" (cl-first f))
                           (s-ends-with-p "FlamesOfFreedom" (cl-first f))
                           ;; (s-ends-with-p "markup-faces" (cl-first f))
                           ;; (s-ends-with-p "sicp-info" (cl-first f))
                           ;; (s-ends-with-p "sallet" (cl-first f))
                           ;; (s-ends-with-p "libegit2" (cl-first f))
                           ))
                     (directory-files-and-attributes my-module-folder
                                                     t "^[^.]" t)))
         (dir-names (mapcar #'cl-first dir-infos))
         (statuses '()))
    (cl-loop for dir in dir-names
             do
             (if (eq 'success
                     (ignore-errors ;; don't stop if 1 package is bad
                       (my-delete-elc-files dir)
                       (byte-recompile-directory
                        dir
                        0 ;; 0 means compile .el files if .elc is missing.
                        t) ;; t means force re-compile even if the .elc is up-to-date. May
                       ;; be useful if the Emacs version changed and should have an
                       ;; .elc compiled again to be compatible.
                       'success))
                 ;; NOTE: this doesn't mean byte compilation was successful for all files
                 ;; just that no elisp-level errors were thrown.
                 (push `(,dir success) statuses)
               ;; else exception during byte compilation
               (push `(,dir error) statuses)))
    ;; return the results for informational purposes.
    statuses))


(defun my-byte-compile-all-notElpa ()
  "Byte compile all .el files in ~/.emacs.d/notElpa and sub dirs.
Does inlcude individual files.
Skip themes.
Possibly skip some packages that don't like to be byte compiled."
  (interactive)
  (let* ((dir-infos (cl-remove-if
                     (lambda (f)
                       (or
                        ;; skip non ".el" files in top level notElpa/ folder
                        (and (not (my-folder-p f))
                             (not (string-suffix-p ".el" (cl-first f) t)))
                        ;; skip themes
                        (s-ends-with-p "themes" (cl-first f))
                        ;; Skip specific projects that don't ignore .elc files.
                        ;; Revisit this after I fork the projects, and use a personal branch.
                        (s-ends-with-p "FlamesOfFreedom" (cl-first f))))
                     ;; files and folders
                     (directory-files-and-attributes my-module-folder
                                                     t "^[^.]" t)))
         (statuses '()))
    (cl-loop for obj in dir-infos
             do
             (if (my-folder-p obj)
                 ;; compile folder
                 (let ((dir (cl-first obj)))
                   (if (eq 'success
                           (ignore-errors ;; don't stop if 1 package is bad
                             (my-delete-elc-files dir)
                             (byte-recompile-directory
                              dir
                              0 ;; 0 means compile .el files if .elc is missing.
                              t) ;; t means force re-compile even if the .elc is up-to-date. May
                             ;; be useful if the Emacs version changed and should have an
                             ;; .elc compiled again to be compatible.
                             'success))
                       ;; NOTE: this doesn't mean byte compilation was successful for all files
                       ;; just that no elisp-level errors were thrown.
                       (push `(,dir success) statuses)
                     ;; else exception during byte compilation
                     (push `(,dir error) statuses)))
               ;; else compile single file
               (let ((file (cl-first obj)))
                 (if (byte-compile-file file) ; returns t if succesful. keeps going on err
                     (push `(,file success) statuses)
                   ;; else exception during byte compilation
                   (push `(,file error) statuses)))))
    ;; return the results for informational purposes.
    statuses))

(defun my--scrape-module-info ()
  "This is is for dev-time use only.
Generates a skeleton list for `my-modules'.  With possibly incorrect and
incomplete info about the modules.
Saves me from typing a lot of module stuff."
  (let* ((dir-infos (cl-remove-if
                     (lambda (f)
                       (or (not (my-folder-p f)) ;; skip individual files
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

(defun my-gen-go-code-for-submodules ()
  "Helper fn to generate Golang code.
Saving to a function for reference purposes.
Was doing an experiment of go to concurrently fetch the latest upstream code
for each submodule."
  (let ((git-submodules (my-get-all-git-submodules))
        (lines '()))
    (cl-loop for m in git-submodules
             do
             (insert (format "{Folder: \"%s\", UpstreamAlias: \"%s\"},\n"
                             (module-folder m)
                             (cl-getf (my-get-remote m 'upstream) :alias))))))

(provide 'my-modules)

;;; my-modules.el ends here
