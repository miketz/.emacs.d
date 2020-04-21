;;; old code that used to be in init.el
;;; converted away from packages to git submodules so this is not currently
;;; used. Storing code here for reference purposes.

(require 'package)

;; TODO: specify if it should use elpa or melpa version of a package.
;; NOTE: to limit package installation to specific computers (or other
;; conditions), the second place in each list item is a true/false value.
(defvar my-packages
  `((s nil) ;; string library. using git submodule
    (evil nil) ;; using git submodule
    (evil-leader nil) ;; using git submodule
    (pos-tip nil) ;; for pop up on eval with leader "e". git submodule
    (key-chord nil) ;; using file in /notElpa/
    (slime nil ;; using git submodule
           ;;,my-install-slime-p
     )
    ;; (paredit t) ; using git submodule instead of melpa package
    ;;paxedit
    ;;smartparens
    ;;redshank
    ;;auto-complete
    ;;ac-slime
    (company nil) ;; using git submodule
    (company-web nil) ;; using git submodule
    (slime-company nil ;; using git submodule
                   ;;,my-install-slime-company-p
     )
    (ace-window nil) ;; using git submodule
    (csharp-mode nil) ;; using git submodule
    (js2-mode nil) ;; using git submodule
    (js2-highlight-vars nil ;; using git submodule
                        ;;,(not (version< emacs-version "24.4"))
     )
    (skewer-mode ,(memq my-curr-computer '(work-laptop)))
    (json-mode nil) ;; using git submodule

    (helm ,my-use-helm-p)
    (helm-cmd-t nil) ;; broken with recent helm versions?
    (helm-swoop ,my-use-helm-p)
    (helm-w32-launcher ,(and my-use-helm-p
                             (eq my-curr-computer 'work-laptop)))
    ;;helm-git-grep ;search text of files.
    ;;helm-ls-git ;search for files. Similar to helm-cmd-t but with git.
    ;;helm-flycheck
    ;;helm-descbinds

    ;; NOTE: icicles removed from melpa due to emacs wiki hosting.
    ;; (icicles ,(eq my-narrow-type 'icicles))

    ;;projectile
    ;;clippy
    ;;yasnippet
    (rainbow-delimiters nil) ;; using git submodule
    (rainbow-mode nil) ;; using git submodule
    (expand-region nil) ;; using git submodule
    ;;multiple-cursors
    ;;(omnisharp (work-laptop))
    ;;sublimity

    (nyan-mode nil) ;; using git submodule
    ;;(nyan-prompt t) ; nyan-prompt removed from melpa?

    ;;powerline
    ;;dired-details ;default feature in emacs 24.4+
    (web-mode nil) ;; using git submodule
    ;; (htmlize t)
    (magit nil ;; using git submodule
           ;;,(not (version< emacs-version "25.1"))
     )
    (vimrc-mode nil) ;; using git submodule
    (sicp nil) ;; using git submodule
    ;;neotree
    (num3-mode nil) ;; using git submodule
    (powershell nil) ;; using git submodule
    (irony ,my-has-clang-p)
    (company-irony ,my-has-clang-p)
    ;; (flycheck-irony ,my-has-clang-p)
    ;;(rtags)
    ;;aggressive-indent
    (sx nil ;; using git submodule
        ;;,(not (version< emacs-version "24.4"))
     )
    (leerzeichen nil) ;; using git submodule
    (darkroom nil) ;; using git submodule
    ;;vim-empty-lines-mode
    ;; (fill-column-indicator ,(not (version< emacs-version "25")))
    (flycheck nil) ;; using git submodule
    (hydra nil) ;; using git submodule
    ;;linum-relative
    ;;(guide-key)
    (unkillable-scratch nil) ;; using git submodule
    ;; (speed-type t)
    (bug-hunter nil) ;; using git submodule

    ;; NOTE: using git submodule instead of package for ivy, swiper, counsel.
    (ivy nil)
    (swiper nil)
    (counsel nil)
    (flx nil) ;; using git submodule.
              ;; flx can be used by ivy and flx-ido for ordering flx matches.

    ;;color-identifiers-mode
    ;;svg-mode-line-themes ;; only works on gnu/linux
    (avy nil) ;; using git submodule
    (lispy nil) ;; using git submodule for lispy.
    ;;(worf t)
    (elisp-slime-nav nil) ;; using git submodule

    ;; on 11-28-2016 electric-spacing had an unbalanced paren. seems to be
    ;; fixed now. Using it again.
    ;; weird spacing rules introduced. stop using again.
    (electric-spacing nil)

    ;;w3
    ;;w3m
    ;;flymake-jslint
    ;; (nlinum ,(not native-line-numbers-p))
    ;; (nlinum-relative ,(not native-line-numbers-p))

    (ido-vertical-mode ,my-use-ido-p)
    (ido-grid-mode ,my-use-ido-p)
    (ido-ubiquitous ,my-use-ido-p)
    (flx-ido nil ;; TODO: move this into a git submodule.
             ;;,my-use-ido-p
     )
    ;; (ido-occur ,my-use-ido-p)
    (smex nil ;; using git submodule

          ;; ,(or my-use-ido-p
          ;;      my-use-bare-ido-p
          ;;      my-use-ivy-p ;; smex can be used by `counsel-M-x'
          ;;      my-use-mish-mash-p)
          )
    ;; (ov nil) ;; ov is no longer a needed dependency? keep it as a comment
    ;; because may useful for my own purposes later.
    (highlight-tail nil) ;; removed from melpa (emacs wiki purge?)
    (function-args nil)
    (highlight-indent-guides nil) ;; using git submodule
    (ace-link nil) ;; using git submodule
    (smart-tabs-mode nil) ;; using git submodule
    (lua-mode nil) ;; using git submodule

    ;; (ggtags ,(let ((has-gnu-global-p (memq my-curr-computer
    ;;                                        '(work-laptop wild-dog))))
    ;;            has-gnu-global-p))

    ;; (clojure-mode ,(not (version< emacs-version "25.1")))
    (iedit nil) ;; using git submodule
    ;; (cider ,(memq my-curr-computer '(wild-dog)))
    ;; (hl-line+ ;; used for custom `occur' mods, but only pre emacs 25
    ;;  ,(<= emacs-major-version 24))
    (geiser nil) ;;,(memq my-curr-computer '(work-laptop))
    ;; (debbugs ,(memq my-curr-computer '(work-laptop wild-dog)))
    (adoc-mode nil) ;; using git submodule
    (markdown-mode nil ;; using git submodule
                   ;;,(not (version< emacs-version "24.4"))
     )
    (typescript-mode nil) ;; using git submodule
    (tide nil ;; using git submodule
          ;; ,(memq my-curr-computer
          ;;        '(work-laptop-2019 work-laptop wild-dog work-laptop-bash))
          )
    (context-coloring nil)
    (nov nil ;; using git submodule
         ;;,(not (version< emacs-version "24.4"))
     ) ;; an epub reader
    (autothemer nil) ;; dependency for some themes. using git submodule.
    (erc-hl-nicks nil) ;; using git submodule
    ;; (sql-indent t)
    (vdiff nil)
    ;; (tern ,has-nodejs-p)
    ;; (company-tern ,has-nodejs-p)
    ;; (browse-kill-ring t)
    ;; (git-gutter ,(not (version< emacs-version "24.3")))
    (eglot nil ;; using git submodule
           ;;,(not (version< emacs-version "26.1"))
     )
    (lsp-mode nil ;; using git submodule
              ;;,(not (version< emacs-version "25.1"))
	      )
    (company-lsp nil ;; using git submodule
                ;;,(not (version< emacs-version "25.1"))
		 )
    ;; (ccls ,(memq my-curr-computer '(wild-dog)))
    ;; (cquery ,(memq my-curr-computer '(wild-dog)))
    (websocket nil) ;; not used? maybe an old dependency?
    ;; (deadgrep ,(not (version< emacs-version "25.1")))
    (rg nil) ;; using git submodule
    (eros nil) ;; using git submodule

    ;; hl-block-mode shades outers scopes progresivly darker.
    (hl-block-mode nil
                   ;;,(not (version< emacs-version "26.0"))
     )
    (mini-modeline nil)
    (yaml-mode nil) ;; using git submodule
    (php-mode nil) ;; using git submodule
    ;; (lsp-python-ms t)
    (transient nil) ;; using git submodule
    (wgrep nil) ;; using git submodule
    ;; (spinner t)
    (ht nil) ;; using git submodule
    (dash-functional nil) ;; using git submodule
    (dash nil) ;; using git submodule
    )
  "Packages I use from elpa/melpa.")

;; (defun my-ssl-p ()
;;   "True if the Emacs instance has ssl setup/enabled."
;;   (or (not (memq system-type '(windows-nt ms-dos)))
;;       (gnutls-available-p)))

;; temporarily stop using ssl (https) in the package archive urls.
;; https causes a hang on ms-windows when calling `list-packages'.
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; ;; set up package archives.
;; (let* ((protocol (if (my-ssl-p) "https" "http"))
;;        (url-melpa (concat protocol "://melpa.org/packages/")))
;;   (add-to-list 'package-archives `("melpa" . ,url-melpa) t)

;;   (when (< emacs-major-version 24)
;;     (let ((url-elpa (concat protocol "://elpa.gnu.org/packages/")))
;;       (add-to-list 'package-archives `("gnu" . ,url-elpa)))))

(setq package-pinned-packages ; Emacs 24.4 or newer
      '((sql-indent . "gnu")
        (electric-spacing . "melpa")
        (aggressive-indent . "melpa")
        (ggtags . "melpa")
        (vdiff . "melpa")
        (websocket . "melpa")
        (eglot . "melpa")))

;; (package-initialize)

(defun my-install-packages ()
  "Call this function on a new Emacs installation to install packages.
Installs packages in the list `my-packages'."
  (interactive)
  ;; fetch the list of packages available
  (unless package-archive-contents
    (package-refresh-contents))

  ;; install the missing packages
  (dolist (obj my-packages)
    (let ((pkg (cl-first obj))
          (installp (cl-second obj)))
      (when installp
        (unless (package-installed-p pkg)
          (message "installing pkg: %s" (symbol-name pkg))
          (package-install pkg))))))

;; (unless (eq my-curr-computer 'work-laptop-bash)
;;   (my-install-packages))

(defun my-upgrade-packages ()
  "Upgrade installed packages.
Code taken from http://oremacs.com/2015/03/20/managing-emacs-packages/"
  (interactive)
  (save-window-excursion
    (package-list-packages) ;; (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute t)))

(defun my-package-list-unaccounted-packages ()
  "Display unaccounted packages.
Like `package-list-packages', but only show packages that are installed and not
in `my-packages'.  Useful for cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (let ((my-packs (mapcar #'cl-first my-packages)))
     (cl-remove-if-not (lambda (x)
                         (and (not (memq x my-packs))
                              (not (package-built-in-p x))
                              (package-installed-p x)))
                       (mapcar 'car package-archive-contents)))))