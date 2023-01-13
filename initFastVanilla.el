(load "~/.emacs.d/initFast") ;; .el or .elc if available


;;;----------------------------------------------------------------------------
;;; Packages. NOTE: with the conversion to git submodules, most of the old
;;; package code is moved to ~/.emacs.d/notElpa/mine/my-package-stuff.el
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/" load-path) ; stores elisp files that are
                                       ; not "packages".
(push "~/.emacs.d/notElpa/mine/" load-path)
(setq custom-theme-directory "~/.emacs.d/notElpa/themes/") ;color themes.
;; some themes require helper files so add themes dir to load-path.
(push custom-theme-directory load-path)

(push "~/.emacs.d/notElpa/themes/replace-colorthemes/" custom-theme-load-path)


;;;----------------------------------------------------------------------------
;;; Color theme stuff.
;;;----------------------------------------------------------------------------
;; In Emacs 27+ make `custom-theme-set-faces' work immediately.
(setq custom--inhibit-theme-enable nil)

;;TODO: implement a way to undo color settings made outside the theme
;;      definition. Use custom-theme-set-faces to set the colors/styles so they
;;      are rolled back when switching/disabling themes.
(defadvice load-theme (before disable-before-load)
  "Disable any loaded themes before enabling a new theme.
This prevents overlapping themes; something I would rarely want."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

;; (defadvice load-theme (before capture-theme)
;;   "Capture the theme in a global var."
;;   (setq my-curr-theme theme))

(setq custom-safe-themes t) ;; Disable the confirmation to load themes.
;; (defadvice load-theme (around disable-security)
;;   "Disable the confirmation to load themes."
;;     (let ((no-confirm t))
;;       ad-do-it))

(ad-activate 'load-theme)

;;;----------------------------------------------------------------------------
;;; rainbow-delimiters
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/rainbow-delimiters" load-path)
(autoload #'rainbow-delimiters-mode "rainbow-delimiters" nil t)
(autoload #'rainbow-delimiters-mode-enable "rainbow-delimiters" nil t)
(autoload #'rainbow-delimiters-mode-disable "rainbow-delimiters" nil t)

(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'slime-repl-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'ielm-mode-hook #'rainbow-delimiters-mode-enable)

;; this doesn't work for mini buffer? TODO: look into it.
;; (add-hook 'eval-expression-minibuffer-setup-hook
;;           #'rainbow-delimiters-mode-enable)

;; (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'sql-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'c-mode-common-hook #'rainbow-delimiters-mode-enable)
;; (add-hook 'sly-mrepl-mode-hook #'rainbow-delimiters-mode-enable)
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable)
;;(global-rainbow-delimiters-mode)
