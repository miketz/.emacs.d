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

;;;----------------------------------------------------------------------------
;;; font
;;;----------------------------------------------------------------------------
(push
   ;; '(font . "-*-Menlo-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
   ;; '(font . "-*-JetBrains Mono NL-light-normal-normal-*-15-*-*-*-m-0-iso10646-1")
   ;; '(font . "-*-Ubuntu Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
   ;; '(font . "-*-Ubuntu Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
   ;; '(font . "-*-Iosevka-regular-normal-normal-*-17-*-*-*-m-0-iso10646-1")
   '(font . "-*-Iosevka-light-normal-normal-*-17-*-*-*-m-0-iso10646-1")
   default-frame-alist)

(load-theme 'charcoal t)

;;;----------------------------------------------------------------------------
;;; ido
;;;----------------------------------------------------------------------------
(ido-mode 1)

;;;----------------------------------------------------------------------------
;;; Paredit
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/paredit" load-path)
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook #'enable-paredit-mode)
;;(add-hook 'sly-mrepl-mode-hook (lambda () (paredit-mode +1)))
;;(add-hook 'sql-mode-hook #'enable-paredit-mode)


(with-eval-after-load 'paredit
  ;; remove the new "RET" key behavior added to paredit. Seems to break indent and
  ;; in the minibuffer it eats the return key so I can't press RET to eval.
  ;; TODO: look into the "proper" way to fix this.
  (define-key paredit-mode-map (kbd "RET") nil)
  (define-key paredit-mode-map (kbd "C-j") nil)

  ;; barf/slurp keybinds
  (define-key paredit-mode-map (kbd "C-9") #'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-0") #'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-M-9") #'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-M-0") #'paredit-backward-barf-sexp)
  ;; don't allow paredit to steal the M-r keybind.
  (define-key paredit-mode-map (kbd "M-r") #'move-to-window-line-top-bottom)
  ;; rebind `paredit-raise-sexp' to C-M-r
  (define-key paredit-mode-map (kbd "C-M-r") #'paredit-raise-sexp))


;;;----------------------------------------------------------------------------
;;; MISC
;;;----------------------------------------------------------------------------
