(load "~/.emacs.d/initFast") ;; .el or .elc if available

;;;----------------------------------------------------------------------------
;;; load-path
;;;----------------------------------------------------------------------------
;; do this at the top becuase some packages like evil-leader require being
;; loaded before their parent depency
(push "~/.emacs.d/notElpa/" load-path)
(push "~/.emacs.d/notElpa/mine/" load-path)
(push "~/.emacs.d/notElpa/evil" load-path)
(push "~/.emacs.d/notElpa/evil-leader" load-path)
(push "~/.emacs.d/notElpa/rainbow-delimiters" load-path)
(push "~/.emacs.d/notElpa/paredit" load-path)

;;;----------------------------------------------------------------------------
;;; Color theme stuff.
;;;----------------------------------------------------------------------------
(setq custom-theme-directory "~/.emacs.d/notElpa/themes/") ; color themes.
;; some themes require helper files so add themes dir to load-path.
(push custom-theme-directory load-path)
(push "~/.emacs.d/notElpa/themes/replace-colorthemes/" custom-theme-load-path)

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

(load-theme 'charcoal t)

;;;----------------------------------------------------------------------------
;;; rainbow-delimiters
;;;----------------------------------------------------------------------------
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
;;; ido
;;;----------------------------------------------------------------------------
(ido-mode 1)

;;;----------------------------------------------------------------------------
;;; Paredit
;;;----------------------------------------------------------------------------
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
;;; repeat
;;;----------------------------------------------------------------------------
(defvar repeat-exit-timeout) ;; silence byte computer warning

(with-eval-after-load 'repeat
  (setq repeat-exit-timeout nil)) ;; don't timeout

(when (fboundp #'repeat-mode) ;; emacs 28+
  (repeat-mode))

;;;----------------------------------------------------------------------------
;;; evil-leader
;;;----------------------------------------------------------------------------
;; NOTE: per docs, evil-leader must be enabled before evil mode.
(require 'evil-leader)
(global-evil-leader-mode)
;; leader keys
(evil-leader/set-leader ",")
(evil-leader/set-key "q" #'balance-windows)
(evil-leader/set-key "x" #'maximize-window)
(evil-leader/set-key "," #'delete-other-windows)
(evil-leader/set-key "d" #'delete-window)
(evil-leader/set-key "c" #'quit-window) ; buffer left alive
(evil-leader/set-key "v" #'evil-visual-block)
(evil-leader/set-key "b" #'switch-to-buffer)

;;;----------------------------------------------------------------------------
;;; evil
;;;----------------------------------------------------------------------------
(require 'evil)
(evil-mode)

;;;----------------------------------------------------------------------------
;;; key-chord
;;;----------------------------------------------------------------------------
(require 'key-chord)
(setq key-chord-two-keys-delay 0.2)
(setq key-chord-one-key-delay 0.4)
(key-chord-define evil-insert-state-map "fj" #'evil-normal-state)
(key-chord-define evil-visual-state-map "fj" #'evil-exit-visual-state)
(key-chord-mode 1)
