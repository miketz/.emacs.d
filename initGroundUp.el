;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))


(custom-set-faces
 `(default ((t (:family "Consolas"
                        :foundry "outline"
                        :slant normal
                        :weight normal
                        :height 125
                        :width normal)))))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq inhibit-startup-message t)
(setq initial-scratch-message "\n\n\n\n\n")

(setq-default cursor-type '(bar . 2))
(blink-cursor-mode 0)
;;(hl-line-mode 0)

(global-auto-revert-mode t) ;;reload buffer if it changes on disk outside emacs.

(setq-default line-spacing 0)
;(global-linum-mode 0) ;show/hide line numbers
;(setq-default column-number-mode t)
;(setq-default line-number-mode t)
(setq-default indicate-empty-lines t) ;Like vim's tildes

(setq-default transient-mark-mode t)  ;show selected regions
(setq ring-bell-function 'ignore)

(progn ;;tab handling
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default indent-line-function 'insert-tab)
  )

(setq make-backup-files nil backup-inhibited t) ;No annoying backup files
(setq auto-save-default nil) ;No annoying auto-save files


;;disable annoying newline emacs automatically adds to the end of a file when saving.
(setq require-final-newline nil)
(setq mode-require-final-newline nil)

;; to access packages run (package-initialize)
(package-initialize)

(setq my-packages
      '(god-mode
        paredit
        zenburn-theme
        ace-jump-mode
        lispy))

;;----------------------------------------------------------------
;; theme
;;----------------------------------------------------------------
(load-theme 'zenburn t)

;;----------------------------------------------------------------
;; god-mode
;;----------------------------------------------------------------
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)

;;------------------------------------------------------------------------------
;; swiper
;;------------------------------------------------------------------------------
;; swiper from melpa is broken atm.
;;(global-set-key (kbd "C-s") #'swiper)

;;------------------------------------------------------------------------------
;; ace-jump-mode
;;------------------------------------------------------------------------------
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;------------------------------------------------------------------------------
;; lispy
;;------------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook       #'lispy-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'lispy-mode)
(add-hook 'ielm-mode-hook             #'lispy-mode)
(add-hook 'lisp-mode-hook             #'lispy-mode)
(add-hook 'lisp-interaction-mode-hook #'lispy-mode)
(add-hook 'scheme-mode-hook           #'lispy-mode)
(add-hook 'slime-repl-mode-hook #'lispy-mode)

(with-eval-after-load "lispy"
  ;;(lispy-set-key-theme '(special)) ;helps when using paredit with lispy.
  (lispy-set-key-theme '(special paredit c-digits))
  ;; don't evaluate/insert on C-j. Use the plain way like paredit.
  (define-key lispy-mode-map (kbd "C-j") #'lispy-newline-and-indent-plain))