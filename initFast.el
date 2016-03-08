;; turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

(load-theme 'leuven t)
;; (set-background-color "white")

;;;----------------------------------
;;; cursor
;;;----------------------------------
(setq-default cursor-type '(bar . 2))
;; (custom-set-faces
;;  '(cursor ((t (:background "cyan")))))


;; (when (eq system-type 'windows-nt)
;;  (set-frame-font "-raster-r_ansi-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"))
(when (eq system-type 'windows-nt)
  (set-frame-font "-raster-Fixedsys-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1")
    ;; (custom-set-faces
    ;;  `(default ((t (:family "Consolas"
    ;;                         :foundry "outline"
    ;;                         :slant normal
    ;;                         :weight normal
    ;;                         :height 125
    ;;                         :width )))))
    )

;; (custom-set-faces
;;  '(default ((t (:family "Droid Sans Mono" :foundry "unknown" :slant normal :weight normal :height 140 :width normal)))))


(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq inhibit-startup-message t)
(setq initial-scratch-message "\n\n\n\n\n")

(blink-cursor-mode 0)
;(hl-line-mode 0)

;(global-auto-revert-mode t) ;;reload buffer if it changes on disk outside emacs.

(setq-default line-spacing 0)
;(global-linum-mode 0) ;show/hide line numbers
;(setq-default column-number-mode t)
;(setq-default line-number-mode t)
;(setq-default indicate-empty-lines t) ;Like vim's tildes

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
