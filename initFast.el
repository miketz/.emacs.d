;; turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; (load-theme 'leuven t)
;; (set-background-color "white")

;;;----------------------------------
;;; cursor
;;;----------------------------------
(setq-default cursor-type '(bar . 2))
(custom-set-faces
 '(cursor ((t (:background "blue")))))


;; (when (eq system-type 'windows-nt)
;;  (set-frame-font "-raster-r_ansi-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"))
(when (eq system-type 'windows-nt)
  (set-frame-font "-raster-Terminal-normal-normal-normal-mono-18-*-*-*-c-*-ms-oemlatin"))

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

(progn ;; for better or worse, prevent creation of tmp backup files
  (setq make-backup-files nil)          ;No annoying backup files
  (setq-default backup-inhibited t)
  (setq auto-save-default nil)          ;No annoying auto-save files
  )


;;disable annoying newline emacs automatically adds to the end of a file when saving.
(setq require-final-newline nil)
(setq mode-require-final-newline nil)


;; speed up opening files. see https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
;; TODO: revist this later. The performance problems may be fixed soon.
;;       see: https://lists.gnu.org/archive/html/emacs-devel/2016-02/msg00440.html
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(defun my-load-full-init ()
  (interactive)
  (load "~/.emacs.d/init.el"))

;; to access packages run (package-initialize)
