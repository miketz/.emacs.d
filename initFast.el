;;; -*- lexical-binding: t -*-
;;; NOTE: keep this init basic so it works with older Emacs versions.
;;; Oldest emacs verified: 22.3.1

;; turn off mouse interface early in startup to avoid momentary display
;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;;;----------------------------------
;;; cursor
;;;----------------------------------
;; (setq-default cursor-type '(bar . 2))
;; (custom-set-faces
;;  '(cursor ((t (:background "blue")))))
;; (blink-cursor-mode 0)

;; sacrifice proper display of right-to-left languages for performance.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Avoid resizing the GUI frame when font changes.
;; see https://old.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn
;; _fast/
(setq frame-inhibit-implied-resize t)


(setq fast-but-imprecise-scrolling t)
;; scrolling performance increase?
;; see https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-moveme
;; nt-lag/28746
(setq auto-window-vscroll nil)


(when (eq system-type 'windows-nt)
  ;; (set-frame-font "-raster-Fixedsys-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1")

  ;; faster than `set-frame-font' for setting the font?
  ;; see https://old.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_da
  ;; mn_fast/
  (push '(font . "-raster-Fixedsys-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1")
        default-frame-alist))

;; case insensitive for emacs completion
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completion-ignore-case t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq inhibit-startup-message t)
(setq initial-scratch-message "\n\n\n\n\n")
(setq initial-major-mode #'fundamental-mode) ;;for faster startup.

;; (hl-line-mode 0)

;; (global-auto-revert-mode t) ; reload buffer if it changes on disk outside emacs.

(setq-default line-spacing 0)

;; (setq-default indicate-empty-lines t) ; Like vim's tildes

(setq-default transient-mark-mode t)  ;show selected regions
(setq ring-bell-function 'ignore)

(progn ;;tab handling
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default indent-line-function 'insert-tab))

(progn ;; for better or worse, prevent creation of tmp backup files
  (setq make-backup-files nil)          ; No annoying backup files
  (setq-default backup-inhibited t)
  (setq auto-save-default nil))         ; No annoying auto-save files

;;disable annoying newline emacs automatically adds to the end of a file when saving.
(setq require-final-newline nil)
(setq mode-require-final-newline nil)


;; speed up opening files. see https://www.reddit.com/r/emacs/comments/4c0mi3/t
;; he_biggest_performance_improvement_to_emacs_ive/
;; TODO: revisit this later. The performance problems may be fixed soon.
;;       see: https://lists.gnu.org/archive/html/emacs-devel/2016-02/msg00440.h
;;       tml
(let ((hook (if (boundp 'find-file-hook)
                'find-file-hook      ; use new hook var if available
              'find-file-hooks))     ; else older emacs-version < 22.1
      (fn (if (fboundp 'vc-refresh-state)
              'vc-refresh-state      ; use new hook fn if available.
            'vc-find-file-hook)))    ; else older emacs-version < 25.1
  ;; remove a vc source control hook that greatly slows down opening files.
  (remove-hook hook fn))

(when (and (boundp global-eldoc-mode) ;;(>= emacs-major-version 25)
           global-eldoc-mode)
  (global-eldoc-mode 0))

(when (eq system-type 'windows-nt)
  ;; performance tweak for weird fonts on windows.
  ;; see https://github.com/sabof/org-bullets/issues/11
  (setq inhibit-compacting-font-caches t))

;; (defun my-load-full-init ()
;;   (interactive)
;;   (load "~/.emacs.d/init.el"))

;; to access packages run (package-initialize)
