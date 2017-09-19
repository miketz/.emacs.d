;;; -*- lexical-binding: t -*-

;;; Commentary: This theme was made for the 16 color full screen console in
;;; Windows XP. It's named "cmd" after cmd.exe.

;; Possible_Colors:
;; emacs-name   vim-name
;; -----------------------
;; black        black
;; blue         darkBlue
;; green        darkGreen
;; cyan         darkCyan
;; red          darkRed
;; magenta      darkMagenta
;; brown        darkYellow ?
;; lightgray    gray
;; darkgray     darkGray
;; lightblue    blue
;; lightgreen   green
;; lightcyan    cyan
;; lightred     red
;; lightmagenta magenta
;; yellow       yellow ?
;; white        white


(deftheme cmd "16 color theme for the windows command prompt.")

;; color name strings/values obtained via `list-colors-display' on an Emacs
;; running in the the windows command prompt.
;; NOTE: the "string" names from `list-colors-display' have  different rgb
;;       values in different environments. So specify rgb values so the
;;       theme will look similar in GUI mode. Even with the same rgb values
;;       the colors seem to render diffrently in GUI mode, but it's closer than
;;       the string names.
(let* ((black "#000000")
       (blue "#0000cd")
       (green "#228b22")
       (cyan "#00ced1")
       (red "#b22222")
       (magenta "#8b008b")
       (brown "#a0522d")
       (lightgray "#bebebe")
       (darkgray "#666666")
       (lightblue "#0000ff")
       (lightgreen "#00ff00")
       (lightcyan "#00ffff")
       (lightred "#ff0000")
       (lightmagenta "#ff00ff")
       (yellow "#ffff00")
       (white "#ffffff")

       (fg black)
       (bg lightgray)
       (keyword blue)
       (var lightblue))

 (custom-theme-set-variables
  'cmd

  `(fci-rule-color ,darkgray)

  `(ivy-switch-buffer-faces-alist
    '((emacs-lisp-mode . '((t (:foreground ,green))))
      (lisp-interaction-mode . '((t (:foreground ,green))))
      (dired-mode . `((t (:foreground ,lightblue))))
      (org-mode . `((t (:foreground ,red)))))))


 (custom-theme-set-faces
  'cmd

  `(default ((t (:foreground ,fg :background ,bg))))

  `(show-paren-match ((t (:background ,white :foreground ,black))))

  ;; separates windows.
  `(vertical-border ((t (:foreground ,darkgray
                                     :background ,black))))

  `(mode-line
    ((t (:foreground ,white
                     :background ,green))
     (t :inverse-video t)))
  `(mode-line-inactive
    ((t (:foreground ,black
                     :background ,darkgray))))
  `(mode-line-buffer-id ((t (:foreground ,white :weight bold))))


  `(region
    ((t (:background ,white))))

  `(isearch ((t :background ,yellow
                :foreground ,black
                :bold t
                :underline nil)))
  ;; the non-selected matches from isearch
  `(lazy-highlight ((t
                     :background ,lightcyan
                     :foreground ,green
                     :bold nil
                     :underline t)))

  `(font-lock-builtin-face ((t (:foreground ,cyan :weight bold))))
  `(font-lock-comment-face ((t (:foreground ,green))))
  `(font-lock-comment-delimiter-face ((t (:foreground ,green))))
  `(font-lock-constant-face ((t (:foreground ,cyan))))
  `(font-lock-doc-face ((t (:foreground ,red))))
  `(font-lock-function-name-face ((t (:foreground ,var))))
  `(font-lock-variable-name-face ((t (:foreground ,var))))
  `(font-lock-keyword-face ((t (:foreground ,keyword :weight bold))))
  ;; `(font-lock-negation-char-face ((t (:foreground "???" :weight bold))))
  ;; `(font-lock-preprocessor-face ((t (:foreground "???"))))
  ;; `(font-lock-regexp-grouping-construct ((t (:foreground "???" :weight bold))))
  ;; `(font-lock-regexp-grouping-backslash ((t (:foreground "???" :weight bold))))
  `(font-lock-string-face ((t (:foreground ,red))))
  ;; `(font-lock-type-face ((t (:foreground "???"))))
  `(font-lock-warning-face ((t (:foreground ,lightred :background ,black :weight bold))))


  `(completions-common-part ((t (:foreground ,darkgray))))
  `(completions-first-difference ((t (:foreground ,lightblue))))


  `(ido-first-match ((t (:background ,white :foreground ,black :weight bold))))
  `(ido-only-match ((t (:background ,white :foreground ,lightblue :weight bold))))
  `(ido-subdir ((t (:foreground ,lightblue ))))
  ;; `(ido-indicator ((t (:foreground ,white))))

  `(magit-diff-file-heading-highlight ((t :foreground ,white :background ,black)))
  `(magit-diff-added-highlight ((t :foreground ,lightcyan :background ,green)))
  ;; magit-log-grpah is the short comment text (highlighted) in magit-log
  `(magit-section-highlight ((t :background ,white)))
  ;; magit-log-grpah is the short comment text (unhighlighted) in magit-log
  `(magit-log-graph ((t :foreground ,fg :background ,bg)))
  ;; `(magit-section-heading ((t :foreground ,lightred :background ,yellow)))
  ;; `(magit-section-secondary-heading ((t :foreground ,lightred :background ,yellow)))
  ;; `(magit-section-heading-selection ((t :foreground ,lightred :background ,yellow)))


  `(rainbow-delimiters-depth-1-face ((t (:foreground ,black))))
  `(rainbow-delimiters-depth-2-face ((t (:foreground ,lightgreen :background ,darkgray))))
  `(rainbow-delimiters-depth-3-face ((t (:foreground ,lightred ))))
  `(rainbow-delimiters-depth-4-face ((t (:foreground ,lightcyan :background ,darkgray))))
  `(rainbow-delimiters-depth-5-face ((t (:foreground ,lightmagenta))))
  `(rainbow-delimiters-depth-6-face ((t (:foreground ,lightblue))))
  `(rainbow-delimiters-depth-7-face ((t (:foreground ,brown))))
  `(rainbow-delimiters-depth-8-face ((t (:foreground ,red :background ,brown))))
  `(rainbow-delimiters-depth-9-face ((t (:foreground ,white :background ,darkgray))))
  `(rainbow-delimiters-unmatched-face
    ((t (:foreground ,lightred :background ,black))))))


