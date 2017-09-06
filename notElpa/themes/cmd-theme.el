;;; -*- lexical-binding: t -*-

;;; Commentary: This theme was made for the 16 color full screen console in
;;; Windows XP. It's named "cmd" after cmd.exe.

;; Possible_Colors: emacs-name / vim-name
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


(custom-theme-set-variables
 'cmd

 `(fci-rule-color "darkgray")

 `(ivy-switch-buffer-faces-alist
   '((emacs-lisp-mode . '((t (:foreground "green"))))
     (lisp-interaction-mode . '((t (:foreground "green"))))
     (dired-mode . `((t (:foreground "lightblue"))))
     (org-mode . `((t (:foreground "red")))))))


(custom-theme-set-faces
 'cmd

 `(default ((t (:foreground "black" :background "lightgray"))))

 '(show-paren-match ((t (:background "black"))))

 ;; separates windows.
 '(vertical-border ((t (:foreground "darkgray"
                                    :background "black"))))

 `(mode-line
   ((t (:foreground "white"
                    :background "green"
                    :box (:line-width -1 :style released-button)))
    (t :inverse-video t)))
 `(mode-line-inactive
   ((t (:foreground "black"
                    :background "darkgray"
                    :box (:line-width -1 :style released-button)))))
 `(mode-fer-id ((t (:foreground "white" :weight bold))))


 '(region
   ((t (:background "white"))))

 '(isearch ((t :background "yellow"
               :foreground "black"
               :bold t
               :underline nil)))
 ;; the non-selected matches from isearch
 '(lazy-highlight ((t
                    :background "lightcyan"
                    :foreground "green"
                    :bold nil
                    :underline t)))

 ;; `(font-lock-builtin-face ((t (:foreground "???" :weight bold))))
 `(font-lock-comment-face ((t (:foreground "green"))))
 `(font-lock-comment-delimiter-face ((t (:foreground "green"))))
 `(font-lock-constant-face ((t (:foreground "cyan"))))
 `(font-lock-doc-face ((t (:foreground "red"))))
 `(font-lock-function-name-face ((t (:foreground "lightblue"))))
 `(font-lock-variable-name-face ((t (:foreground "lightblue"))))
 `(font-lock-keyword-face ((t (:foreground "blue" :weight bold))))
 ;; `(font-lock-negation-char-face ((t (:foreground "???" :weight bold))))
 ;; `(font-lock-preprocessor-face ((t (:foreground "???"))))
 ;; `(font-lock-regexp-grouping-construct ((t (:foreground "???" :weight bold))))
 ;; `(font-lock-regexp-grouping-backslash ((t (:foreground "???" :weight bold))))
 `(font-lock-string-face ((t (:foreground "red"))))
 ;; `(font-lock-type-face ((t (:foreground "???"))))
 `(font-lock-warning-face ((t (:foreground "lightred" :background "black" :weight bold))))



 '(rainbow-delimiters-depth-1-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "lightgreen"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "lightred"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "lightcyan"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "lightmagenta"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "lightblue"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "brown"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-unmatched-face
   ((t (:foreground "lightred" :background "darkgray")))))


