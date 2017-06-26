;;; charcoal-theme.el --- A dark theme with pastelish chalkish charcoal colors.

;;; Commentary:
;;; Keywords should appear "raised" or "3D" against the bg.

;;; Code:

(deftheme charcoal "The charcoal color theme")

;;background: #35352B #000000
(let* ((graphicp (display-graphic-p)) ;;  cache
       (class t)
       ;; (class '((class color)
       ;;          (min-colors 88)
       ;;          (background dark)))
       (charcoal-bg "#35352B")
       (charcoal-fg "lightyellow3")) ;; snow3 gray80 lightyellow3
  (custom-theme-set-faces
   'charcoal

   ;; TODO
   `(default ((,class (:foreground ,charcoal-fg :background ,charcoal-bg))))

   ;; DONE
   `(show-paren-match ((,class (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))

   ;; separates windwos.
   `(vertical-border ((,class (:foreground "gray25"))))


   ;; TODO
   `(mode-line
     ((,class (:foreground "#8FB28F"
                      :background "#2B2B2B"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   ;; TODO
   `(mode-line-buffer-id ((,class (:foreground "#F0DFAF" :weight bold))))
   ;; TODO
   `(mode-line-inactive
     ((,class (:foreground "#5F7F5F"
                      :background "#383838"
                      :box (:line-width -1 :style released-button)))))
   ;; TODO
   `(region
     ((,class (:background "#49483E"))))     ;69685E

   ;; TODO
   `(fringe ((,class (:background "black"))))

   ;; TODO font lock
   ;; `(font-lock-builtin-face ((,class (:foreground ,zenburn-fg :weight bold))))
   `(font-lock-comment-face ((,class (:foreground "#8FB28F" :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "medium spring green"))))
   ;; `(font-lock-constant-face ((,class (:foreground ,zenburn-green+4))))
   ;; `(font-lock-doc-face ((,class (:foreground ,zenburn-green+2))))
   ;; `(font-lock-function-name-face ((,class (:foreground ,zenburn-cyan))))
   `(font-lock-keyword-face ((,class (:foreground "blanched almond" :weight bold))))
   ;; `(font-lock-negation-char-face ((,class (:foreground ,zenburn-yellow :weight bold))))
   ;; `(font-lock-preprocessor-face ((,class (:foreground ,zenburn-blue+1))))
   ;; `(font-lock-regexp-grouping-construct ((,class (:foreground ,zenburn-yellow :weight bold))))
   ;; `(font-lock-regexp-grouping-backslash ((,class (:foreground ,zenburn-green :weight bold))))
   ;; `(font-lock-string-face ((,class (:foreground ,zenburn-red))))
   ;; `(font-lock-type-face ((,class (:foreground ,zenburn-blue-1))))
   ;; `(font-lock-variable-name-face ((,class (:foreground ,zenburn-orange))))
   ;; `(font-lock-warning-face ((,class (:foreground ,zenburn-yellow-2 :weight bold))))


   ;; TODO rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground "orange red"))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground "cyan"))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground "yellow"))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground "plum"))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground "lawn green"))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground "orange"))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground "white"))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground "hot pink" :background "#2F2F2F"))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground "burlywood3"))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground "sienna" :background "black"))))))
