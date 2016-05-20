;;; charcoal-theme.el --- A dark theme with pastelish chalkish charcoal colors.

;;; Credits: Using zenburn-theme.el from Bozhidar Batsov as a base.

;;; Commentary:
;;; Keywords should appear "raised" or "3D" against the bg.

;;; Code:

(deftheme charcoal "The charcoal color theme")

;;background: #35352B #000000
(let* ((graphicp    (display-graphic-p)); cache
       (charcoal-bg "#35352B")
       (charcoal-fg "lightyellow3"      ; snow3 gray80 lightyellow3
                    ))
  (custom-theme-set-faces
   'charcoal

   ;; TODO
   `(default ((t (:foreground ,charcoal-fg :background ,charcoal-bg))))

   ;; DONE
   '(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))

   ;; separates windwos.
   '(vertical-border ((t (:foreground "gray25"))))


   ;; TODO
    `(mode-line
     ((t (:foreground "#8FB28F"
                           :background "#2B2B2B"
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   ;; TODO
   `(mode-line-buffer-id ((t (:foreground "#F0DFAF" :weight bold))))
   ;; TODO
   `(mode-line-inactive
     ((t (:foreground "#5F7F5F"
                      :background "#383838"
                      :box (:line-width -1 :style released-button)))))
   ;; TODO
   '(region
     ((t (:background "#49483E"))))     ;69685E

   ;; TODO
   `(fringe ((t (:background "black"))))

   ;; TODO font lock
   ;; `(font-lock-builtin-face ((t (:foreground ,zenburn-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground "#8FB28F" :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground "medium spring green"))))
   ;; `(font-lock-constant-face ((t (:foreground ,zenburn-green+4))))
   ;; `(font-lock-doc-face ((t (:foreground ,zenburn-green+2))))
   ;; `(font-lock-function-name-face ((t (:foreground ,zenburn-cyan))))
   `(font-lock-keyword-face ((t (:foreground "blanched almond" :weight bold))))
   ;; `(font-lock-negation-char-face ((t (:foreground ,zenburn-yellow :weight bold))))
   ;; `(font-lock-preprocessor-face ((t (:foreground ,zenburn-blue+1))))
   ;; `(font-lock-regexp-grouping-construct ((t (:foreground ,zenburn-yellow :weight bold))))
   ;; `(font-lock-regexp-grouping-backslash ((t (:foreground ,zenburn-green :weight bold))))
   ;; `(font-lock-string-face ((t (:foreground ,zenburn-red))))
   ;; `(font-lock-type-face ((t (:foreground ,zenburn-blue-1))))
   ;; `(font-lock-variable-name-face ((t (:foreground ,zenburn-orange))))
   ;; `(font-lock-warning-face ((t (:foreground ,zenburn-yellow-2 :weight bold))))


   ;; TODO rainbow-delimiters
   '(rainbow-delimiters-depth-1-face ((t (:foreground "orange red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "plum"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "lawn green"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "white"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "hot pink" :background "#2F2F2F"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black"))))))



