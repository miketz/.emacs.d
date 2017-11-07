;; -*- lexical-binding: t -*-
;;; charcoal-theme.el --- A dark theme with pastelish chalkish charcoal colors.

;;; Code:

(deftheme charcoal "Charcoal color theme")

(let* ((class        t)
       (cnt          (display-color-cells))
       (color-full-p (>= cnt 16777216))
       (color-256-p  (and (not color-full-p) (>= cnt 256)))
       (color-16-p   (and (not color-256-p) (>= cnt 16)))
       (color-8-p    (and (not color-16-p) (>= cnt 8)))
       (i            (cond (color-full-p 0)
                           (color-256-p  1)
                           (color-16-p   2)
                           (color-8-p    3)
                           (t            3)))
       (todo-colr   "#FFFFFF") ; temp color where I haven't decided yet

       ;; Color Palette      full      256       16        8
       (bg           (aref ["#35352B" todo-colr todo-colr todo-colr] i))
       (fg           (aref ["#EEEED1" todo-colr todo-colr todo-colr] i))
       (faint        (aref ["#4D4D3D" todo-colr todo-colr todo-colr] i))
       (keyword      (aref ["#FFEBCD" todo-colr todo-colr todo-colr] i))
       (var          (aref ["#66CDAA" todo-colr todo-colr todo-colr] i)))

  ;; lightyellow2=#EEEED1
  ;; snow3 gray80 lightyellow3

  (custom-theme-set-variables
   'charcoal

   `(fci-rule-color ,faint)

   `(evil-emacs-state-cursor    '(bar "cyan"))
   `(evil-normal-state-cursor   '(hollow "spring green"))
   `(evil-insert-state-cursor   '(bar "spring green"))
   `(evil-visual-state-cursor   '(hollow "orange"))
   `(evil-operator-state-cursor '(hollow "spring green"))
   `(evil-replace-state-cursor  '(hbar "orange red"))
   `(evil-motion-state-cursor   '(box "spring green"))

   ;; Duplicating the default vc-annotate colors for now.
   ;; TODO: tailor them for the bg.
   `(vc-annotate-color-map '((20 . "#FF3F3F")
                             (40 . "#FF6C3F")
                             (60 . "#FF993F")
                             (80 . "#FFC63F")
                             (100 . "#FFF33F")
                             (120 . "#DDFF3F")
                             (140 . "#B0FF3F")
                             (160 . "#83FF3F")
                             (180 . "#56FF3F")
                             (200 . "#3FFF56")
                             (220 . "#3FFF83")
                             (240 . "#3FFFB0")
                             (260 . "#3FFFDD")
                             (280 . "#3FF3FF")
                             (300 . "#3FC6FF")
                             (320 . "#3F99FF")
                             (340 . "#3F6CFF")
                             (360 . "#3F3FFF")))
   `(vc-annotate-very-old-color "blue3"))


  (custom-theme-set-faces
   'charcoal

   `(default ((,class (:foreground ,fg :background ,bg))))

   `(cursor ((,class (:background "spring green"))))

   `(show-paren-match ((,class (:slant italic
                                       :bold t
                                       :strike-through t
                                       :background nil))))

   ;; line that separates vertically split windows.
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
   `(font-lock-keyword-face ((,class ((,class (:foreground ,keyword :weight bold))))))
   ;; `(font-lock-negation-char-face ((,class (:foreground ,zenburn-yellow :weight bold))))
   ;; `(font-lock-preprocessor-face ((,class (:foreground ,zenburn-blue+1))))
   ;; `(font-lock-regexp-grouping-construct ((,class (:foreground ,zenburn-yellow :weight bold))))
   ;; `(font-lock-regexp-grouping-backslash ((,class (:foreground ,zenburn-green :weight bold))))
   ;; `(font-lock-string-face ((,class (:foreground ,zenburn-red))))
   ;; `(font-lock-type-face ((,class (:foreground ,zenburn-blue-1))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
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
