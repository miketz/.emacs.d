;;; charcoal-theme.el --- Chalkish charcoal colors. -*- lexical-binding: t -*-

;;; Commentary:
;;; Supports rgb, 256, 16, and 8 color environments.  The main focus is on the
;;; full 16777216 color rgb version.  Colors are not restricted in the full
;;; rgb version for the sake of consistency with the lesser versions.

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
       (todo-tmp     "#FFFFFF") ; temp color where I haven't decided yet

       ;; Color Palette       full      256       16        8
       (bg           (aref `["#35352B" "#262626" "#000000" "#000000"] i))
       (fg           (aref `["#EEEED1" "#FFFFD7" "#BEBEBE" "#FFFFFF"] i))
       (bg-purple    (aref `["#440033" "#5F005F" "#8B008B" ,todo-tmp] i))
       (fg-purple    (aref `["#FFC0CB" "#FFAFD7" "#FF00FF" ,todo-tmp] i))
       (bg-green     (aref `["#004400" ,todo-tmp "#228B22" ,todo-tmp] i))
       (fg-green     (aref `["#98FB98" ,todo-tmp "#00FF00" ,todo-tmp] i))
       (bg-yellow    (aref `["#3A3A00" ,todo-tmp "#A0522D" ,todo-tmp] i))
       (fg-yellow    (aref `["#FFFF00" ,todo-tmp "#FFFFFF" ,todo-tmp] i))
       (faint        (aref `["#4D4D3D" ,todo-tmp "#666666" ,todo-tmp] i))
       (faint-less   (aref `["#8D8D8D" ,todo-tmp "#666666" ,todo-tmp] i))
       (keyword      (aref `["#FFEBCD" ,todo-tmp "#FFFFFF" ,todo-tmp] i))
       (var          (aref `["#66CDAA" ,todo-tmp "#00CED1" ,todo-tmp] i))
       (highlight    (aref `["#49483E" ,todo-tmp "#228B22" ,todo-tmp] i))
       (popup-bg     (aref `["#222222" ,todo-tmp "#0000CD" ,todo-tmp] i))
       (scrollb-bg   (aref `["#000000" ,todo-tmp "#666666" ,todo-tmp] i))
       (scrollb-fg   (aref `["#999999" ,todo-tmp "#FFFFFF" ,todo-tmp] i))
       (rain-1       (aref `["#FF4500" ,todo-tmp "#FF0000" ,todo-tmp] i))
       (rain-2       (aref `["#00FFFF" ,todo-tmp "#00FFFF" ,todo-tmp] i))
       (rain-3       (aref `["#FFFF00" ,todo-tmp "#A0522D" ,todo-tmp] i))
       (rain-4       (aref `["#DDA0DD" ,todo-tmp "#FF00FF" ,todo-tmp] i))
       (rain-5       (aref `["#7CFC00" ,todo-tmp "#00FF00" ,todo-tmp] i))
       (rain-6       (aref `["#FFA500" ,todo-tmp "#FFFFFF" ,todo-tmp] i))
       (rain-7       (aref `["#FFFFFF" ,todo-tmp "#B22222" ,todo-tmp] i))
       (rain-8       (aref `["#FF69B4" ,todo-tmp "#00CED1" ,todo-tmp] i))
       (rain-8-bg    (aref `["#101010" ,todo-tmp ,bg       ,todo-tmp] i))
       (rain-9       (aref `["#CDAA7D" ,todo-tmp "#0000FF" ,todo-tmp] i))
       (rain-fg-u    (aref `["#A0522D" ,todo-tmp "#FFFFFF" ,todo-tmp] i))
       (rain-bg-u    (aref `["#000000" ,todo-tmp "#FF0000" ,todo-tmp] i)))

  ;; lightyellow2=#EEEED1
  ;; snow3 gray80 lightyellow3

  (custom-theme-set-variables
   'charcoal

   `(fci-rule-color ,faint)

   `(evil-emacs-state-cursor    '(bar "cyan"))
   `(evil-normal-state-cursor   '(hollow "spring green"))
   `(evil-insert-state-cursor   '(bar "spring green"))
   `(evil-visual-state-cursor   '(hollow "orange"))
   `(evil-operator-state-cursor '(box "red"))
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
     ((,class (:background ,highlight))))     ;69685E

   ;; TODO
   `(fringe ((,class (:background "black"))))

   ;; TODO font lock
   ;; `(font-lock-builtin-face ((,class (:foreground ,zenburn-fg :weight bold))))
   `(font-lock-comment-face ((,class (:foreground "#8FB28F" :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "medium spring green"))))
   ;; `(font-lock-constant-face ((,class (:foreground ,zenburn-green+4))))
   ;; `(font-lock-doc-face ((,class (:foreground ,zenburn-green+2))))
   ;; `(font-lock-function-name-face ((,class (:foreground ,zenburn-cyan))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword :weight bold))))
   ;; `(font-lock-negation-char-face ((,class (:foreground ,zenburn-yellow :weight bold))))
   ;; `(font-lock-preprocessor-face ((,class (:foreground ,zenburn-blue+1))))
   ;; `(font-lock-regexp-grouping-construct ((,class (:foreground ,zenburn-yellow :weight bold))))
   ;; `(font-lock-regexp-grouping-backslash ((,class (:foreground ,zenburn-green :weight bold))))
   ;; `(font-lock-string-face ((,class (:foreground ,zenburn-red))))
   ;; `(font-lock-type-face ((,class (:foreground ,zenburn-blue-1))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   ;; `(font-lock-warning-face ((,class (:foreground ,zenburn-yellow-2 :weight bold))))

   `(minibuffer-prompt ((,class (:foreground ,fg-purple))))

   ;; default emacs completion.
   `(completions-common-part ((,class (:foreground ,faint-less))))
   `(completions-first-difference ((,class (:foreground ,fg-green))))

   ;; ido
   `(ido-first-match ((,class (:foreground ,fg-yellow :background ,bg-yellow))))
   `(ido-only-match ((,class (:foreground ,fg-green :background ,bg-green))))
   `(ido-subdir ((,class (:foreground ,fg-purple :background ,bg-purple))))


   ;; company
   `(company-tooltip ((,class (:background ,popup-bg :foreground ,fg))))
   `(company-scrollbar-bg ((,class (:background ,scrollb-bg))))
   `(company-scrollbar-fg ((,class (:background ,scrollb-fg))))
   `(company-tooltip-common ((,class (:foreground ,faint-less))))
   `(company-tooltip-selection ((,class (:background ,highlight))))
   ;; company-echo
   ;; company-echo-common
   ;; company-preview
   ;; company-preview-common
   ;; company-preview-search
   ;; company-template-field
   ;; company-tooltip-annotation
   ;; company-tooltip-annotation-selection
   ;; company-tooltip-common-selection
   ;; company-tooltip-mouse
   ;; company-tooltip-search
   ;; company-tooltip-search-selection

   ;; TODO rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,rain-1))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,rain-2))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,rain-3))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,rain-4))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,rain-5))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,rain-6))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,rain-7))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,rain-8 :background ,rain-8-bg))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,rain-9))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,rain-fg-u :background ,rain-bg-u))))))

(provide-theme 'charcoal)

;;; charcoal-theme.el ends here
