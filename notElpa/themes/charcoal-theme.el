;;; charcoal-theme.el --- Chalkish charcoal colors. -*- lexical-binding: t -*-

;;; Commentary:
;;; Supports rgb, 256, 16, and 8 color environments.  The main focus is on the
;;; full 16777216 color rgb version.  Colors are not restricted in the full
;;; rgb version for the sake of consistency with the lesser versions.

;;; Code:

(deftheme charcoal "Charcoal color theme")

(let* ((class        t)
       (cnt          (if (boundp '*charcoal-color-cnt*)
                         ;; use dynamically bound count if available.
                         *charcoal-color-cnt*
                       ;; else detect the color count.
                       (display-color-cells)))
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
       (todo-tbg     "#000000") ; temp color where I haven't decided yet

       ;; Color Palette       full      256       16        8
       (bg           (aref `["#35352B" "#262626" "#000000" "#000000"] i))
       (fg           (aref `["#EEEED1" "#FFFFD7" "#BEBEBE" "#FFFFFF"] i))
       (bg-purple    (aref `["#440033" "#5F005F" "#8B008B" "#000000"] i))
       (fg-purple    (aref `["#FFC0CB" "#FFAFD7" "#FF00FF" "#FF00FF"] i))
       (bg-green     (aref `["#004400" "#005F00" "#228B22" "#000000"] i))
       (fg-green     (aref `["#98FB98" "#00FF87" "#00FF00" "#00FF00"] i))
       (bg-yellow    (aref `["#3A3A00" "#5F5F00" "#A0522D" "#000000"] i))
       (fg-yellow    (aref `["#FFFF00" "#FFFF00" "#FFFFFF" "#FFFF00"] i))
       (faint        (aref `["#4D4D3D" "#303030" "#666666" "#0000FF"] i))
       (faint-less   (aref `["#8D8D8D" "#6C6C6C" "#666666" "#0000FF"] i))
       (keyword      (aref `["#FFEBCD" "#FFAFFF" "#FFFFFF" "#FFFFFF"] i))
       (var          (aref `["#66CDAA" "#D75FAF" "#00CED1" "#00FFFF"] i))
       (highlight    (aref `["#49483E" "#4E4E4E" "#228B22" "#FF00FF"] i))
       (popup-bg     (aref `["#222222" ,todo-tbg "#0000CD" "#FFFF00"] i))
       (scrollb-bg   (aref `["#000000" ,todo-tbg "#666666" "#0000FF"] i))
       (scrollb-fg   (aref `["#999999" ,todo-tmp "#FFFFFF" "#FFFFFF"] i))
       (mode-line-fg (aref `["#8FB28F" ,todo-tmp ,todo-tmp ,todo-tmp] i))
       (mode-line-bg (aref `["#151515" ,todo-tbg ,todo-tbg ,todo-tbg] i))
       (ml-inact-fg  (aref `["#5F7F5F" ,todo-tmp ,todo-tmp ,todo-tmp] i))
       (ml-inact-bg  (aref `["#383838" ,todo-tbg ,todo-tbg ,todo-tbg] i))
       (ml-bufferid  (aref `["#F0DFAF" ,todo-tmp ,todo-tmp ,todo-tmp] i))
       (rain-1       (aref `["#FF4500" ,todo-tmp "#FF0000" "#FF0000"] i))
       (rain-1-bg    (aref `[,bg       ,bg       ,bg       ,bg      ] i))
       (rain-2       (aref `["#00FFFF" ,todo-tmp "#00FFFF" "#00FFFF"] i))
       (rain-2-bg    (aref `[,bg       ,bg       ,bg       ,bg      ] i))
       (rain-3       (aref `["#FFFF00" ,todo-tmp "#A0522D" "#FFFF00"] i))
       (rain-3-bg    (aref `[,bg       ,bg       ,bg       ,bg      ] i))
       (rain-4       (aref `["#DDA0DD" ,todo-tmp "#FF00FF" "#FF00FF"] i))
       (rain-4-bg    (aref `[,bg       ,bg       ,bg       ,bg      ] i))
       (rain-5       (aref `["#7CFC00" ,todo-tmp "#00FF00" "#00FF00"] i))
       (rain-5-bg    (aref `[,bg       ,bg       ,bg       ,bg      ] i))
       (rain-6       (aref `["#FFA500" ,todo-tmp "#FFFFFF" "#FFFFFF"] i))
       (rain-6-bg    (aref `[,bg       ,bg       ,bg       ,bg      ] i))
       (rain-7       (aref `["#FFFFFF" ,todo-tmp "#B22222" "#0000FF"] i))
       (rain-7-bg    (aref `[,bg       ,bg       ,bg       ,bg      ] i))
       (rain-8       (aref `["#FF69B4" ,todo-tmp "#00CED1" "#000000"] i))
       (rain-8-bg    (aref `["#101010" ,todo-tbg ,bg       "#FF00FF"] i))
       (rain-9       (aref `["#CDAA7D" ,todo-tmp "#0000FF" "#000000"] i))
       (rain-9-bg    (aref `[,bg       ,bg       ,bg       "#0000FF"] i))
       (rain-fg-u    (aref `["#A0522D" ,todo-tbg "#FFFFFF" "#FFFFFF"] i))
       (rain-bg-u    (aref `["#000000" ,todo-tmp "#FF0000" "#FF0000"] i)))

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

   `(mode-line
     ((,class (:foreground ,mode-line-fg ;"#8FB28F"
                           :background ,mode-line-bg ;"#2B2B2B"
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((,class (:foreground ,ml-bufferid ;"#F0DFAF"
                                               :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,ml-inact-fg ;"#5F7F5F"
                           :background ,ml-inact-bg ;"#383838"
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

   `(rainbow-delimiters-depth-1-face
     ((,class (:foreground ,rain-1 :background ,rain-1-bg))))
   `(rainbow-delimiters-depth-2-face
     ((,class (:foreground ,rain-2 :background ,rain-2-bg))))
   `(rainbow-delimiters-depth-3-face
     ((,class (:foreground ,rain-3 :background ,rain-3-bg))))
   `(rainbow-delimiters-depth-4-face
     ((,class (:foreground ,rain-4 :background ,rain-4-bg))))
   `(rainbow-delimiters-depth-5-face
     ((,class (:foreground ,rain-5 :background ,rain-5-bg))))
   `(rainbow-delimiters-depth-6-face
     ((,class (:foreground ,rain-6 :background ,rain-6-bg))))
   `(rainbow-delimiters-depth-7-face
     ((,class (:foreground ,rain-7 :background ,rain-7-bg))))
   `(rainbow-delimiters-depth-8-face
     ((,class (:foreground ,rain-8 :background ,rain-8-bg))))
   `(rainbow-delimiters-depth-9-face
     ((,class (:foreground ,rain-9 :background ,rain-9-bg))))
   `(rainbow-delimiters-unmatched-face
     ((,class (:foreground ,rain-fg-u :background ,rain-bg-u))))))

(provide-theme 'charcoal)

;;; charcoal-theme.el ends here
