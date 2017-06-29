(defun my-color-gruvbox ()
  (interactive)
  (load-theme 'gruvbox t)
  ;;(my-set-font :weight 'normal)
  ;; (set-face-foreground 'font-lock-string-face "salmon")
  ;;(set-face-foreground 'font-lock-comment-face "#66A555")
  (custom-theme-set-faces
   'gruvbox
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   ;; `(font-lock-comment-face
   ;;   ((t (:foreground "#66A555"))))
   `(fringe
     ((t (:foreground "burlywood"
                      :background "black"))))
   ;; `(mode-line
   ;;   ((t (:foreground "#00AF00"         ;"#A08F10"
   ;;                    :background "#150505"
   ;;                    :box (:line-width -1 :style released-button)))
   ;;    (t :inverse-video t)))
   ;; `(mode-line-inactive
   ;;   ((t (:foreground "dark gray"
   ;;                    :background "#051515"
   ;;                    :box (:line-width -1 :style pressed-button)))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "orange red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "plum"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "lawn green"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "white"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "seagreen1"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black"))))))
