(defun my-color-gruvbox ()
  (interactive)
  (load-theme 'gruvbox t)
  ;;(my-set-font :weight 'normal)
  (my-cursor-stuff-darkBg)
  (my-rainbow-parens-dark-bg)
  ;; (set-face-foreground 'font-lock-string-face "salmon")
  ;;(set-face-foreground 'font-lock-comment-face "#66A555")
  (custom-theme-set-faces
   'gruvbox
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   `(font-lock-comment-face
     ((t (:foreground "#66A555"))))
   `(fringe
     ((t (:foreground "burlywood"
                      :background "black"))))
   `(mode-line
     ((t (:foreground "#00AF00";"#A08F10"
                      :background "#150505"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "dark gray"
                      :background "#051515"
                      :box (:line-width -1 :style pressed-button)))))))