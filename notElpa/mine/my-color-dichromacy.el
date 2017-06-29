(defun my-color-dichromacy ()
  (interactive)
  (load-theme 'dichromacy t)
  ;; (my-cursor-stuff :color-emacs "red" :color-evil "blue")
  (custom-theme-set-faces
   'dichromacy
   ;;`(default ((t (:foreground "black" :background ,mayan-smoke))))
   `(default ((t (:foreground "black" :background ,"white"))))
   `(mode-line
     ((t (:foreground "black"
                      :background "#CCCCCC"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "#666666"
                      :background "#EEEEEE"
                      :box (:line-width -1 :style released-button)))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "black"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "light cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background "#faEaEa"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "lavenderblush"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "lemon chiffon"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background "#EEEEFF"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "#fff7ca"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black")))))

  ;; (my-set-font :weight 'bold
  ;;              :height 140)
  ;;(set-background-color "floral white")
  )
