(defun my-color-dichromacy ()
  (interactive)
  (load-theme 'dichromacy t)
  ;; (my-cursor-stuff :color-emacs "red" :color-evil "blue")

  (my-rainbow-parens-light-bg2 'dichromacy)

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
                                  :background nil)))))

  ;; (my-set-font :weight 'bold
  ;;              :height 140)
  ;;(set-background-color "floral white")
  )
