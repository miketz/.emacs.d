(defun my-color-badger ()
  (interactive)
  (load-theme 'badger t)
  (custom-theme-set-faces
   'badger
   ;; separates windows.
   '(vertical-border ((t (:foreground "gray15"))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   `(font-lock-comment-face
     ((t (:foreground "dark cyan" :slant italic))))
   '(region ((t :background "#7F073F")))
   `(mode-line
     ((t (:foreground "#00AF00"
                      :background "#150505"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "dark gray"
                      :background  "#090202";"#051515"
                      :box (:line-width -1 :style pressed-button)))))))