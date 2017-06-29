(defun my-color-gruvbox ()
  (interactive)
  (load-theme 'gruvbox t)

  (my-rainbow-parens-dark-bg 'gruvbox)

  (custom-theme-set-faces
   'gruvbox
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   `(fringe
     ((t (:foreground "burlywood"
                      :background "black"))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))))
