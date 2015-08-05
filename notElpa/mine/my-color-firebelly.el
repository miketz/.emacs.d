(defun my-color-firebelly ()
  (interactive)
  (load-theme 'firebelly t)
  (custom-theme-set-faces
   'firebelly

   '(my-tilde-face
     ((t (:foreground "#523030"))))

   `(font-lock-comment-face
     ((t (:foreground "RosyBrown4" :slant italic))))
   `(font-lock-comment-delimiter-face
     ((t (:foreground "RosyBrown4"))))

   `(fringe
     ((t (:foreground "burlywood"
                      :background "black"))))
   `(region
     ((t (:background "#494949"))))     ;"#49483E"

   '(rainbow-delimiters-depth-1-face ((t (:foreground "orange red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "plum"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "lawn green"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "white"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "yellow" :background "black"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black"))))))