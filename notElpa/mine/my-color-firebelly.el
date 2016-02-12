(defun my-color-firebelly ()
  (interactive)
  (my-load-theme-make-bold-like-zenburn 'firebelly) ;;(load-theme 'firebelly t)
  ;; fci color is not a face???
  (setq fci-rule-color "#343434")
  (custom-theme-set-faces
   'firebelly

   '(highlight-indent-guides-odd-face
     ((t (:background "#242424"
                      :weight bold))))
   '(highlight-indent-guides-even-face
     ((t (:background "#202020"
                      :weight bold))))

   ;; override the hi-yellow face for printf escapes.
   '(hi-yellow
     ((t (:foreground "hotpink"
                      :weight bold))))

   `(org-level-2 ((t (:foreground "gray"))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   '(sldb-section-face
     ((t (:foreground "light sky blue"))))
   ;; swiper/ivy faces TODO: fix more ivy/swiper faces.
   '(ivy-current-match
     ((t (:background "black" :foreground "light sky blue"))))

   '(font-lock-variable-name-face
     ((t (:foreground "#924040"))))
   '(my-tilde-face
     ((t (:foreground "#523030"))))

   `(font-lock-comment-face
     ((t (:foreground "RosyBrown4" :slant italic))))
   `(font-lock-comment-delimiter-face
     ((t (:foreground "RosyBrown3"))))

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