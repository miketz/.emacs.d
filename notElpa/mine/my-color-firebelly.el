(defun my-color-firebelly ()
  (interactive)
  (my-load-theme-make-bold-like-zenburn 'firebelly) ;;(load-theme 'firebelly t)

  (custom-theme-set-variables
   'firebelly
   `(fci-rule-color "#343434"))

  (my-rainbow-parens-dark-bg 'firebelly)

  (custom-theme-set-faces
   'firebelly

   `(avy-lead-face
     ((t (:foreground "orange"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))

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
   ))
