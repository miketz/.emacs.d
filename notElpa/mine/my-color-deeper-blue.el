(defun my-color-deeper-blue ()
  (interactive)
  (load-theme 'deeper-blue t)
  (when my-use-evil-p
    (my-cursor-stuff-darkBg)) ;;TODO: move into `custom-set-faces'

  ;;wrap mods in `custom-theme-set-faces' so they can be rolled back with `disable-theme'
  (custom-theme-set-faces
   'deeper-blue


   '(leerzeichen ((t (:foreground "gray40"           ;"#A8A800"
                                  :background "black" ;"#D4D4C8"
                                  :italic nil
                                  :bold nil))))

   ;; separates windwos.
   '(vertical-border ((t (:foreground "gray20"))))

   '(my-tilde-face
     ((t (:foreground "gray35"))))

   '(org-agenda-calendar-event ((t (:background "black"))))

   ;; `(mode-line ((t (:background "gray40"
   ;;                              :foreground "black"
   ;;                              :box (:line-width -1
   ;;                                                :style released-button)))))
   `(mode-line ((t (:background "black"
                                              :foreground "slategray"
                                              :box (:line-width -1
                                                                :color "gray20"
                                                                :style released-button)))))
   `(mode-line-buffer-id ((t (:weight bold
                                           :background nil
                                           :foreground "slategray"))))
   `(mode-line-inactive ((t (:background "black"
                                              :foreground "gray30"
                                              :box (:line-width -1
                                                                :color "black"
                                                                :style released-button)))))


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
   ;;'(rainbow-delimiters-depth-8-face ((t (:foreground "seagreen1"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "hot pink" :background "#2F2F2F"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black"))))))