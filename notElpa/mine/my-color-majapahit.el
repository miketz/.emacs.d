(defun my-color-majapahit ()
  (interactive)
  (load-theme 'majapahit-dark t)
  (when my-use-evil-p
    (my-cursor-stuff-darkBg)) ;;TODO: move into `custom-set-faces'

  ;;wrap mods in `custom-theme-set-faces' so they can be rolled back with `disable-theme'
  (custom-theme-set-faces
   'majapahit-dark

   ;; override the hi-yellow face for printf escapes.
   '(hi-yellow
     ((t (:foreground "hotpink"
                      :weight bold))))

   '(sr-highlight-path-face
     ((t (:background "black" :foreground "light sky blue"
                      :bold t
                      :height 120))))
   '(sr-active-path-face
     ((t (:background "black" :foreground "light sky blue"
                      :bold t
                      :height 120))))
   '(sr-passive-path-face
     ((t (:background "black" :foreground "gray"
                      :bold t
                      :height 120))))

   ;; swiper/ivy faces
   '(ivy-current-match
     ((t (:background "black" :foreground "light sky blue"))))

   '(js2-highlight-vars-face
     ((t (:background "#69685E"))))

   '(slime-repl-inputed-output-face
     ((t (:foreground "light sky blue"))))

   '(org-hide
     ((t (:foreground "#5F5F5F"))))
   '(my-tilde-face
     ((t (:foreground "dark gray"))))
   '(aw-leading-char-face               ; ace-window character.
     ((t (:foreground "spring green"
                      :background "black"
                      :height 400       ; big font
                      ))))
   '(cursor ((t (:background "cyan"))))
   '(hydra-face-red
     ((t (:foreground "green" :bold t))))
   '(hydra-face-blue
     ((t (:foreground "cyan" :bold t))))
   '(hydra-face-amaranth
     ((t (:foreground "green" :bold t))))

   `(font-lock-comment-face
     ((t (:foreground "#8FB28F" :slant italic))))
   `(font-lock-comment-delimiter-face
     ((t (:foreground "darkolivegreen3"))))

   `(ace-jump-face-foreground
     ((t (:foreground "spring green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(avy-lead-face-0 ;; the first overlay char
     ((t (:foreground "spring green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(avy-lead-face-1 ;; for matched chars, but currently not used???? matches disapear at the moment.
     ((t (:foreground "green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(avy-lead-face ;;for the chars after the first?
     ((t (:foreground "spring green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))


   `(region
     ((t (:background "#69685E"))));"#49483E"

   `(num3-face-even
     ((t (:underline nil
                     ;; :background "#99988E"
                     ;; :foreground "black"
                     :background "black"
                     :foreground "yellow green"
                     :bold nil))))
   '(leerzeichen ((t (:foreground "yellow4";"#A8A800"
                                  :background "black";"#D4D4C8"
                                  :italic nil
                                  :bold nil))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))))
