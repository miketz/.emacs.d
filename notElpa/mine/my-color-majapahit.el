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

   '(aw-leading-char-face               ; ace-window character.
     ((t (:foreground "spring green"
                      :background "black"
                      :height 400       ; big font
                      ))))
   '(cursor ((t (:background "cyan"))))

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
     ((t (:background "#69685E"))))     ;"#49483E"

   `(num3-face-even
     ((t (:underline nil
                     ;; :background "#99988E"
                     ;; :foreground "black"
                     :background "black"
                     :foreground "yellow green"
                     :bold nil))))
   '(leerzeichen ((t (:foreground "yellow4" ;"#A8A800"
                                  :background "black" ;"#D4D4C8"
                                  :italic nil
                                  :bold nil))))
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
   '(rainbow-delimiters-depth-8-face ((t (:foreground "hot pink" :background "#2F2F2F"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black")))))

  ;; remove the bold from rainbow-delimiters.
  ;; (dolist (f (face-list))
  ;;   (when (my-str-starts-with-p (symbol-name f)
  ;;                               "rainbow-delimiters-")
  ;;     (set-face-attribute f
  ;;                         nil ;; all frames
  ;;                         :weight 'normal)))
  )
