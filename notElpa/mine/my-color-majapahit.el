;;; -*- lexical-binding: t -*-

(defun my-color-majapahit ()
  (interactive)
  (load-theme 'majapahit-dark t)

  (my-rainbow-parens-dark-bg 'majapahit-dark)

  ;;wrap mods in `custom-theme-set-faces' so they can be rolled back with `disable-theme'
  (custom-theme-set-faces
   'majapahit-dark

   `(mode-line
     ((t (:foreground "#F8F8F2"
                      :background "#3E3D31"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "#75715E"
                      :background "#272822"
                      :box (:line-width -1 :style released-button)))))

   '(isearch ((t :background "black"
                 :foreground "yellow"
                 :bold nil
                 :underline t)))
   '(lazy-highlight ((t
                      :background "black"
                      :foreground "cyan"
                      :bold nil
                      :underline t)))

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
   ;; '(cursor ((t (:background "cyan"))))

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

   `(fringe
     ((t (:foreground "#eddcca"
                      :background "black"))))

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
                                  :background nil)))))

  ;; remove the bold from rainbow-delimiters.
  ;; (dolist (f (face-list))
  ;;   (when (my-str-starts-with-p (symbol-name f)
  ;;                               "rainbow-delimiters-")
  ;;     (set-face-attribute f
  ;;                         nil ;; all frames
  ;;                         :weight 'normal)))
  )
