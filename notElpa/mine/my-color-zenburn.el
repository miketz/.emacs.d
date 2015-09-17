(defun my-color-zenburn ()
  "Load the zenburn theme created by Bozhidar Batsov.  Make a few extra mods too."
  (interactive)
  (load-theme 'zenburn t)
  (when my-use-evil-p
    (my-cursor-stuff-darkBg)) ;;TODO: move into `custom-set-faces'

  ;;wrap mods in `custom-theme-set-faces' so they can be rolled back with `disable-theme'
  (custom-theme-set-faces
   'zenburn
   ;;from VIM charcoal: hi Normal guifg=#ADC299 guibg=#35352B "*
   ;;`(default ((t (:foreground "#CFC5A9" :background "#35352B"))))
   ;;`(default ((t (:foreground "#CCCCBC" :background "#35352B"))))

   ;;'(cursor ((t (:foreground "blue" :background "red"))))
   ;; `(mode-line
   ;;   ((t (:foreground "#8FB28F"
   ;;                    :background "#032203"
   ;;                    ;;:underline "dark yellow"
   ;;                    ;;:overline "green"
   ;;                    :box (:line-width -1 :style released-button)))
   ;;    (t :inverse-video t)))

   ;; '(hl-line
   ;;   ((t (:background "black"))))

   ;; '(minibuffer-prompt
   ;;   ((t (:foreground "spring green"))))

   ;; swiper/ivy faces
   '(ivy-current-match
     ((t (:background "black" :foreground "light sky blue"))))

   '(js2-highlight-vars-face
     ((t (:background "#69685E"))))

   '(slime-repl-inputed-output-face
     ((t (:foreground "light sky blue"))))
   '(nxml-tag-delimiter
     ((t (:foreground "#83D0FF" :weight bold))))
   '(nxml-element-local-name
     ((t (:foreground "#93E0E3" :weight bold))))
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
   ;; `(avy-background-face
   ;;   ((t (:foreground "white"
   ;;                    :slant normal
   ;;                    :weight normal
   ;;                    :inverse-video nil))))

   ;;highlight so i can see the slime function parameters highlight.
   '(highlight ((t (:foreground "spring green"
                                :background "black"))))

   `(region
     ((t (:background "#69685E"))));"#49483E"
   ;; '(region ((t :background "black")))
   '(isearch ((t :background "black"
                 :foreground "yellow"
                 :bold nil
                 :underline t)))
   ;;This isn't a js2 error face. But externals tend to be an error since js2 doesn't find the them.
   '(js2-external-variable ((t :underline (:color "red" :style wave)
                               :background "black")))
   '(js2-error ((t :underline (:color "red" :style wave)
                   :background "dark red")))
   '(js2-function-call ((t :foreground "#93E0E3"))) ;;making same as font-lock-function-name-face
   '(js2-warning ((t :underline (:color "yellow" :style wave)
                     :background "navy blue")))

   ;;'(js2-private-member ((t :foreground "green")))
   ;;'(js2-function-param ((t :foreground "green")))
   ;;'(js2-instance-member ((t :foreground "green")))
   ;;'(js2-private-function-call ((t :foreground "green")))

   '(lazy-highlight ((t
                      :background "black"
                      :foreground "cyan"
                      :bold nil
                      :underline t)))
   ;; '(num3-face-odd ((t)))
   ;; '(num3-face-even ((t (:underline t :background "black"))))
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
                                  :background nil))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "orange red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "plum"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "lawn green"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "white"))))
   ;;'(rainbow-delimiters-depth-8-face ((t (:foreground "seagreen1"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "hot pink" :background "#2F2F2F"
                                                      ))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black"))))))
