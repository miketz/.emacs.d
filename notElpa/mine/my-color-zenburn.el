(defun my-color-zenburn ()
  "Load the zenburn theme created by Bozhidar Batsov.  Make a few extra mods too."
  (interactive)
  (load-theme 'zenburn t)

  ;; set variables (that are not faces) using `custom-theme-set-variables'. It
  ;; will allow them to be rolled back automatically when the theme is later
  ;; disabled.
  (custom-theme-set-variables
   'zenburn
   `(fci-rule-color "#4d4d4d")

   `(evil-emacs-state-cursor    '(bar "cyan"))
   `(evil-normal-state-cursor   '(hollow "spring green"))
   `(evil-insert-state-cursor   '(bar "spring green"))
   `(evil-visual-state-cursor   '(hollow "orange"))
   `(evil-operator-state-cursor '(hollow "spring green"))
   `(evil-replace-state-cursor  '(hbar "spring green"))
   `(evil-motion-state-cursor   '(box "spring green")))


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

   ;; `(header-line
   ;;   ((t (:foreground "lemon chiffon"
   ;;                    :background "#303030"
   ;;                    :weight bold
   ;;                    :box (:line-width -1 :style released-button)))))


   '(completions-common-part ((t (:foreground "gray60"))))
   '(completions-first-difference ((t (:foreground "spring green"))))

   ;; used in adoc-mode
   '(markup-meta-face ((t (:foreground "yellow green"))))
   '(markup-internal-reference-face ((t (:foreground "yellow green"))))
   '(markup-meta-hide-face ((t (:foreground "powder blue"))))

   '(cider-fringe-good-face ((t (:foreground "lime green"))))
   '(cider-result-overlay-face ((t (:foreground "orange"))))

   '(org-agenda-calendar-event ((t (:background "black"))))

   ;; separates windows.
   '(vertical-border ((t (:foreground "gray50"))))

   '(highlight-indent-guides-odd-face
     ((t (:background "#3F3F3C" ;; "#40403A"
                      :weight bold))))

   '(highlight-indent-guides-even-face
     ((t (:background "#3D3D3D" ;; "#3B3B3B"
                      :weight bold))))

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

   ;; used by lisp doc strings
   ;; `(font-lock-doc-face ((t (:inherit font-lock-keyword-face :weight normal))))
   ;; `(font-lock-doc-face ((t (:foreground "moccasin"))))

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
                      :background "black"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(avy-lead-face-1 ;; for matched chars, but currently not used???? matches disapear at the moment.
     ((t (:foreground "green"
                      :background "black"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(avy-lead-face ;;for the chars after the first?
     ((t (:foreground "spring green"
                      :background "black"
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
     ((t (:background "#69685E"))))     ;"#49483E"
   ;; '(region ((t :background "black")))
   '(isearch ((t :background "yellow"
                 :foreground "black"
                 :bold t
                 :underline nil)))
   ;; the non-selected matches from isearch
   '(lazy-highlight ((t
                      :background "black"
                      :foreground "cyan"
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

   ;; '(num3-face-odd ((t)))
   ;; '(num3-face-even ((t (:underline t :background "black"))))
   `(num3-face-even
     ((t (:underline nil
                     ;; :background "#99988E"
                     ;; :foreground "black"
                     :background "black"
                     :foreground "yellow green"
                     :bold nil))))
   '(leerzeichen ((t (:foreground "yellow4"           ;"#A8A800"
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
   ;;'(rainbow-delimiters-depth-8-face ((t (:foreground "seagreen1"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "hot pink" :background "#2F2F2F"
                                                      ))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black"))))))
