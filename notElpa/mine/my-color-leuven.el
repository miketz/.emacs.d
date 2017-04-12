(defun my-color-leuven ()
  (interactive)
  (load-theme 'leuven t)
  (my-cursor-stuff :color-emacs "maroon" :color-evil "blue")
  (let ((cur '(box "blue")))
    (setq evil-normal-state-cursor cur)
    (setq evil-visual-state-cursor '(hollow "blue"))
    (setq evil-operator-state-cursor cur))

  (custom-theme-set-faces
   'leuven
   ;; `(default ((t (:foreground "black" :background ,mayan-smoke))))
   `(default ((t (:foreground "black"))))
   ;;`(default ((t (:foreground "black" :background ,"white"))))
   `(mode-line ((t (:box (:line-width -1 :color "#1A2F54")
                         :foreground "#85CEEB" :background "#335EA8"
                         :style released-button))))
   `(mode-line-inactive ((t (:box (:line-width -1 :color "#4E4E4C")
                                  :foreground "#F0F0EF" :background "#9B9C97"
                                  :style released-button))))


   '(js2-function-call ((t :foreground "blue"
                           :background "alice blue")))
   '(js2-external-variable ((t :underline (:color "black" :style wave)
                               ;; :background "yellow"
                               )))
   `(js2-warning ((t :underline (:color "black" :style wave)
                     :strike-through t
                     :background "lemonchiffon"
                     )))
   `(js2-error ((t :underline (:color "blue" :style wave)
                   :strike-through t
                   :background "light salmon")))


   '(erc-timestamp-face ((t :foreground "purple" :weight bold)))
   '(fringe
     ((t (:foreground "#9B9B9B" :background "alice blue"))))
   '(leerzeichen ((t (:foreground "black" ;"#A8A800"
                                  :background "white" ;"#D4D4C8"
                                  :italic nil
                                  :bold nil
                                  ;;:box t
                                  ))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))

   `(font-lock-function-name-face ((t (:weight bold :foreground "#006699"))))
   `(font-lock-keyword-face ((t (:bold t :foreground "#0000FF")))) ; #3654DC
   `(font-lock-builtin-face ((t (:weight bold :foreground "#006FE0"))))

   `(avy-lead-face ;; if 1 highlight char, or for remaining highlight chars.
     ((t (:foreground "black"
                      :background "yellow"
                      :slant normal
                      :weight normal
                      :strike-through nil
                      :underline nil))))

   `(avy-lead-face-0 ;; the first overlay char if 2+
     ((t (:foreground "black"
                      :background "spring green"
                      :slant normal
                      :weight normal
                      :strike-through nil
                      :underline nil))))
   `(avy-lead-face-1 ;; ??? Maybe the 2cd overlay char if 3+ ???
     ((t (:foreground "black"
                      :background "red"
                      :slant normal
                      :weight normal
                      :strike-through nil
                      :underline nil))))

   ;; '(rainbow-delimiters-depth-1-face ((t (:foreground "black" :background "gray94" :weight bold))))
   ;; '(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "light cyan" :weight bold))))
   ;; '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background "#faEaEa" :weight bold))))
   ;; '(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "lavenderblush" :weight bold))))
   ;; '(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "lemon chiffon" :weight bold))))
   ;; '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background "#EEEEFF" :weight bold))))
   ;; '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52" :weight bold))))
   ;; '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3" :weight bold))))
   ;; '(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "#fff7ca" :weight bold))))
   ;; '(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black" :weight bold))))
   )
  (my-rainbow-parens-light-bg2))
