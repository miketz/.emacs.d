(defun my-color-github ()
  (interactive)
  (load-theme 'github t)

  ;; (my-cursor-stuff :color-emacs "maroon" :color-evil "blue")
  ;; (let ((cur '(box "blue")))
  ;;   (setq evil-normal-state-cursor cur)
  ;;   (setq evil-visual-state-cursor '(hollow "blue"))
  ;;   (setq evil-operator-state-cursor cur))


  ;; (set-background-color "white")
  (custom-theme-set-faces
   'github
   `(mode-line ((t (:background "grey75"
                                :foreground "black"
                                :box (:line-width -1 :style released-button)
                                :height 1.0))))

   `(avy-lead-face
     ((t (:foreground "black"
                      :background "yellow"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))

   '(fringe
     ((t (:foreground "#9B9B9B" :background "gray90"))))

   `(ace-jump-face-foreground
     ((t (:foreground "yellow"
                      :background "black"
                      :slant normal
                      :weight bold
                      :inverse-video nil))))

   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   ;; '(js2-external-variable ((t :underline (:color "red" :style wave)
   ;;                             :background "black")))
   ;; '(js2-error ((t :underline (:color "red" :style wave)
   ;;                             :background "dark red")))
   '(js2-function-call ((t :foreground "#990000"))) ;;making same as font-lock-function-name-face
   ;; '(js2-warning ((t :underline (:color "yellow" :style wave)
   ;;                   :background "navy blue")))
   ;;'(js2-private-member ((t :foreground "green")))
   ;;'(js2-function-param ((t :foreground "green")))
   ;;'(js2-instance-member ((t :foreground "green")))
   ;;'(js2-private-function-call ((t :foreground "green")))

   '(rainbow-delimiters-depth-1-face ((t (:foreground "black" :background "gray94" :weight normal))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "light cyan" :bold nil))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background "#faEaEa" :bold nil))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "lavenderblush" :bold nil))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "lemon chiffon" :bold nil))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background "#EEEEFF" :bold nil))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52" :bold nil))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3" :bold nil))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "#fff7ca" :bold nil))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black" :bold nil))))))
