(defun my-color-github ()
  (interactive)
  (load-theme 'github t)
  (set-background-color "white")
  (custom-theme-set-faces
   'github
   `(mode-line ((t (:background "grey75"
                                :foreground "black"
                                :box (:line-width -1 :style released-button)
                                :height 1.0))))
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
   ))