(defun my-color-leuven ()
  (interactive)
  (load-theme 'leuven t)
  (custom-theme-set-faces
   'leuven
   `(default ((t (:foreground "black" :background ,mayan-smoke))))
   ;;`(default ((t (:foreground "black" :background ,"white"))))
   `(mode-line ((t (:box (:line-width -1 :color "#1A2F54")
                         :foreground "#85CEEB" :background "#335EA8"
                         :style released-button))))
   `(mode-line-inactive ((t (:box (:line-width -1 :color "#4E4E4C")
                                  :foreground "#F0F0EF" :background "#9B9C97"
                                  :style released-button))))
   '(js2-function-call ((t :foreground "blue")))
   '(leerzeichen ((t (:foreground "black";"#A8A800"
                                  :background "white";"#D4D4C8"
                                  :italic nil
                                  :bold nil
                                  ;;:box t
                                  ))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "black"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "light cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background "#faEaEa"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "lavenderblush"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "lemon chiffon"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background "#EEEEFF"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "#fff7ca"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black"))))))