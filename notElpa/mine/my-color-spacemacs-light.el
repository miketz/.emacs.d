(defun my-color-spacemacs-light ()
  (interactive)
  (load-theme 'spacemacs-light t)

  (custom-theme-set-variables
   'spacemacs-light
   `(fci-rule-color "#4d4d4d"))

  (my-rainbow-parens-light-bg 'spacemacs-light)

  (custom-theme-set-faces
   'spacemacs-light

   `(default ((t (:foreground "black" :background "#fffeFa"))))
   ;; `(default ((t (:foreground "black"))))

   '(highlight-indent-guides-odd-face
     ((t (:background "floral white"))))

   '(highlight-indent-guides-even-face
     ((t (:inherit default))))

   `(whitespace-space-before-tab ((t (:background "orange"))))

   `(fringe ((t (:background "gray96"))))

   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))

   `(font-lock-string-face ((t (:foreground "deeppink"))))
   `(font-lock-constant-face ((t (:foreground "darkgreen" :background "lemonchiffon" :slant italic)))) ;; `(font-lock-comment-face ((t (:foreground "orangered" :background nil :slant italic))))
   `(font-lock-comment-face ((t (:foreground "#05b505" :background nil :slant italic))))
   ;; `(font-lock-comment-delimiter-face ((t (:foreground "purple" :background nil :slant italic))))

   `(mode-line ((t (:foreground "#655370" :background "#e7e5eb" :box (:color "#b3b9be"  :line-width -1)))))
   `(mode-line-inactive ((t (:foreground "#655370" :background "#fbf8ef" :box (:color "#b3b9be" :line-width -1)))))
   ;; `(mode-line ((t (:box (:line-width -1 :style released-button)))))
   ;; `(mode-line-inactive ((t (:box (:line-width -1 :sytle pressed-button)))))
   ))
