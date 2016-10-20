(defun my-color-spacemacs-light ()
  (interactive)
  (load-theme 'spacemacs-light t)

  (my-cursor-stuff :color-emacs "maroon" :color-evil "blue")
  (let ((cur '(box "blue")))
    (setq evil-normal-state-cursor cur)
    (setq evil-visual-state-cursor '(hollow "blue"))
    (setq evil-operator-state-cursor cur))

  (custom-theme-set-faces
   'spacemacs-light

   ;; `(default ((t (:foreground "black" :background "white"))))
   `(default ((t (:foreground "black"))))

   `(whitespace-space-before-tab ((t (:background "orange"))))

   `(fringe ((t (:background "gray96"))))

   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))

   `(font-lock-constant-face ((t (:foreground "orangered" :background "lemonchiffon" :slant italic))))
   `(font-lock-comment-face ((t (:foreground "orangered" :background nil :slant italic))))
   ;; `(font-lock-comment-face ((t (:foreground "purple" :background nil :slant italic))))
   ;; `(font-lock-comment-delimiter-face ((t (:foreground "purple" :background nil :slant italic))))

   `(mode-line ((t (:foreground "#655370" :background "#e7e5eb" :box (:color "#b3b9be"  :line-width -1)))))
   `(mode-line-inactive ((t (:foreground "#655370" :background "#fbf8ef" :box (:color "#b3b9be" :line-width -1)))))
   ;; `(mode-line ((t (:box (:line-width -1 :style released-button)))))
   ;; `(mode-line-inactive ((t (:box (:line-width -1 :sytle pressed-button)))))
   ))