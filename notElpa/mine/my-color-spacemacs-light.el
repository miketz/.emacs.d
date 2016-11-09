(defun my-color-spacemacs-light ()
  (interactive)
  (load-theme 'spacemacs-light t)

  (my-cursor-stuff :color-emacs "maroon" :color-evil "blue")
  (let ((cur '(box "blue")))
    (setq evil-normal-state-cursor cur)
    (setq evil-visual-state-cursor '(hollow "blue"))
    (setq evil-operator-state-cursor cur))

  ;; fci color is not a face???
  (setq fci-rule-color "gray88")

  (custom-theme-set-faces
   'spacemacs-light

   `(default ((t (:foreground "black" :background "#fffeFa"))))
   ;; `(default ((t (:foreground "black"))))

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

   '(rainbow-delimiters-depth-1-face ((t (:foreground "black"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#09a509"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#3388ff"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "black" :background "red"))))

   ))