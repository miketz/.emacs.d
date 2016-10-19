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

   `(default ((t (:foreground "black" :background "white"))))

   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))

   `(mode-line ((t (:foreground "#655370" :background "#e7e5eb" :box (:color "#b3b9be"  :line-width -1)))))
   `(mode-line-inactive ((t (:foreground "#655370" :background "#fbf8ef" :box (:color "#b3b9be" :line-width -1)))))

   ;; `(mode-line ((t (:box (:line-width -1
   ;;                                    :style released-button)))))
   ;; `(mode-line-inactive ((t (:box (:line-width -1
   ;;                                             :sytle pressed-button)))))
   ))