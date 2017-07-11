(defun my-color-gruvbox-dark ()
  (interactive)
  (load-theme 'gruvbox-dark t)

  (custom-theme-set-variables
   'gruvbox-dark
   `(fci-rule-color "#4d4d4d")

   `(evil-emacs-state-cursor '(bar "cyan"))
   `(evil-normal-state-cursor '(hollow "spring green"))
   `(evil-insert-state-cursor '(bar "spring green"))
   `(evil-visual-state-cursor '(hollow "yellow"))
   `(evil-operator-state-cursor '(hollow "spring green"))
   `(evil-replace-state-cursor '(hbar "spring green"))
   `(evil-motion-state-cursor '(box "spring green")))

  (custom-theme-set-faces
   'gruvbox-dark
   `(region ((t (:background "#59584E"))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))))
