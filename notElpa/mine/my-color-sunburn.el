(defun my-color-sunburn ()
  (interactive)
  (load-theme 'sunburn t)
  (my-rainbow-parens-dark-bg 'sunburn)

  (custom-theme-set-variables
   'sunburn

   `(fci-rule-color "#4d4d4d")

   `(evil-emacs-state-cursor    '(bar "cyan"))
   `(evil-normal-state-cursor   '(hollow "spring green"))
   `(evil-insert-state-cursor   '(bar "spring green"))
   `(evil-visual-state-cursor   '(hollow "orange"))
   `(evil-operator-state-cursor '(hollow "spring green"))
   `(evil-replace-state-cursor  '(hbar "spring green"))
   `(evil-motion-state-cursor   '(box "spring green")))

  (custom-theme-set-faces
   'sunburn

   `(region ((t (:background "#69685E"))))))
