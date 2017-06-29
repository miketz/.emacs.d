(defun my-color-gandalf ()
  (interactive)
  (load-theme 'gandalf t)
  ;; (my-cursor-stuff-lightBg)
  ;; (let ((cur '(box "blue")))
  ;;   (setq evil-normal-state-cursor cur)
  ;;   (setq evil-visual-state-cursor '(hollow "blue"))
  ;;   (setq evil-operator-state-cursor cur))

  (my-rainbow-parens-light-bg2 'gandalf)

  (custom-theme-set-faces
   'gandalf
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   `(region
     ((t (:background "goldenrod"))))
   `(fringe
     ((t (:foreground "black"
                      :background "gray"))))

   `(ace-jump-face-foreground
     ((t (:foreground "black"
                      :background "yellow"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(mode-line
     ((t (:foreground "black"
                      :background "gray"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "#505050"
                      :background "#e3e3e3"
                      :box (:line-width -1 :style released-button)))))

   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))))
