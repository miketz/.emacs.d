(defun my-color-gandalf ()
  (interactive)
  (load-theme 'gandalf t)
  ;; (my-cursor-stuff-lightBg)
  ;; (let ((cur '(box "blue")))
  ;;   (setq evil-normal-state-cursor cur)
  ;;   (setq evil-visual-state-cursor '(hollow "blue"))
  ;;   (setq evil-operator-state-cursor cur))
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
