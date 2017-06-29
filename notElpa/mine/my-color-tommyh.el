(defun my-color-tommyh ()
  (interactive)
  (load-theme 'tommyh t)
  ;; (my-cursor-stuff-lightBg)
  ;; (let ((cur '(box "blue")))
  ;;   (setq evil-normal-state-cursor cur)
  ;;   (setq evil-visual-state-cursor '(hollow "blue"))
  ;;   (setq evil-operator-state-cursor cur))
  (custom-theme-set-faces
   'tommyh

   '(erc-timestamp-face
     ((t (:foreground "#b8574e"))))
   '(my-tilde-face
     ((t (:foreground "black"))))
   '(fringe
     ((t (:foreground "black"
                      :background "#74a6bd"))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background "yellow"))))
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
