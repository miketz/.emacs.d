;;; -*- lexical-binding: t -*-

(defun my-color-tommyh ()
  (interactive)
  (load-theme 'tommyh t)

  (my-rainbow-parens-light-bg2 'tommyh)

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
                                  :background "yellow"))))))
