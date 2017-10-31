;;; -*- lexical-binding: t -*-

(defun my-color-deeper-blue ()
  (interactive)
  (load-theme 'deeper-blue t)
  ;; (when my-use-evil-p
  ;;   (my-cursor-stuff-darkBg)) ;;TODO: move into `custom-set-faces'

  (my-rainbow-parens-dark-bg 'deeper-blue)

  ;;wrap mods in `custom-theme-set-faces' so they can be rolled back with `disable-theme'
  (custom-theme-set-faces
   'deeper-blue


   '(leerzeichen ((t (:foreground "gray40"           ;"#A8A800"
                                  :background "black" ;"#D4D4C8"
                                  :italic nil
                                  :bold nil))))

   ;; separates windwos.
   '(vertical-border ((t (:foreground "gray20"))))

   '(my-tilde-face
     ((t (:foreground "gray35"))))

   '(org-agenda-calendar-event ((t (:background "black"))))

   ;; `(mode-line ((t (:background "gray40"
   ;;                              :foreground "black"
   ;;                              :box (:line-width -1
   ;;                                                :style released-button)))))
   `(mode-line ((t (:background "black"
                                              :foreground "slategray"
                                              :box (:line-width -1
                                                                :color "gray20"
                                                                :style released-button)))))
   `(mode-line-buffer-id ((t (:weight bold
                                           :background nil
                                           :foreground "slategray"))))
   `(mode-line-inactive ((t (:background "black"
                                              :foreground "gray30"
                                              :box (:line-width -1
                                                                :color "black"
                                                                :style released-button)))))


   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))))
