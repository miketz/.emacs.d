;;; -*- lexical-binding: t -*-

(defun my-color-tango-dark ()
  (interactive)
  (load-theme 'tango-dark t)

  (my-rainbow-parens-dark-bg 'tango-dark)

  ;;wrap mods in `custom-theme-set-faces' so they can be rolled back with `disable-theme'
  (custom-theme-set-faces
   'tango-dark

   `(mode-line ((t (:foreground "#aaaaaa"
                                :background "#331133"
                                :box (:line-width -1 :style released-button)))))
   ;;colors copied from grandshell-theme.el
   `(mode-line-inactive ((t (:foreground "#643"
                                         :background "#110011"
                                         :weight light
                                         :box (:line-width -1 :style released-button)
                                         :inherit (mode-line)))))
   ;; separates windows.
   '(vertical-border ((t (:foreground "gray30"))))

   '(highlight-indent-guides-odd-face
     ((t (:background "#3F3F3C" ;; "#40403A"
                      :weight bold))))

   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))))
