(defun my-color-tango-dark ()
  (interactive)
  (load-theme 'tango-dark t)

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
                                  :background nil))))

   '(rainbow-delimiters-depth-1-face ((t (:foreground "orange red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "plum"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "lawn green"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "white"))))
   ;;'(rainbow-delimiters-depth-8-face ((t (:foreground "seagreen1"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "hot pink" :background "#2F2F2F"
                                                      ))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black"))))))
