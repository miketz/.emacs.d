(defun my-color-niflheim ()
  "Load the zenburn theme created by Bozhidar Batsov.  Make a few extra mods too."
  (interactive)
  (load-theme 'niflheim t)

  (let ((class '((class color) (min-colors 89)))
        (background "#303030")
        (dark "#202020")
        (fringe "#353535")
        (highlight "#454545")
        (comment "#929283")
        (light "#f6f3e8")
        (veryligh "fbfaf5")
        (grey "#666666")
        (grey-light "#aaaaaa")
        (grey-darker "#333333")
        (grey-dark "#252525")
        (orange "#ffcd8e")
        (orange-light "#ffedd5")
        (orange-2 "#f7af75")
        (orange-dark "#da8548")
        (orange-darker "#bd6626")
        (mode-line-inactive "#2a2a2a")
        (yellow-dark "#888833")
        (purple "#cbaaf5")
        (purple-light "#ddcaf6")
        (purple-dark "#7846b5")
        (purple-darker "#544568")
        (blue "#7ac1ff")
        (blue-alt "#1268b4")
        (blue-light "#aaccff")
        (blue-dark "#456981")
        (blue-darker "#3e4d58")
        (green "#789771")
        (green-2 "#70a56f")
        (green-3 "#92a65e")
        (green-4 "#83e1b2")
        (green-light "#aaeeab")
        (green-dark "#284437")
        (green-alt "#198754")
        (red "#ff6c6b")
        (red-light "#ff5b66")
        (red-alt "#981b1b")
        (red-dark "#553333")
        (default "#b8c4cf")
        (cursor-background "#b6c4cf"))

    (custom-theme-set-faces
     'niflheim

     `(mode-line ((,class (:background ,purple-darker
                                       :foreground ,light
                                       :box (:line-width -1 :color ,grey-light)))))
     `(mode-line-inactive ((,class (:background ,mode-line-inactive
                                                :foreground ,grey-light
                                                :box (:line-width -1 :color ,grey-light)))))

     `(whitespace-space-before-tab ((t (:background "orange"))))

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
     '(rainbow-delimiters-depth-8-face ((t (:foreground "hot pink" :background "#2F2F2F"))))
     '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
     '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black")))))))
