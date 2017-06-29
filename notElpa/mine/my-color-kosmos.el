(defun my-color-kosmos ()
  (interactive)
  (load-theme 'kosmos t)

  (let ((kosmos-fg "#bdbdbd")
        (kosmos-bg "#000000")
        (kosmos-bg-active "#102040")
        (kosmos-bg-inactive "#202020")
        (kosmos-keyword "#ffffff")
        (kosmos-str "#77cc77")
        (kosmos-comment "#50abab")
        (kosmos-gray "#777777")
        (kosmos-fg-todo "#bdabab")
        (kosmos-bg-todo "#775555")
        (kosmos-fg-done "#abbdab")
        (kosmos-bg-done "#557755")
        (kosmos-h1 "#d1d2d3"))

    (custom-theme-set-faces
     'kosmos
     `(show-paren-match ((t (:slant italic
                                    :bold t
                                    :strike-through t
                                    :background nil))))

     `(mode-line ((t (:background ,kosmos-bg-active
                                  :foreground ,kosmos-keyword
                                  :box (:line-width -1
                                                    :color "#ffffff"
                                                    :style released-button)))))
     `(mode-line-inactive ((t (:background ,kosmos-bg-inactive :foreground ,kosmos-gray
                                           :box (:line-width -1
                                                             :color ,kosmos-gray
                                                             :sytle pressed-button)))))
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
     '(rainbow-delimiters-depth-8-face ((t (:foreground "seagreen1"))))
     '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
     '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black")))))))
