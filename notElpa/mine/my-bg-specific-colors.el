(defun my-rainbow-parens-dark-bg (theme)
  "Colors for parens that are easy to distinguish from each other when against a dark bg."
  (interactive)
  (custom-theme-set-faces
   theme
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
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black"))))))

(defun my-rainbow-parens-dark-bg-bold (theme)
  "Colors for parens that are easy to distinguish from each other when against a dark bg."
  (interactive)
  (custom-theme-set-faces
   theme
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "orange red" :bold t))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan" :bold t))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow" :bold t))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "plum" :bold t))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "lawn green" :bold t))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orange" :bold t))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "white" :bold t))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "seagreen1" :bold t))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3" :bold t))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black" :bold t))))))

(defun my-rainbow-parens-light-bg (theme)
  (interactive)
  (custom-theme-set-faces
   theme
   '(rainbow-delimiters-depth-1-face ((t (:foreground "black"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#09a509"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#3388ff"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "black" :background "red"))))))

(defun my-rainbow-parens-light-bg2 (theme)
  "Colored parens with highlighting."
  (interactive)
  (custom-theme-set-faces
   theme
   '(rainbow-delimiters-depth-1-face ((t (:foreground "black" :background "gray94" :weight normal))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "light cyan" :bold nil))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background "#faEaEa" :bold nil))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "lavenderblush" :bold nil))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "lemon chiffon" :bold nil))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background "#EEEEFF" :bold nil))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52" :bold nil))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3" :bold nil))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "#fff7ca" :bold nil))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black" :bold nil))))))

(defun my-rainbow-parens-light-bg3 (theme)
  "Colored parens with highlighting."
  (interactive)
  (custom-theme-set-faces
   theme
   '(rainbow-delimiters-depth-1-face ((t (:foreground "black" :background "gray94" :weight bold))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "light cyan" :bold t))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background "#faEaEa" :bold t))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "lavenderblush" :bold t))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "lemon chiffon" :bold t))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background "#EEEEFF" :bold t))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52" :bold t))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3" :bold t))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "#fff7ca" :bold t))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black" :bold t))))))
