(defun my-color-leuven ()
  (interactive)
  (load-theme 'leuven t)
  (my-cursor-stuff :color-emacs "maroon" :color-evil "blue")
  (let ((cur '(box "blue")))
    (setq evil-normal-state-cursor cur)
    (setq evil-visual-state-cursor '(hollow "blue"))
    (setq evil-operator-state-cursor cur))

  (custom-theme-set-faces
   'leuven
   ;; `(default ((t (:foreground "black" :background ,mayan-smoke))))
   `(default ((t (:foreground "black"))))
   ;;`(default ((t (:foreground "black" :background ,"white"))))
   `(mode-line ((t (:box (:line-width -1 :color "#1A2F54")
                         :foreground "#85CEEB" :background "#335EA8"
                         :style released-button))))
   `(mode-line-inactive ((t (:box (:line-width -1 :color "#4E4E4C")
                                  :foreground "#F0F0EF" :background "#9B9C97"
                                  :style released-button))))
   '(js2-function-call ((t :foreground "blue")))
   '(erc-timestamp-face ((t :foreground "purple" :weight bold)))
   '(fringe
     ((t (:foreground "#9B9B9B" :background "alice blue"))))
   '(leerzeichen ((t (:foreground "black";"#A8A800"
                                  :background "white";"#D4D4C8"
                                  :italic nil
                                  :bold nil
                                  ;;:box t
                                  ))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "black" :background "gray94" :weight bold))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "light cyan" :weight bold))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background "#faEaEa" :weight bold))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "lavenderblush" :weight bold))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "lemon chiffon" :weight bold))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background "#EEEEFF" :weight bold))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52" :weight bold))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3" :weight bold))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "#fff7ca" :weight bold))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black" :weight bold))))))