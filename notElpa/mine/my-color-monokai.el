(defun my-color-monokai ()
  "Load the monokai theme with several adjustments."
  (interactive)
  (load-theme 'monokai t)
  (custom-theme-set-faces
   'monokai
   ;;from VIM charcoal: hi Normal guifg=#ADC299 guibg=#35352B "*
   ;; `(default ((t (:background ,my-charcoal))))

   `(compilation-info
     ((t (:foreground "DarkOrange2"))))
   `(cursor
     ((t (:foreground "black"
                      :background "green"))))
   `(font-lock-comment-face
     ((t (:foreground "#66A555"))))
   `(fringe
     ((t (:foreground "burlywood"
                      :background "black"))))

   `(ace-jump-face-foreground
     ((t (:foreground "spring green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))

   ;; s-mode-line-bg "#3E3D31"
   ;; s-mode-line-fg "#F8F8F2"
   `(mode-line
     ((t (:foreground "#F8F8F2"
                      :background "#3E3D31"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   ;; s-mode-line-inactive-fg "#75715E"
   ;; s-mode-line-inactive-bg "#272822"
   `(mode-line-inactive
     ((t (:foreground "#75715E"
                      :background "#272822"
                      :box (:line-width -1 :style released-button)))))


   ;; '(js2-error
   ;;   ((t (:foreground "red"
   ;;                    :underline t))))
   ;;highlight so i can see the slime function parameters highlight.
   '(highlight ((t (:foreground "spring green"
                                :background "black"))))
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
   '(rainbow-delimiters-depth-8-face ((t (:foreground "yellow" :background "black"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black"))))))