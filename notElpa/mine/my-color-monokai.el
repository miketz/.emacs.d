(defun my-color-monokai ()
  "Load the monokai theme with several adjustments."
  (interactive)
  (load-theme 'monokai t)

  (my-rainbow-parens-dark-bg 'monokai)

  (custom-theme-set-faces
   'monokai
   ;;from VIM charcoal: hi Normal guifg=#ADC299 guibg=#35352B "*
   ;; `(default ((t (:background ,my-charcoal))))

   `(region
     ((t (:background "#69685E"))))

   `(erc-notice-face
     ((t (:foreground "dark gray"))))
   `(compilation-info
     ((t (:foreground "DarkOrange2"))))
   ;; `(cursor
   ;;   ((t (:foreground "black"
   ;;                    :background "green"))))
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
                                  :background nil))))))
