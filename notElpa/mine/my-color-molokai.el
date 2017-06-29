(defun my-color-molokai ()
  (interactive)
  (load-theme 'molokai t)

  (my-rainbow-parens-dark-bg 'molokai)

  (custom-theme-set-faces
   'molokai

   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))

   ;; swiper/ivy faces TODO: fix more ivy/swiper faces.
   '(ivy-current-match
     ((t (:background "black" :foreground "light sky blue"))))

   ;; '(font-lock-variable-name-face
   ;;   ((t (:foreground "#924040"))))
   ;; '(my-tilde-face
   ;;   ((t (:foreground "#523030"))))

   `(font-lock-comment-face
     ((t (:foreground "RosyBrown4" :slant italic))))
   `(font-lock-comment-delimiter-face
     ((t (:foreground "RosyBrown3"))))

   `(fringe
     ((t (:foreground "burlywood"
                      :background "black"))))

   `(mode-line
     ((t (:foreground "#F8F8F2"
                      :background "#3E3D31"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "#75715E"
                      :background "#272822"
                      :box (:line-width -1 :style released-button)))))

   ;; the buffer name
   '(mode-line-buffer-id ((t (:foreground nil :weight semi-bold))))

   `(avy-lead-face-0 ;; the first overlay char
     ((t (:foreground "spring green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(avy-lead-face-1 ;; for matched chars, but currently not used???? matches disapear at the moment.
     ((t (:foreground "green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(avy-lead-face ;;for the chars after the first?
     ((t (:foreground "spring green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))))
