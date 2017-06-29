;;; loads the default emacs theme. Makes a few mods too.
(defun my-color-default ()
  (interactive)
  ;;default is the lack of themes, so disable any enabled themes.
  (dolist (thm custom-enabled-themes)
    (disable-theme thm))
  ;;(set-background-color "ivory2")

  ;; (my-cursor-stuff-lightBg)
  ;; (let ((cur '(box "blue")))
  ;;   (setq evil-normal-state-cursor cur)
  ;;   (setq evil-visual-state-cursor '(hollow "blue"))
  ;;   (setq evil-operator-state-cursor cur))

  ;; (my-set-font :sym 'consolas
  ;;              :weight 'bold
  ;;              :height 125
  ;;              :resize-window t)
  (progn ; use bold font.
    ;; (custom-set-faces
    ;;  '(default ((t (:weight bold)))))

    ;; `set-face-attribute' is better than `custom-set-faces'.
    ;; it doesn't wipe out all the attributes.
    (mapc (lambda (face)
            (set-face-attribute face
                                nil ;; all frames
                                :weight 'bold))
          (face-list))
    ;; refresh screen.
    (when (fboundp 'my-w32-run) ; TODO: make it work on non-Windows machines.
      (my-w32-run 'restore-curr-frame)
      (my-w32-run 'max)))

  ;;(set-face-background hl-line-face "#EEFFEE")
  (my-rainbow-parens-light-bg2)
  ;;(my-set-font :weight 'bold)

  (custom-set-faces
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   `(ace-jump-face-foreground
     ((t (:foreground "black"
                      :background "cyan"
                      :slant normal
                      :weight bold
                      :inverse-video nil))))))
