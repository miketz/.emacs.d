;;; -*- lexical-binding: t -*-

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
  ;;(my-set-font :weight 'bold)

  ;; TODO: find a way to set these faces so they can be rolled back. Like when
  ;; set for a specific theme with `custom-theme-set-faces'.
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
                      :inverse-video nil))))

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
