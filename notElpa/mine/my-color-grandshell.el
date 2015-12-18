(defun my-color-grandshell ()
  "Grandshell with a few mode-specific additoins."
  (interactive)
  (load-theme 'grandshell t)

  (custom-theme-set-faces
   'grandshell

   `(font-lock-comment-face
     ((t (:foreground "gray50" :slant italic))))
   `(font-lock-comment-delimiter-face
     ((t (:foreground "gray50"))))

   ;;This isn't a js2 error face. But externals tend to be an error since js2 doesn't find the them.
   '(js2-external-variable ((t :underline (:color "red" :style wave)
                               :background "black")))
   '(js2-error ((t :underline (:color "red" :style wave)
                   :background "dark red")))
   '(js2-function-call ((t :foreground "violet"))) ;;making same as font-lock-function-name-face
   '(js2-warning ((t :underline (:color "yellow" :style wave)
                     :background "navy blue")))
   ;; colors copied from grandshell-theme.el
   `(mode-line ((t (:foreground "#eee"
                                :background "#331133"
                                :box (:line-width -1 :style released-button)))))
   ;;colors copied from grandshell-theme.el
   `(mode-line-inactive ((t (:foreground "#643"
                                         :background "#110011"
                                         :weight light
                                         :box (:line-width -1 :style released-button)
                                         :inherit (mode-line)))))))