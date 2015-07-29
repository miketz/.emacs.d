(defun my-color-default-fancy ()
  (interactive)
  (my-color-default)

  (set-background-color mayan-smoke)

  (let ((a 85))
    (set-frame-parameter (selected-frame) 'alpha `(,a ,a)))

  ;; (progn
  ;;   (require 'highlight-tail)
  ;;   (setq highlight-tail-colors '(("lawn green" . 0)
  ;;                                 ("yellow" . 40)))
  ;;   (setq highlight-tail-steps 20       ;80
  ;;         highlight-tail-timer 0.04     ;0.04
  ;;         )
  ;;   (setq highlight-tail-posterior-type t) ;(setq highlight-tail-posterior-type 'const)
  ;;   (highlight-tail-mode)
  ;;   ;;(highlight-tail-reload)
  ;;   )
  )