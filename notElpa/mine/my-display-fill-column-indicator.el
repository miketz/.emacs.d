;;; my-display-fill-column-indicator.el --- helper funcs -*- lexical-binding: t -*-

(require 'display-fill-column-indicator)

(let ((cols '(nil 79 80 100 110))
      (curr nil))
  (defun my-cycle-col-line ()
    (interactive)
    (setq curr (car (or (cdr (memq curr cols))
                        cols)))
    (cond
     ;; if mode on and cycled to nil col.
     ((and display-fill-column-indicator-mode
           (eq curr nil))
      (display-fill-column-indicator-mode 0))
     ;; if mode off and cycled to integer col.
     ((and (not display-fill-column-indicator-mode)
           (integerp curr))
      (display-fill-column-indicator-mode 1)))

    (setq display-fill-column-indicator-column curr)
    (message "display-fill-column-indicator-column: %s" curr)))