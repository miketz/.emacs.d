;;; my-cycle-line-position --- M-r replacement -*- lexical-binding: t -*-

;;; Commentary:
;;; Cycle between the mid, bot, and top positions in the buffer.
;;; Similar to `move-to-window-line-top-bottom' but takes into account buffer
;;; sizes smaller than the window height.

;;; Code:

(let ((positions '(mid top bot))
      (curr-pos nil) ; cache values for repeated M-r presses.
      (page-top nil)
      (page-bot nil)
      (page-mid nil))
  (defun my-cycle-line-position ()
    "Cycle between the mid, bot, and top positions in the buffer.
Similar to `move-to-window-line-top-bottom' but takes into account buffer sizes
smaller than the window height."
    (interactive)
    (let ((repeatp (eq this-command last-command)))
      (unless repeatp
        ;; calculate line numbers.
        (setq page-top (line-number-at-pos (window-start)))
        ;; TODO: figure out how much to subtract. 2 is needed most often.
        (setq page-bot (- (line-number-at-pos (window-end)) 2))
        (setq page-mid (+ page-top
                          (/ (1+ (- page-bot page-top)) 2))))
      (setq curr-pos (car (or (cdr (memq (if repeatp curr-pos nil)
                                         positions))
                              positions)))
      ;; (setq curr-pos (my-next-cycle-pos (if repeatp curr-pos nil)))
      ;; per emacs warnings, don't use `goto-line'.
      (goto-char (point-min))
      (forward-line (1- (cond ((eq curr-pos 'top) page-top)
                              ((eq curr-pos 'mid) page-mid)
                              ((eq curr-pos 'bot) page-bot)))))))

(provide 'my-cycle-line-position)

;;; my-cycle-line-position.el ends here