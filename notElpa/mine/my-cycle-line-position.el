;;; my-cycle-line-position --- M-r replacement -*- lexical-binding: t -*-

;;; Commentary:
;;; Cycle between the mid, bot, and top positions in the buffer.
;;; Similar to `move-to-window-line-top-bottom' but takes into account buffer
;;; sizes smaller than the window height.

;;; Code:
(require 'array) ; for #'current-line

;; older emacs. before 29
(when (not (fboundp #'array-current-line))
  (defalias 'array-current-line 'current-line))

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
        (setq page-bot (save-excursion
                         ;; (move-to-window -1) goes to the last fully visible
                         ;; line. This avoids scrolling when jumping to
                         ;; partially visible bottom line.
                         (move-to-window-line -1)
                         ;; Incriment 1 becuase #'current-line is 0-based.
                         ;; But #'line-number-at-pos is 1-based.
                         (1+ (array-current-line))))
        (setq page-mid (+ page-top
                          (/ (- page-bot page-top) 2))))
      (setq curr-pos (car (or (cdr (memq (if repeatp curr-pos nil)
                                         positions))
                              positions)))
      ;; (setq curr-pos (my-next-cycle-pos (if repeatp curr-pos nil)))
      ;; per emacs warnings, don't use `goto-line'.
      (goto-char (point-min))
      (forward-line (1- (cond ((eq curr-pos 'top) page-top)
                              ((eq curr-pos 'mid) page-mid)
                              ((eq curr-pos 'bot) page-bot)))))))

(defun my-evil-goto-page-mid ()
  "Move point to the middle of the currently visible lines.
Evil recenly broke the M command where it only goes to the window middle.
In Vim M goes to the middle of the currently visible lines, ignoring parts
of the window showing the void after the end-of-file."
  (interactive)
  (let* ((page-top (line-number-at-pos (window-start)))
         (page-bot (save-excursion
                     ;; (move-to-window -1) goes to the last fully visible
                     ;; line. This avoids scrolling when jumping to
                     ;; partially visible bottom line.
                     (move-to-window-line -1)
                     ;; Incriment 1 becuase #'current-line is 0-based.
                     ;; But #'line-number-at-pos is 1-based.
                     (1+ (array-current-line))))
         (page-mid (+ page-top
                      (/ (- page-bot page-top) 2))))
    ;; (goto-line page-mid)
    ;; per emacs warnings, don't use `goto-line'.
    (goto-char (point-min))
    (forward-line (1- page-mid))
    (list :top page-top
          :bot page-bot
          :mid page-mid)))

(provide 'my-cycle-line-position)

;;; my-cycle-line-position.el ends here
