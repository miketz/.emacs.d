;;; my-horizontal-scroll --- Scroll horizontally -*- lexical-binding: t -*-

;;; Commentary:
;;; Helper functions for scrolling horizontally over long lines.

;;; Code:

(defun my-scroll-left ()
  "Scroll left."
  (interactive)
  (my-scroll-horizontal #'scroll-left))

(defun my-scroll-right ()
  "Scroll right."
  (interactive)
  (my-scroll-horizontal #'scroll-right))

(defun my-scroll-horizontal (scroll-fn)
  "Scroll 25% of the window width.
SCROLL-FN will be `my-scroll-left' or `my-scroll-right'."
  (let ((cols (floor (* 0.25 (window-total-width)))))
    (funcall scroll-fn cols)
    ;; TODO: ensure point is moved to a visible character to make subsequent
    ;; navigation/selection of visible text possible.
    ))

(provide 'my-horizontal-scroll)

;;; my-horizontal-scroll.el ends here