;;; my-horizontal-scroll --- Scroll horizontally -*- lexical-binding: t -*-

;;; Commentary:
;;; Helper functions for scrolling horizontally over long lines.

;;; Code:

(defun my-scroll-left ()
  "Scroll left."
  (interactive)
  (my-scroll-horizontal-4 #'scroll-left))

(defun my-scroll-right ()
  "Scroll right."
  (interactive)
  (my-scroll-horizontal-4 #'scroll-right))

(defun my-scroll-horizontal (scroll-fn)
  "Scroll 25% of the window width.
SCROLL-FN will be `my-scroll-left' or `my-scroll-right'."
  (let ((cols (floor (* 0.25 (window-total-width)))))
    (funcall scroll-fn cols)
    ;; TODO: ensure point is moved to a visible character to make subsequent
    ;; navigation/selection of visible text possible.
    ))

(defun my-scroll-horizontal-4 (scroll-fn)
  "Scroll 4 cols.
Common indentation width, so works well for Java/C# code that's always indented
due to namespace and/or class nesting."

  ;; use prefix-arg to pass in "4" to scroll-fn
  (let ((current-prefix-arg '(4)))
    ;; calling interactively prefix arg will "lock" the scroll position which is
    ;; what we want. use C-x > to go back.
    (call-interactively scroll-fn nil))


  ;; overwrite `fringe-indicator-alist' to remove the fring arrows. (buffer local)
  ;; In this case we know we are scrolling and the arrows are annoying.
  ;; TODO: figureout why setq, setq-local don't work
  (setq-default fringe-indicator-alist
                (assq-delete-all 'truncation
                                 (copy-sequence fringe-indicator-alist)))

  (redraw-display) ; clears old dupe cursor point
  ;; (set-window-buffer nil (current-buffer))
  ;; (redraw-frame)
  ;; (print fringe-indicator-alist)

  ;; (funcall scroll-fn 4)
  )

(provide 'my-horizontal-scroll)

;;; my-horizontal-scroll.el ends here