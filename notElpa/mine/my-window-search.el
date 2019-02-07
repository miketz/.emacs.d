;;; -*- lexical-binding: t -*-

(defun my-window-search ()
  "Interactive search, limited to the visible portion of the buffer."
  (interactive)
  (save-restriction        ;automatically calls widen when block ends.
    (narrow-to-region (window-start) (window-end))
    (isearch-forward)))

;; (defun my-window-search (fn)
;;   "Interactive search, limited to the visible portion of the buffer."
;;   (interactive)
;;   (save-restriction
;;     (narrow-to-region (window-start) (window-end))
;;     (call-interactively fn)))
;; (defun window-search-forward ()
;;   (interactive)
;;   (my-window-search #'isearch-forward))
;; (defun window-search-backward ()
;;   (interactive)
;;   (my-window-search #'isearch-backward))

(provide 'my-window-search)