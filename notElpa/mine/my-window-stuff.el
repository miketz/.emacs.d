;;; -*- lexical-binding: t -*-

(defun my-quit-window ()
  "Kill the buffer and close the window if one was opened specifically for
that buffer."
  (interactive)
  (quit-window t))

(defun my-shrink-window-horizontally ()
  (interactive)
  (shrink-window-horizontally 15))

(defun my-enlarge-window-horizontally ()
  (interactive)
  (enlarge-window-horizontally 15))


(declare-function my-w32-run 'suppress)
;; for MS Windows only
;;`maxp' can get out of sync. Hit <Leader>f a 2nd time to re-sync.
(let ((maxp nil))
  (defun my-toggle-frame-max-MS-Windows ()
    "Closure over `maxp'."
    (interactive)
    (let ((flag (if maxp
                    'restore-curr-frame
                  'max)))
      (my-w32-run flag)
      ;; toggle bool flag
      (setq maxp (not maxp)))))


(provide 'my-window-stuff)