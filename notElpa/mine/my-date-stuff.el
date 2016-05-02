(defun my-insert-date-string ()
  "Insert a date string.  Everything you need to know about the date and time."
  (interactive)
  (insert
   (format-time-string
    "%Y-%m-%d (Numerical)%n%m-%d-%Y (USA)%n%A %B %e, %Y%n%I:%M%P%nsecond: %S.%3N")))

(defun my-insert-date-string-new-buff ()
  (interactive)
  (let ((tmp-buff "*Date and Time*"))
    ;; TODO: see if there are any emacs tmp buffer functions that will handle
    ;;       the window managemnt. ie q to delete, close window, etc.
    (switch-to-buffer-other-window tmp-buff)
    (with-current-buffer tmp-buff
      (goto-char (point-max)) ;; end of buffer
      (insert "\n\n")
      (my-insert-date-string))))

;; BROKEN
;; (defun my-insert-date-string-new-buff2 ()
;;   (interactive)
;;   (let ((tmp-buff "*Date and Time*"))
;;     ;; TODO: see if there are any emacs tmp buffer functions that will handle
;;     ;;       the window managemnt. ie q to delete, close window, etc.
;;     (with-current-buffer-window tmp-buff nil nil
;;                                 ;; (switch-to-buffer-other-window tmp-buff)
;;                                 (select-window (get-buffer-window tmp-buff))
;;                                 (goto-char (point-max)) ;;end of buffer
;;                                 (insert "\n\n")
;;                                 (my-insert-date-string)
;;                                 (help-mode) ; for the window closing features.
;;                                             ; press q to close.
;;                                 )))

(defun now ()
  (interactive)
  (format-time-string "%b %a %m-%d-%Y %I:%M %p"))
(defun now-minibuffer ()
  (interactive)
  (message (now)))
(defun now-put-in-buffer ()
  (interactive)
  (insert (now)))
