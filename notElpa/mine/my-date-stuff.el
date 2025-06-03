;;; -*- lexical-binding: t -*-

(defun my-quit-window-date ()
  (interactive)
  (quit-window t) ; kill the buffer too.
  )

(define-minor-mode my-date-mode
  "Minor mode to simulate buffer local keybindings."
  :init-value nil
  ;; TODO: figure out how to get "q" to work for evil mode.
  ;; TODO: "q" is breaking for evil-emacs mode "C-z". Fix it.
  :keymap '(("q" . #'my-quit-window-date)))


(defun my-insert-date-big ()
  "Insert a date string.  Everything you need to know about the date and time."
  (interactive)
  (let* ((now (current-time))
         (utc-time (format-time-string "%-I:%M%#p UTC" now t)) ; t = UTC
         )
    (insert
     (format-time-string
      (concat "%Y-%m-%d (Numerical)%n%m-%d-%Y (USA)%n%A %B %-e, %Y%n%-I:%M%#p%n" utc-time "%nsecond: %S.%3N")
      ;; "%Y-%m-%d (Numerical)%n%m-%d-%Y (USA)%n%A %B %e, %Y%n%I:%M%P%nsecond: %S.%3N"
      now))))

(defun my-insert-date-short ()
  (interactive)
  (insert (format-time-string "%-m-%-d-%Y")))

(defun my-insert-date-string-new-buff ()
  (interactive)
  (let ((tmp-buff (get-buffer-create "*Date and Time*")))
    (unless (eq tmp-buff (current-buffer))
      (switch-to-buffer-other-window tmp-buff))
    (with-current-buffer tmp-buff
      (goto-char (point-max)) ;; end of buffer
      (insert "\n\n")
      (my-insert-date-big)
      (my-date-mode)))) ; for the windwow closing keybind "q"

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
;;                                 (my-insert-date-big)
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

(provide 'my-date-stuff)