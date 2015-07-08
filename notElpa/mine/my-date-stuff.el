(defun my-insert-date-string ()
  "Insert a date string.  Everything you need to know about the date and time."
  (interactive)
  (insert
   (format-time-string
    "%Y-%m-%d (Numerical)%n%m-%d-%Y (USA)%n%A %B %e, %Y%n%I:%M%P%nsecond: %S.%3N")))

(defun now ()
  (interactive)
  (format-time-string "%b %a %m-%d-%Y %I:%M %p"))
(defun now-minibuffer ()
  (interactive)
  (message (now)))
(defun now-put-in-buffer ()
  (interactive)
  (insert (now)))
