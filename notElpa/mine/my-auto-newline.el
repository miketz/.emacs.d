;;; -*- lexical-binding: t -*-

;;; Commentary:
;;; I forget exactly what I was going for with this fn's. But they were used
;;; with cc-mode and electric-spacing.
;;; Not currently used, just preserving them here.

;;; Code:

(defun my-search-line-backwards (str)
  (interactive)
  (let ((line-start (save-excursion
                      (beginning-of-line 1)
                      (point))))
    (save-excursion
      (search-backward str line-start t))))

(cl-defun my-next-char-}-p ()
  "Return t if the first non-whitespace char after point is }.
Also only return t if the } is relatively close to (point)."
  (interactive)
  ;; Search a max of 200 chars forward (or less if near end of buffer).
  (let* ((distance-until-end (- (buffer-size) (point)))
         (end (min 200 distance-until-end)))
    (cl-loop named loop for i from (point) to end
             do
             (let ((c (byte-to-string (char-after i))))
               ;; unless whitespace
               (unless (or (string-equal c " ")
                           (string-equal c "	")
                           (string-equal c "\n"))
                 ;; (return (string-equal c "}"))
                 (cl-return-from my-next-char-}-p (string-equal c "}")))))))

(defun my-add-newline-automatically-p ()
  (interactive)
  (if (or (my-search-line-backwards "return")
          (my-next-char-}-p))
      'stop
    'yes-add-newline))

(provide 'my-auto-newline)