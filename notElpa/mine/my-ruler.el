;;; my-ruler --- measure col widths -*- lexical-binding: t -*-

;;; Commentary:
;;; Insert a textual ruler into the buffer to measure the column width.

;;; Code:

(require 'cl-lib)

(defun my-insert-ruler ()
  "Insert text to visually measure width in the buffer.  By columns."
  ;; TODO: make it recursive, to handle 1000's, millions, etc.
  (interactive)
  (let* ((default-cols 110)
         (cols (if (equal current-prefix-arg '(4))
                   (read-number "cols: " default-cols)
                 default-cols))
         (secs-10 (/ cols 10))
         (remainder-10 (mod cols 10))
         (secs-100 (/ cols 100)))
    ;; the 100's
    (cl-loop for s from 1 to secs-100
             do
             (cl-loop for i from 1 to 100
                      do
                      (if (< i 100)
                          (insert " ")
                        (insert (format "%d" (mod s 100))))))
    (when (> secs-100 0)
      (insert "\n"))
    ;; the 10's
    (cl-loop for s from 1 to secs-10
             do
             (cl-loop for i from 1 to 10
                      do
                      (if (< i 10)
                          (insert " ")
                        (insert (format "%d" (mod s 10))))))
    (when (> secs-10 0)
      (insert "\n"))
    ;; the 1's
    (cl-loop repeat secs-10
             do
             (insert "1234567890"))
    (cl-loop for i from 1 to remainder-10
             do
             (insert (format "%d" i)))))


(defun my-goto-line (line-num)
  "Docs say don't use `goto-line' in Lisp code.
Making my own using the recommended alternative.
LINE-NUM = line to goto."
  (goto-char (point-min))
  (forward-line (1- line-num)))

(defun my-longest-line ()
  "Find the longest line in the buffer and jump to it.
It is slow in large buffers."
  (interactive)
  (let ((end (progn
               (goto-char (point-max)) ; (end-of-buffer)
               (line-number-at-pos)))
        (start 1)
        (biggest-col 0)
        (at-line 0))
    (cl-loop for line from start to end
             do
             (my-goto-line line)
             (move-end-of-line nil)
             (let ((col (current-column)))
               (when (> col biggest-col)
                 (setq biggest-col col)
                 (setq at-line line))))
    (my-goto-line at-line)
    (message (format "biggest col: %d\n line: %d"
                     biggest-col
                     at-line))))

(provide 'my-ruler)

;;; my-ruler.el ends here
