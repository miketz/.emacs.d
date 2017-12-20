;;; -*- lexical-binding: t -*-

(defvar my-shrink-wild-dog 10
  "Size to shrink a split window.
So lines of text are not too long to follow with my eyes.")

(defun proj-dive-python ()
  (interactive)
  (interactive)
  (find-file-existing
   "/home/mike/books/diveintopython-5.4/diveintopython.txt")
  (markdown-mode))

(defun proj-pcl ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "pcl-test")
  (common-lisp-mode)
  (split-window-horizontally)
  (shrink-window-horizontally my-shrink-wild-dog)
  ;; NOTE: cloned from https://github.com/akosma/PracticalCommonLisp_ePub
  (eww-open-file "~/books/PracticalCommonLisp_ePub/html/index.html"))

(defun proj-progit2 ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (shrink-window-horizontally my-shrink-wild-dog)
  (eww-open-file "~/books/progit2/progit.html"))

(defun proj-progit2-dired ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (shrink-window-horizontally my-shrink-wild-dog)
  (dired "~/books/progit2/book"))

(defun proj-ydnjs ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (shrink-window-horizontally my-shrink-wild-dog)
  ;; (find-file-existing "~/books/You-Dont-Know-JS/README.md")
  (dired "~/books/You-Dont-Know-JS"))
