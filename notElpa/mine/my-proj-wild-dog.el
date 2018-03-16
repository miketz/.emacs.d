;;; -*- lexical-binding: t -*-

(require 'bookmark)

(defvar my-shrink-wild-dog 10
  "Size to shrink a split window.
So lines of text are not too long to follow with my eyes.")

(defun my-proj-dive-python ()
  "Open the dive into python ebook."
  (interactive)
  (find-file-existing
   "/home/mike/books/python/diveintopython-5.4/diveintopython.txt")
  (markdown-mode))

(defun my-proj-pcl ()
  "Open the Practical Common Lisp ebook."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "pcl-test")
  (common-lisp-mode)
  (split-window-horizontally)
  (shrink-window-horizontally my-shrink-wild-dog)
  ;; NOTE: cloned from https://github.com/akosma/PracticalCommonLisp_ePub
  (eww-open-file "~/books/lisp/PracticalCommonLisp_ePub/html/index.html"))

(defun my-proj-progit2 ()
  "Open the progit2 ebook."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (shrink-window-horizontally my-shrink-wild-dog)
  (eww-open-file "~/books/git/progit2/progit.html"))

(defun my-proj-progit2-dired ()
  "Open the progit2 ebook folder."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (shrink-window-horizontally my-shrink-wild-dog)
  (dired "~/books/git/progit2/book"))

(defun my-proj-ydnjs ()
  "Open the You Don't Know JS ebook."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (shrink-window-horizontally my-shrink-wild-dog)
  ;; (find-file-existing "~/books/You-Dont-Know-JS/README.md")
  (dired "~/books/javascript/You-Dont-Know-JS"))

(defun my-proj-paip ()
  "Open the PAIP book."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "paip-test")
  (common-lisp-mode)
  (split-window-horizontally)
  (shrink-window-horizontally 13)
  (let ((paip (car (member "paip" (bookmark-all-names)))))
    (if paip
        (bookmark-jump paip)
      ;; else go to the folder
      (dired "~/books/lisp/paip-lisp/docs"))))
