;;; -*- lexical-binding: t -*-

(require 'bookmark)

(defun my-proj-pcl ()
  "Open the Practical Common Lisp ebook."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "pcl-test")
  (common-lisp-mode)
  (split-window-horizontally)
  (shrink-window-horizontally 10)
  ;; NOTE: cloned from https://github.com/akosma/PracticalCommonLisp_ePub
  (eww-open-file "~/books/PracticalCommonLisp_ePub/html/index.html"))
