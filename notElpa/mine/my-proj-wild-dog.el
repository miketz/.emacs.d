
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
  ;; (common-lisp-mode)
  (split-window-horizontally)
  (shrink-window-horizontally 24)
  ;; NOTE: cloned from https://github.com/akosma/PracticalCommonLisp_ePub
  (eww-open-file "~/books/PracticalCommonLisp_ePub/html/index.html"))
