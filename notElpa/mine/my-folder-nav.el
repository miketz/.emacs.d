;;; -*- lexical-binding: t -*-

(defun my-current-file-path ()
  "Return the full file path of the current buffer as a string."
  (interactive)
  (or load-file-name
      buffer-file-name))

(defun my-current-folder-path ()
  "Return the folder path of the current buffer as a string."
  (interactive)
  (file-name-directory (my-current-file-path)))

(defun my-folder-nav ()
  "Opens a dired buffer.  Dired does all the actual work.  This just handles the visual aspects like window placement and size."
  (interactive)
  (dired-other-window (my-current-folder-path))
  (evil-window-move-far-left)
  ;;I can't find a function to set the exact window size, so collapsing the buffer then enlarging to the size I want.
  (let ((bigNumToCollapse 500)
        (width 65))
    (shrink-window-horizontally bigNumToCollapse)
    (enlarge-window-horizontally (- width
                                    (window-total-width)))))
