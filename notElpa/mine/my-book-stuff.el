;;; -*- lexical-binding: t -*-

;;; Commentary:
;;; Helper functions for reading books in Emacs.

;;; Code:

(require 'cl-lib)
(require 'mode-on-region)

;;;###autoload
(cl-defun my-proj-pcl-fancy-modes ()
  "Call this while in the eww buffer.
While reading Pracitcal Common Lisp."
  (interactive)

  ;; GUARD: only work in eww buffer. Presumably while reading Practical common lisp.
  (unless (string-equal "*eww*" (buffer-name))
    (message "This fn is only relevant while reading pracitcal common lisp in an *eww* buffer.")
    (cl-return-from my-proj-pcl-fancy-modes))

  (let ((curr-pos (point))) ; remember position in eww buffer.
    (eww-reload) ; causes images to load that may have failed previously.
    ;; dynamically override default mor settings
    (let ((mor-readonly-for-extra-protection-p nil) ; eww is already readonly.
          (mor-format-automatically-p nil) ; non-lisp text is present.
          (mor-switch-buff-fn #'switch-to-buffer) ; use same window.
          (mor-mode-fn #'common-lisp-mode))
      (mor-mode-on-region (point-min) (point-max)))
    (whitespace-cleanup)
    ;; try to warp to generally the same area you were in the eww buffer.
    (goto-char curr-pos)))

(provide 'my-book-stuff)