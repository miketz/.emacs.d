;;; my-devdocs-helpers.el --- helper funcs for devdocs pkg -*- lexical-binding: t -*-


;;;###autoload
(defun my-devdocs-lookup ()
  "Wrapper over `devdocs-lookup'. Default input to `thing-at-point'."
  (interactive)
  (require 'thingatpt)
  (let ((txt (thing-at-point 'symbol 'no-properties)))
    (devdocs-lookup nil txt)))

;;;###autoload
(defun my-devdocs-install-docs ()
  "Install docs for languages I use."
  (interactive)
  (let ((docs '("c" "go" "bash" "html" "javascript" "css" "perl~5.38"
                "lua~5.4")))
    (cl-loop for d in docs
             do
             (devdocs-install d))))