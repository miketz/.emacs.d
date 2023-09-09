;;; -*- lexical-binding: t -*-

(require 'project)

(defun my-compile-at-root-dir ()
  "Shadow `default-directory' with project root.  Then call `compile'.
This is needed if you are in a sub-dir of the project and want to invoke make
via M-x compile."
  (interactive)
  (let* (;; nil input to `project-current'. if project root cannot be detected
         ;; do not prompt for it as the 'project library doesn't allow you to
         ;; pick folders it doesn't think are projects! defeating the point of
         ;; manual selection!
         (root-obj (project-current nil))
         (root-folder (if (not (null root-obj))
                          ;; extract folder field out of obj.
                          (project-root root-obj)
                        ;; else get root folder manually from user
                        (read-directory-name "proj root: " nil nil t)))
         ;; shadow `default-directory' with root before calling `compile'
         (default-directory root-folder))
    (call-interactively #'compile)))

(provide 'my-compile-stuff)