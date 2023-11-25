;;; my-select-folder.el --- helper funcs to select a folder -*- lexical-binding: t -*-

;;; Code:
(require 'cl-lib)
(require 'project)
(require 'ivy)

(defun my-common-folders-list ()
  (let* ((folders '())
         (proj (project-current nil))
         (in-proj-p (not (null proj))))
    ;; custom is a special flag which means user will need to manually input a folder
    (push "CUSTOM" folders)
    ;; project root folder. (ie git projects)
    (when in-proj-p
      (push (project-root proj) folders))
    ;; current folder
    (push default-directory folders)
    folders))


;;;###autoload
(cl-defun my-select-folder ()
  "Select a folder.
Provide current dir and project root dir as quick options."
  (interactive)
  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window. -4 so scrolling doens't go off screen.
        ;; (ivy-height (- (window-height) 4))
        )
    (let ((folder (completing-read "type: " (my-common-folders-list) nil t)))
      (unless (string-equal folder "CUSTOM")
        (cl-return-from my-select-folder folder))
      ;; they chose custom
      (read-directory-name "proj root: " nil nil t))))



(provide 'my-select-folder)

;;; my-select-folder.el ends here