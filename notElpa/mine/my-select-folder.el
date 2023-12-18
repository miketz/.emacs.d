;;; my-select-folder.el --- helper funcs to select a folder -*- lexical-binding: t -*-

;;; Code:
(require 'cl-lib)
(require 'project)
(require 'ivy)
(require 'my-git-helpers)

(defun my-common-folders-list ()
  "List of common folders you usually want when running a program.
    (such as a linter)
1. The current folder.
2. The project root folder, if in a project.
3. Custom manually chosen folder."
  (let* ((folders '())
         (proj (project-current nil))
         (in-non-submodule-proj-p (and (not (null proj))
                                       ;; if in a submodule `project' gets the root dir of parent proejct!
                                       (not (my-is-in-git-submodule)))))
    ;; custom is a special flag which means user will need to manually input a folder
    (push "CUSTOM" folders)
    ;; project root folder. (ie git projects)
    (when in-non-submodule-proj-p
      (push (project-root proj) folders))
    ;; current folder
    (push default-directory folders)
    folders))


;;;###autoload
(cl-defun my-select-folder ()
  "Select a folder.
Provide current dir and project root dir as quick options.

Presumably the folder selected will be passed to another program such as a
linter."
  (interactive)
  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window. -4 so scrolling doesn't go off screen.
        ;; (ivy-height (- (window-height) 4))
        )
    (let ((folder (completing-read "dir: " (my-common-folders-list) nil t)))
      (unless (string-equal folder "CUSTOM")
        (cl-return-from my-select-folder folder))
      ;; they chose custom
      (read-directory-name "dir: " nil nil t))))



(provide 'my-select-folder)

;;; my-select-folder.el ends here