;;; my-select-folder.el --- helper funcs to select a folder -*- lexical-binding: t -*-

;;; Code:
(require 'cl-lib)
(require 'project)
(require 'ivy)
(require 'my-git-helpers)
(require 'projectile)

(defun my-common-folders-list ()
  "List of common folders you usually want when running a program.
    (such as a linter)
1. Current folder.
2-a. Project root folder, if in a project.
2-b. Project root folder of current git-submodule.
3. Custom manually chosen folder."
  (let* ((folders '())
         (proj (project-current nil))
         (in-proj-p (not (null proj)))
         ;; if in a submodule `project' gets the root dir of parent proejct!
         (in-submodule-p (and in-proj-p (my-is-in-git-submodule)))
         (in-non-submodule-proj-p (and in-proj-p
                                       (not in-submodule-p)))
         )
    ;; custom is a special flag which means user will need to manually input a folder
    (push "CUSTOM" folders)
    ;; current folder
    (push default-directory folders)
    ;; project root folder. (ie git projects)
    (when in-non-submodule-proj-p
      (push (project-root proj) folders))
    (when in-submodule-p
      ;; `projectile-acquire-root' gets the root of the submodule project, not the parent container project
      (push (projectile-acquire-root) folders))
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



(defun my-common-folders-list2 ()
  "Faser select folder.
`my-common-folders-list' is doing git submodule checks which are pretty slow on MS-Windows."
  (let* ((folders '())
         (proj (project-current nil))
         (in-proj? (not (null proj)))
         (proj-root (and in-proj? (project-root proj))))
    ;; custom is a special flag which means user will need to manually input a folder
    (push "CUSTOM" folders)
    ;; current folder
    (push default-directory folders)
    ;; project root folder. (ie git projects)
    (when (and in-proj? (not (string-equal default-directory proj-root)))
      (push proj-root folders))
    folders))

;;;###autoload
(cl-defun my-select-folder2 ()
  "Faser select folder.
`my-select-folder' is doing git submodule checks which are pretty slow on MS-Windows."
  (interactive)
  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window. -4 so scrolling doesn't go off screen.
        ;; (ivy-height (- (window-height) 4))
        )
    (let ((folder (completing-read "dir: " (my-common-folders-list2) nil t)))
      (unless (string-equal folder "CUSTOM")
        (cl-return-from my-select-folder2 folder))
      ;; they chose custom
      (read-directory-name "dir: " nil nil t))))



(provide 'my-select-folder)

;;; my-select-folder.el ends here