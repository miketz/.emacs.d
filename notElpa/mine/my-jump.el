;;; my-jump.el --- Helper funcs to search with ripgrep. -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Keywords: convenience
;;; Package-Requires: ((emacs "24.1") (rg 2.3.0) (hydra "0.15.0"))
;;; Version: 0.1.0
;;; URL: TODO


;;; Code:
(require 'project)
(require 'rg)
(require 'hydra)


;; TODO: in git submodule project, verify this stops at top of submodule, not top of outer containing project.
(defun my-jump-read-search-dir ()
  "Select the dir to search.
Attempt to use project root as default selection.
If no project detected, current dir will be the default selection."
  (let* ((proj (project-current nil))
         (in-proj? (not (null proj)))
         (starting-dir (if in-proj?
                           (project-root proj)
                         ;; else, not in a project, use current dir
                         default-directory)))
    (read-directory-name "dir: " starting-dir nil t)))


(defun my-jump (regex-fn)
  "Run ripgrep search for thing at point.
Using REGEX-FN to construct the regex with the thing-at-point text."
  (let* ((cursor-txt (thing-at-point 'symbol 'no-properties))
         (default-regex (funcall regex-fn cursor-txt))
         (regex (read-string "regex: " default-regex)))
    (rg regex
        (rg-read-files)
        (my-jump-read-search-dir))))


(provide 'my-jump)

;;; my-jump.el ends here