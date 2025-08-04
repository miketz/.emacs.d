;;; my-jump.el --- Helper funcs to search with ripgrep. -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Keywords: convenience
;;; Package-Requires: ((emacs "24.1") (rg 2.3.0))
;;; Version: 0.1.0
;;; URL: TODO


;;; Code:
(require 'project)
(require 'rg)

;; TODO: look into `my-select-folder'. maybe handle git submodule issue?
(defun my-jump-read-search-dir ()
  "Select the dir to search.
Attempt to use project root as default selection.
If no project detected current dir will be the default selection."
  (let* ((proj (project-current nil))
         (in-proj? (not (null proj)))
         (starting-dir (if in-proj?
                           (project-root proj)
                         ;; else, not in a project, use current dir
                         default-directory)))
    (read-directory-name "dir: " starting-dir nil t)))


;; TODO: implement features


(provide 'my-jump)

;;; my-jump.el ends here