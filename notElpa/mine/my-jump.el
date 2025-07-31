;;; my-jump.el --- Helper funcs to search with ripgrep. -*- lexical-binding: t -*-

(require 'project)

;; TODO: look into `my-select-folder'. maybe handle git submodule issue?
(defun my-jump-read-search-dir ()
  "Select the dir to search.
Attempt to use project root as default selection.
If no project detected current dir will be the default selection."
  (let* ((proj (project-current nil))
         (starting-dir (if (not (null proj))
                           (project-root proj)
                         ;; else, not in a project, use current dir
                         default-directory)))
    (read-directory-name "dir: " starting-dir nil t)))


;; TODO: implement features


(provide 'my-jump)

;;; my-jump.el ends here