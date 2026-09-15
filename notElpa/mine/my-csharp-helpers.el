;;; my-csharp-helpers.el --- helper funcs for C# code -*- lexical-binding: t -*-

(require 'thingatpt)
(require 'rg)
(require 'jump)
(require 'cl-lib)


;;;###autoload
(defun my-csharp-delete-region-tags ()
  "Delete #region and #endregion tags in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min)) ;; goto beginning of buffer
    (let ((cnt 0))
      (while (re-search-forward "#.*region"
                                nil ;; no bounds on search
                                t ;; do not trigger an error if no search match
                                )
        (delete-line)
        (setq cnt (+ cnt 1)))
      (message "Deleted %d region tags." cnt))))

;;; my-csharp-helpers.el ends here