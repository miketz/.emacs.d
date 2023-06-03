;;; helper fn for running errcheck

(defvar my-go-errcheck-installed-p (executable-find "errcheck"))

;;;###autoload
(cl-defun my-go-errcheck ()
  (interactive)
  ;; GUARD: errcheck must be installed
  (unless my-go-errcheck-installed-p
    (message "errcheck not installed.\nTry this:\ngo install github.com/kisielk/errcheck@latest")
    (cl-return-from my-go-errcheck))

  ;; shadow `compile-command'. it will automatically rollback to the original
  ;; value without corruption.
  (let ((compile-command "errcheck"))
    (call-interactively #'compile)))


(provide 'my-go-errcheck)