;;; helper fn for running ineffassign

(defvar my-go-ineffassign-installed-p (executable-find "ineffassign"))

;;;###autoload
(cl-defun my-go-ineffassign ()
  (interactive)
  ;; GUARD: ineffassign must be installed
  (unless my-go-ineffassign-installed-p
    (message "ineffassign not installed.\nTry this:\ngo install github.com/gordonklaus/ineffassign")
    (cl-return-from my-go-ineffassign))

  ;; shadow `compile-command'. it will automatically rollback to the original
  ;; value without corruption.
  (let ((compile-command "ineffassign ."))
    (call-interactively #'compile)))


(provide 'my-go-ineffassign)