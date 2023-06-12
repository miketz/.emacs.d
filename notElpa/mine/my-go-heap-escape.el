;;; helper fn for running go's heap escape analysis

;;;###autoload
(cl-defun my-go-heap-escape ()
  (interactive)
  ;; shadow `compile-command'. it will automatically rollback to the original
  ;; value without corruption.
  (let ((compile-command "go build -gcflags=\"-m\""))
    (call-interactively #'compile)))


(provide 'my-go-heap-escape)