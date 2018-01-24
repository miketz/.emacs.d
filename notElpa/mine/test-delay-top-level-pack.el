;;; expiriment avoid package-initialize call.
;;; not currenty used.

(defconst my-fast-load-p nil
  "When t, delay the use of packages.
For when I want a very fast start without `package-initialize'.  So Emacs can
be used for quick vim-like edits.
Call fn `my-do-full-init-with-packages' to continue with the full load.")

(defvar my--delayed-package-code '())

;; TODO: complete this macro, test it.
(cl-defmacro my-top-level-package-code (&rest body)
  "Wraps code related to packages.
This code would have been normal top-level code in the init.el file.
It should *not* include code that would live inside `eval-after-load'.
Wrapping in a macro allows optinally delaying the code to avoid a slow call
to `package-initialize' during start up.
The code is added to a list which can be invoked in the future with fn
`my-do-full-init-with-packages'"
  (declare (indent 0)) ;; indent like progn
  (if my-fast-load-p
      ;; don't expand/execute the code. Just add it to a list.
      (cl-loop for form in body ;; looping to sort of "splice" `body'
               do
               (add-to-list 'my--delayed-package-code form t))
    ;; else, expand to normal code for immediate execution.
    (let ((code '()))
      (cl-loop for form in body
               do
               (add-to-list 'code form t))
      `(funcall (lambda () ,@code)))))

(defun my-do-full-init-with-packages ()
  "Only call this 1 time if you started with `my-fast-load-p' = t."
  (interactive)
  (package-initialize)
  ;; TODO: Complete this, test it.
  ;; run the code I would have normally my-delayed-package-codedone at start up
  ;; for packages.
  (cl-loop for form in my--delayed-package-code
           do
           ;; (eval form)
           ;; (eval `(lambda () ,form) lexical-binding)
           (funcall `(lambda () ,form))
           ))

;; (when nil ;; interactive tetsing
;;   (setq my-fast-load-p t)
;;   (setq my--delayed-package-code '())
;;   (setq glob 0)

;;   (my-top-level-package-code
;;     (print "hi")
;;     (cl-incf glob))

;;   glob

;;   (macroexpand
;;    '(my-top-level-package-code
;;       (cl-incf glob)))


;;   (my-do-full-init-with-packages))
