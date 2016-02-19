;;; -*- lexical-binding: t -*-
;;; Unit testing helpers

;; TODO: better msg printing.
;; TODO: clickable links to jump to test definition on failure.

(require 'cl-lib)

(defvar my-test-funcs '()
  "List of all test functions.")

(cl-defun my-run-all-tests () ;;using `cl-defun' to allow `return-from'
  "Runs all defined tests."
  (interactive)
  (when (null my-test-funcs)
    (let ((msg "no tests"))
      (message msg)
      (return-from my-run-all-tests msg)))
  (dolist (test my-test-funcs)
    (message (concat "running " (symbol-name test)))
    (funcall test))
  "Finished running all tests.")

(cl-defmacro my-deftest (name &body body)
  "Creates a test function.
Controls the output in a standard way for tests.
Adds some book keeping so it can be included in a global run of all tests."
  (declare (indent defun))
  ;; automaticall add test to the list
  (unless (member name my-test-funcs)
    (add-to-list 'my-test-funcs name))
  ;; expands into a regular function.
  `(defun ,name ()
     (interactive)
     ,@body
     'pass))


(when nil ;; interactive testing

  ;; commenting t-add. Macros seem to expand even when wrapped in a (when nil...
  ;; (my-deftest t-add
  ;;   (assert (= 3 (+ 1 2))))

  my-test-funcs
  (my-run-all-tests)
  )