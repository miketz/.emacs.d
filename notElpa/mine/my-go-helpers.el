;;; my-go-helpers.el --- helper funcs for go code -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'hydra)
(require 'project)
(require 'ivy)
(require 'my-go-doc)

(defvar my-go-errcheck-installed-p (executable-find "errcheck"))

;;;###autoload
(cl-defun my-go-errcheck ()
  "Run errcheck."
  (interactive)
  ;; GUARD: errcheck must be installed
  (unless my-go-errcheck-installed-p
    (message "errcheck not installed.\nTry this:\ngo install github.com/kisielk/errcheck@latest")
    (cl-return-from my-go-errcheck))

  ;; shadow `compile-command'. it will automatically rollback to the original
  ;; value without corruption.
  (let ((compile-command "errcheck"))
    (call-interactively #'compile)))


(defvar my-go-ineffassign-installed-p (executable-find "ineffassign"))

;;;###autoload
(cl-defun my-go-ineffassign ()
  "Run ineffassign."
  (interactive)
  ;; GUARD: ineffassign must be installed
  (unless my-go-ineffassign-installed-p
    (message "ineffassign not installed.\nTry this:\ngo install github.com/gordonklaus/ineffassign")
    (cl-return-from my-go-ineffassign))

  ;; shadow `compile-command'. it will automatically rollback to the original
  ;; value without corruption.
  (let ((compile-command "ineffassign ."))
    (call-interactively #'compile)))


;;;###autoload
(defun my-go-heap-escape ()
  "Run go's heap escape analysis."
  (interactive)
  ;; shadow `compile-command'. it will automatically rollback to the original
  ;; value without corruption.
  ;; TODO: grep may not work on windows. try ripgrep which i usually have
  ;;       installed.
  (let ((compile-command "go build -gcflags='-m=3' . |& grep escapes")
        ;; (compile-command "go build -gcflags=\"-m\"")
        )
    (call-interactively #'compile)))

;;;###autoload
(defun my-go-lint ()
  "Run golangci-lint.
It is 1 umbrella command that runs many other linters and combines their
results."
  (interactive)
  ;; shadow `compile-command'. it will automatically rollback to the original
  ;; value without corruption.
  (let* ((compile-command "golangci-lint run")
         ;;(compile-command "golangci-lint run -E gosec")
         ;;(compile-command "golangci-lint run --enable-all")
         ;;(compile-command "golangci-lint run --fix")

         (root-obj (project-current nil))
         (root-folder (if (not (null root-obj))
                          ;; extract folder field out of obj.
                          (project-root root-obj)
                        ;; else get root folder manually from user
                        (read-directory-name "proj root: " nil nil t)))
         ;; run lint at project root dir. `compile' uses `default-directory'.
         (default-directory root-folder))
    (call-interactively #'compile)))

(defun my-go-lint-and-fix ()
  "Run golangci-lint.
Then actually fix the files with the suggested fixes.
Be careful to use if there are many lints. Can be useful if there is a lint
where you are not sure exactly how to fix."
  (interactive)
  ;; shadow `compile-command'. it will automatically rollback to the original
  ;; value without corruption.
  (let ((compile-command "golangci-lint run --fix"))
    (call-interactively #'compile)))



(defvar my-go-types
  '("ComplexType" "FloatType" "IntegerType" "Type" "Type1" "any" "bool" "byte"
    "comparable" "complex128" "complex64" "error" "float32" "float64" "int"
    "int16" "int32" "int64" "int8" "rune" "string" "uint" "uint16" "uint32"
    "uint64" "uint8" "uintptr" "time.Time")
  "Built in types. Or types that are not officially built-in but are part of
the standard lib, like struct time.Time.")

;;;###autoload
(defun my-go-insert-type ()
  "Select and insert a go type with completing-read."
  (interactive)
  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window. -4 so scrolling doens't go off screen.
        (ivy-height (- (window-height) 4)))
    (insert (completing-read "type: " my-go-types))))


(defvar my-go-useful-libs
  '((xfmt "zero alloc printing" "go get lab.nexedi.com/kirr/go123")
    (zerolog "zero alloc logging" "go get -u github.com/rs/zerolog/log")))

;;;###autoload
(defun my-go-install-lib ()
  "Install go library from `my-go-useful-libs'."
  (interactive)
  (let* ((completing-read-function #'ivy-completing-read)
         ;; dynamically shadow ivy completion style to ignore order.
         (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
         ;; taller ivy window. -4 so scrolling doens't go off screen.
         (ivy-height (- (window-height) 4))
         (syms (mapcar #'car my-go-useful-libs))
         (lib-key (completing-read "lib: " syms))
         (install-cmd (cl-third (assoc (intern lib-key) my-go-useful-libs))))
    (shell-command install-cmd)))

;;;###autoload
(defun my-go-run-performance-tests ()
  "This is more of a documentation to help me remember how to run perf tests."
  (interactive)
  ;; shadow `compile-command'
  (let ((compile-command "go test -bench=."))
    (call-interactively #'compile)))

;; silence byte compiler when `rg' is not loaded yet
(defvar rg-command-line-flags)
;;;###autoload
(defun my-go-rg ()
  "Run rg but with with a custom file ignore.
Ignoring test files.
This is more a documentation of how to ignore files in rg."
  (interactive)
  (require 'rg)
  ;; shadow `rg-command-line-flags' for duration this let statement.
  (let ((rg-command-line-flags rg-command-line-flags))
    ;; ignore test files
    (add-to-list 'rg-command-line-flags "-g '!*_test.go'")
    (call-interactively #'rg)))


;; List several go helper functions.
(defhydra my-go-commands-hydra (:color blue :hint nil) ;;(:color blue)
  "
_l_: golangci-lint
_c_: compile
_e_: errcheck
_a_: ineffassign
_h_: heap
_t_: types
_d_: doc
_q_, _C-g_: quit"

  ("c" compile)
  ("e" my-go-errcheck)
  ("a" my-go-ineffassign)
  ("h" my-go-heap-escape)
  ("t" my-go-insert-type)
  ("d" my-go-doc-local)
  ("l" my-go-lint)

  ;; don't use the hint text as it makes (:hint nil) not work?
  ;; ("c" compile "compile")
  ;; ("e" my-go-errcheck "errcheck")
  ;; ("a" my-go-ineffassign "ineffassign")
  ;; ("h" my-go-heap-escape "heap")
  ;; ("t" my-go-insert-type "types")
  ;; ("d" my-go-doc-local "doc")

  ("C-g" nil nil)
  ("q" nil))

;;; my-go-helpers.el ends here
