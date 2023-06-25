;;; my-go-helpers.el --- helper funcs for go code -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'hydra)

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
(cl-defun my-go-heap-escape ()
  "Run go's heap escape analysis."
  (interactive)
  ;; shadow `compile-command'. it will automatically rollback to the original
  ;; value without corruption.
  (let ((compile-command "go build -gcflags=\"-m\""))
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


;; List several go helper functions.
(defhydra my-go-commands-hydra (:color blue :hint nil) ;;(:color blue)
  "
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
