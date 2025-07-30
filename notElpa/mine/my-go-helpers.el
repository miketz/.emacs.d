;;; my-go-helpers.el --- helper funcs for go code -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'hydra)
(require 'project)
(require 'ivy)
;; counsel mini buffer completion not highlighting current selection when
;; running emacs in the terminal. requiring `swiper' seems to fix it.
;; TODO: find/fix root cause.
(require 'swiper)
(require 'my-go-doc)
(require 'my-select-folder)
(require 'my-git-helpers)
(require 's)
(require 'hideshow)
(require 'thingatpt)
(require 'rg)


(defun my-go-scrape-module-name ()
  "Extract the module name from the go.mod file.
For now assumes you are in project root folder."
  (interactive)
  (with-temp-buffer
    ;; read first 100 bytes of file. should be enough to capture module name.
    (insert-file-contents-literally "go.mod" nil 0 100)
    (let* ((mod-name-full (cl-second (string-split (buffer-string))))
           (mod-name-suffix (car (nreverse (string-split mod-name-full "/")))))
      `(:mod-name-full ,mod-name-full
                       :mod-name-suffix ,mod-name-suffix))))


(cl-defun my-go-new-proj-simple ()
  "Initialize a new Go project in a folder."
  (interactive)

  ;; GUARD
  (unless (yes-or-no-p "You should be in an empty project folder. Proceed?")
    (cl-return-from my-go-new-proj-simple))

  (let* ((folder-name (car (nreverse (string-split (directory-file-name default-directory) "/"))))
         (cmd (format "go mod init %s" folder-name)))
    (setq cmd (read-shell-command "cmd: " cmd))
    (shell-command cmd)

    ;; main.go
    (make-empty-file "main.go")
    (let ((buff (find-file-literally "main.go")))
      (insert "package main

import (
	\"fmt\"
)

func main() {
	fmt.Printf(\"hi\\n\")
}")
      (save-buffer)
      (kill-buffer buff))))


(cl-defun my-go-new-proj ()
    "Initialize a new Go project in a folder."
    (interactive)

    ;; GUARD
    (unless (yes-or-no-p "You should be in an empty project folder. git init will be run!
Proceed?")
      (cl-return-from my-go-new-proj))

    (let* ((folder-name (car (nreverse (string-split (directory-file-name default-directory) "/"))))
           (cmd (format "go mod init %s" folder-name)))
      (setq cmd (read-shell-command "cmd: " cmd))
      (shell-command cmd)
      (let* ((mod-names (my-go-scrape-module-name))
             (mod-name-full (cl-getf mod-names :mod-name-full))
             (mod-name-suff (cl-getf mod-names :mod-name-suffix))
             (exe-suffix (if (eq system-type 'windows-nt) ".exe" ""))
             (exe-filename (concat mod-name-suff exe-suffix)))
        ;; common folders
        (make-directory "internal")
        (make-directory "cmd")

        ;; .gitignore
        (make-empty-file ".gitignore")
        (let ((buff (find-file-literally ".gitignore")))
          (insert (format "/%s
a.out
*.exe
.ctags.d/
tags
TAGS
"
                          mod-name-suff))
          (save-buffer)
          (kill-buffer buff))

        ;; Makefile
        (make-empty-file "Makefile")
        (let ((buff (find-file-literally "Makefile")))
          (insert (format ".DEFAULT_GOAL := build

.PHONY: fmt
fmt:
	go fmt ./...

.PHONY: lint
lint: fmt
	golint ./...

.PHONY: vet
vet: fmt
	go vet ./...

.PHONY: build
build: vet
	go build -o %s ./cmd/...

# use -ldflags to omit symbol table, debug info, and dwarf symbol table. (smaller binary).
# use -trimpath to remove filepaths. (smaller binary).
# use -gcflags=-B to eliminate bounds checks
.PHONY: buildProd
buildProd: vet
	go build -o %s -ldflags=\"-s -w\" -trimpath ./cmd/...


# disable optimizations to help Delve debugger.
.PHONY: buildDebug
buildDebug: vet
	go build -o %s -gcflags=\"all=-N -l\" ./cmd/...

.PHONY: run
run:
	./%s

# use favored build method of the moment. likely debug.
.PHONY: buildandrun
buildandrun: buildDebug run
"
                          exe-filename exe-filename exe-filename exe-filename))
          (save-buffer)
          (kill-buffer buff)))

      ;; main.go
      (let ((default-directory (concat default-directory "cmd/")))
        (make-empty-file "main.go")
        (let ((buff (find-file-literally "main.go")))
          (insert "package main

import (
	\"fmt\"
)

func main() {
	fmt.Printf(\"hi\\n\")
}")
          (save-buffer)
          (kill-buffer buff)))

      ;; git init
      (shell-command "git init")))

;;;----------------------------------------------------------------------------
;;; lint and compile stuff
;;;----------------------------------------------------------------------------
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

         ;; (root-obj (project-current nil))
         ;; (root-folder (if (not (null root-obj))
         ;;                  ;; extract folder field out of obj.
         ;;                  (project-root root-obj)
         ;;                ;; else get root folder manually from user
         ;;                (read-directory-name "proj root: " nil nil t)))

         ;; `compile' uses `default-directory'.
         (default-directory (my-select-folder)))
    (call-interactively #'compile)))

;;;###autoload
(defun my-go-compile ()
  "Call `compile' at the project root directory if found.
If root dir not found, have user select via completing-read."
  (interactive)
  (let* ((proj (project-current nil))
         (root-folder (if (and (not (null proj))
                               ;; if in a submodule `project' gets the root dir of parent proejct!
                               (not (my-is-in-git-submodule)))
                          ;; extract folder field out of obj.
                          (project-root proj)
                        ;; else get root folder manually from user
                        (read-directory-name "proj root: " nil nil t)))

         ;; `compile' uses `default-directory'.
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



;;;----------------------------------------------------------------------------
;;; insert go types
;;;----------------------------------------------------------------------------
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

(defvar my-go-builtin-funcs
  '("append" "copy" "delete" "len" "cap" "make" "max" "min" "new" "complex"
    "real" "imag" "clear" "close" "panic" "recover" "print" "println")
  "Built in functions.")

;;;###autoload
(defun my-go-insert-builtin-func ()
  "Select and insert a built-in go function with completing-read."
  (interactive)
  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window. -4 so scrolling doens't go off screen.
        (ivy-height (- (window-height) 4)))
    (insert (completing-read "type: " my-go-builtin-funcs))))


;;;----------------------------------------------------------------------------
;;; library instalation
;;;----------------------------------------------------------------------------
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

;;;----------------------------------------------------------------------------
;;; benchmarks
;;;----------------------------------------------------------------------------
;;;###autoload
(defun my-go-run-benchmarks ()
  "This is more of a documentation to help me remember how to run perf tests.
-bench=. means run all benchmarks. Use -bench=Foo for BenchmarkFoo.
-run=^# skips unit tests.
-test.benchmem includes allocations. Like adding b.ReportAllocs in the benchmark fn."
  (interactive)
  ;; shadow `compile-command'

  (let ((compile-command "go test -bench=. -test.benchmem -run=^#"))
    (call-interactively #'compile)))

;;;----------------------------------------------------------------------------
;;; testing stuff.
;;;----------------------------------------------------------------------------
;;;###autoload
(defun my-go-run-tests ()
  "This is more of a documentation to help me remember how to run tests.
-v means verbose, show PASS status of tests.
-cover shows test coverage percentage."
  (interactive)
  ;; shadow `compile-command'
  (let ((compile-command "go test -v -cover"))
    (call-interactively #'compile)))

;;;###autoload
(cl-defun my-go-gen-test-file ()
  "Create a _test.go file for the current go buffer."
  (interactive)

  ;; GUARD: current buffer must be visitng a Go file.
  (unless (and buffer-file-name
               (s-ends-with-p ".go" buffer-file-name))
    (message "Must be visiting a Go file.")
    (cl-return-from my-go-gen-test-file))

  (let* ((prefix (car (s-split (regexp-quote ".go") buffer-file-name)))
         (new-file-name (concat prefix "_test.go")))
    (make-empty-file new-file-name)
    (find-file new-file-name)
    ;; TODO: dynamically scrape package name from go buffer.
    (let ((txt "package main

import (
	\"os\"
	\"testing\"
)

func TestMain(m *testing.M) {
	// setup code here

	exitCode := m.Run()

	// teardown code here

	os.Exit(exitCode)
}

func BenchmarkFoo(b *testing.B) {
	b.ReportAllocs()
	// TODO: write benchmark
}

func TestFoo(t *testing.T) {
	t.Parallel()
	// TODO: write test
}
"))
      (insert txt)
      (save-buffer))))


;;;----------------------------------------------------------------------------
;;; rg stuff
;;;----------------------------------------------------------------------------
;; silence byte compiler when `rg' is not loaded yet
(defvar rg-command-line-flags)
;;;###autoload
(defun my-go-rg ()
  "Run rg but with with a custom file ignore.
Ignoring test files.
This is more a documentation of how to ignore files in rg."
  (interactive)
  ;; shadow `rg-command-line-flags' for duration this let statement.
  (let ((rg-command-line-flags rg-command-line-flags))
    ;; ignore test files. this is breaking search with rg 14.1.1. comment for now
    ;; (add-to-list 'rg-command-line-flags "-g '!*_test.go'")
    (call-interactively #'rg)))


;;;###autoload
(defun my-go-find-methods-of-struct ()
  "Find methods of a struct."
  (interactive)
  ;; shadow `rg-command-line-flags' for duration this let statement.
  (let* ((rg-command-line-flags rg-command-line-flags)
         (cursor-txt (thing-at-point 'symbol 'no-properties))
         (struct (completing-read "struct: " '() nil nil
                                  ;; default to text under cursor
                                  cursor-txt))
         ;; acutally a single \. double \\ is for the elisp string escape.
         (regex (concat "^func \\(.+" struct "\\)")))
    ;; ignore test files. this is breaking search with rg 14.1.1. comment for now
    ;; (add-to-list 'rg-command-line-flags "-g '!*_test.go'")
    ;; run search
    (rg regex
        (rg-read-files)
        (read-directory-name "dir: " nil nil t))))


;;;###autoload
(defun my-go-find-struct ()
  "Find struct definition."
  (interactive)
  ;; shadow `rg-command-line-flags' for duration this let statement.
  (let* ((rg-command-line-flags rg-command-line-flags)
         (cursor-txt (thing-at-point 'symbol 'no-properties))
         (struct (completing-read "struct: " '() nil nil
                                  ;; default to text under cursor
                                  cursor-txt))
         ;; acutally a single \. double \\ is for the elisp string escape.
         (regex (concat "^type " struct " struct")))
    ;; ignore test files. this is breaking search with rg 14.1.1. comment for now
    ;; (add-to-list 'rg-command-line-flags "--glob '!*_test.go'")
    ;; run search
    (rg regex
        (rg-read-files)
        (read-directory-name "dir: " nil nil t))))


;;;###autoload
(defun my-go-find-function ()
  "Find function definition."
  (interactive)
  ;; shadow `rg-command-line-flags' for duration this let statement.
  (let* ((rg-command-line-flags rg-command-line-flags)
         (cursor-txt (thing-at-point 'symbol 'no-properties))
         (fn-name (completing-read "fn: " '() nil nil
                                   ;; default to text under cursor
                                   cursor-txt))
         ;; acutally a single \. double \\ is for the elisp string escape.
         (regex (concat "^func " fn-name "\\(")))
    ;; ignore test files. this is breaking search with rg 14.1.1. comment for now
    ;; (add-to-list 'rg-command-line-flags "--glob '!*_test.go'")
    ;; run search
    (rg regex
        (rg-read-files)
        (read-directory-name "dir: " nil nil t))))

;;;###autoload
(defun my-go-find-method ()
  "Find method definition."
  (interactive)
  ;; shadow `rg-command-line-flags' for duration this let statement.
  (let* ((rg-command-line-flags rg-command-line-flags)
         (cursor-txt (thing-at-point 'symbol 'no-properties))
         (fn-name (completing-read "fn: " '() nil nil
                                   ;; default to text under cursor
                                   cursor-txt))
         ;; acutally a single \. double \\ is for the elisp string escape.
         (regex (concat "^func \\(.+\\) " fn-name "\\(")))
    ;; ignore test files. this is breaking search with rg 14.1.1. comment for now
    ;; (add-to-list 'rg-command-line-flags "--glob '!*_test.go'")
    ;; run search
    (rg regex
        (rg-read-files)
        (read-directory-name "dir: " nil nil t))))

;;;###autoload
(defun my-go-find-function-or-method ()
  "Find function or method definition.
More general, but may be slower and find more false matches."
  (interactive)
  ;; shadow `rg-command-line-flags' for duration this let statement.
  (let* ((rg-command-line-flags rg-command-line-flags)
         (cursor-txt (thing-at-point 'symbol 'no-properties))
         (fn-name (completing-read "fn: " '() nil nil
                                  ;; default to text under cursor
                                  cursor-txt))
         ;; acutally a single \. double \\ is for the elisp string escape.
         (regex (concat "^(func " fn-name "\\(|func \\(.+\\) " fn-name "\\()")))
    ;; ignore test files. this is breaking search with rg 14.1.1. comment for now
    ;; (add-to-list 'rg-command-line-flags "--glob '!*_test.go'")
    ;; run search
    (rg regex
        (rg-read-files)
        (read-directory-name "dir: " nil nil t))))

(defun my-go-find-function-or-method ()
  "Find function or method definition.
More general, but may be slower and find more false matches."
  (interactive)
  ;; shadow `rg-command-line-flags' for duration this let statement.
  (let* ((rg-command-line-flags rg-command-line-flags)
         (cursor-txt (thing-at-point 'symbol 'no-properties))
         (fn-name (completing-read "fn: " '() nil nil
                                  ;; default to text under cursor
                                  cursor-txt))
         ;; acutally a single \. double \\ is for the elisp string escape.
         (regex (concat "^(func " fn-name "\\(|func \\(.+\\) " fn-name "\\()")))
    ;; ignore test files. this is breaking search with rg 14.1.1. comment for now
    ;; (add-to-list 'rg-command-line-flags "--glob '!*_test.go'")
    ;; run search
    (rg regex
        (rg-read-files)
        (read-directory-name "dir: " nil nil t))))

;;;###autoload
(defun my-go-find-function-refs ()
  "Find refs/calls of function.
Flawed, does not find functions stored as variables due to use of opening ( in search.
But using this regex anyway for performance and fewer false positive matches."
  (interactive)
  ;; shadow `rg-command-line-flags' for duration this let statement.
  (let* ((rg-command-line-flags rg-command-line-flags)
         (cursor-txt (thing-at-point 'symbol 'no-properties))
         (fn-name (completing-read "fn: " '() nil nil
                                  ;; default to text under cursor
                                  cursor-txt))
         ;; acutally a single \. double \\ is for the elisp string escape.
         (regex (concat fn-name "\\(")))
    ;; ignore test files. this is breaking search with rg 14.1.1. comment for now
    ;; (add-to-list 'rg-command-line-flags "--glob '!*_test.go'")
    ;; run search
    (rg regex
        (rg-read-files)
        (read-directory-name "dir: " nil nil t))))





;;;----------------------------------------------------------------------------
;;; show/hide err handling sections
;;;----------------------------------------------------------------------------
(defvar-local my--go-handling-hidden-p nil)

(defun my--go-show-hide (regex show-or-hide-fn)
  (save-excursion
    (goto-char (point-min)) ;; goto beginning of buffer
    (while (re-search-forward regex
                              nil ;; no bounds on search
                              t ;; do not trigger an error if no search match
                              )
      (backward-char) ;; solves issue on trailing comment after {
      (funcall show-or-hide-fn))))

;;;###autoload
(defun my-go-hide-err-handling ()
  (interactive)
  (my--go-show-hide "if.+err != nil.+{" #'hs-hide-block)
  (my--go-show-hide "if.+err == nil.+{" #'hs-hide-block)
  (setq my--go-handling-hidden-p t))

;;;###autoload
(defun my-go-show-err-handling ()
  (interactive)
  (my--go-show-hide "if.+err != nil.+{" #'hs-show-block)
  (my--go-show-hide "if.+err == nil.+{" #'hs-show-block)
  (setq my--go-handling-hidden-p nil))

;;;###autoload
(defun my-go-toggle-err-handling ()
  (interactive)
  (if my--go-handling-hidden-p
      (my-go-show-err-handling)
    (my-go-hide-err-handling)))


;;;----------------------------------------------------------------------------
;;; text scraping. useful for advanced yasnippet expansions.
;;;----------------------------------------------------------------------------
;;;###autoload
(defun my-go-curr-fn-name ()
  "Get name of the current fn.
Seaches backwards for regex ^func. Then gets the following text after that.
Returns nil if not found.
Mostly intened for yasnippet expansions."
  (interactive)
  (save-excursion
    (let ((fn-point (re-search-backward "^func"
                                        nil ; no bounds on search
                                        t ; do not trigger an error if no search match
                                        )))
      (when fn-point ; found func keyword
        (let* ((method-paren-point (re-search-forward "("
                                                      (+ fn-point 6)
                                                      t))
               ;; if method go forward a bit more to get name
               (forward-cnt (if method-paren-point 3 2)))
          (cl-loop repeat forward-cnt do
                   (forward-word))
          (backward-word)
          (thing-at-point 'symbol 'no-properties))))))

;;;----------------------------------------------------------------------------
;;; hydra. List several go helper functions.
;;;----------------------------------------------------------------------------
(defhydra my-go-commands-hydra (:color blue :hint nil)
  "
_l_: golangci-lint
_c_: compile
_e_: errcheck
_a_: ineffassign
_h_: heap
_t_: types
_f_: built-in funcs
_d_: doc
_s_: toggle err handling visibility
_r_: ripgrep custom searches
_q_, _C-g_: quit"

  ("c" my-go-compile)
  ("e" my-go-errcheck)
  ("a" my-go-ineffassign)
  ("h" my-go-heap-escape)
  ("t" my-go-insert-type)
  ("f" my-go-insert-builtin-func)
  ("d" my-go-doc-local)
  ("l" my-go-lint)
  ("r" my-go-rg-hydra/body)
  ("s" my-go-toggle-err-handling)

  ;; don't use the hint text as it makes (:hint nil) not work?
  ;; ("c" my-go-compile "compile")
  ;; ("e" my-go-errcheck "errcheck")
  ;; ("a" my-go-ineffassign "ineffassign")
  ;; ("h" my-go-heap-escape "heap")
  ;; ("t" my-go-insert-type "types")
  ;; ("d" my-go-doc-local "doc")

  ("C-g" nil nil)
  ("q" nil))

(defhydra my-go-rg-hydra (:color blue :hint nil)
  "
_s_: struct
_M_: all methods of struct
_f_: function
_m_: method
_F_: function or method. (more general but more false matches)
_r_: functions references (flawed, misses fn vars)
_q_, _C-g_: quit"
  ("s" my-go-find-struct)
  ("M" my-go-find-methods-of-struct)
  ("f" my-go-find-function)
  ("m" my-go-find-method)
  ("F" my-go-find-function-or-method)
  ("r" my-go-find-function-refs)
  ("C-g" nil nil)
  ("q" nil))

;;; my-go-helpers.el ends here
