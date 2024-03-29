;;; my-go-doc.el --- Show doc for thing at point -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Keywords: docs
;;; Package-Requires: ((emacs "25.1"))
;;; Version: 0.1.0
;;; URL: https://github.com/miketz/.emacs.d/blob/master/notElpa/mine/my-go-doc.el
;;;      TODO: use a separate repo for this package. Currently bundled in my
;;;      emacs config.


;;; Commentary:
;;; Show go documentation for thing at point.  Works for built-in functions,
;;; data types, and packages.
;;; Not a mode, just some elisp functions that can be bound to keys.
;;; Typically used while in a go source buffer with `go-mode'.
;;;
;;; Funcs will attempt to guess or scrape the package name.  You must then
;;; manually confirm or fix the "guessed" package name.  If you're looking up
;;; docs for the package itself then leave the package entry blank.
;;; Function `my-go-doc-local' requires the go tooling "go doc" to be installed.
;;; Function `my-go-doc-website' requires a web browser and internet connection.
;;;
;;; This package is flawed and hacky.  It has trouble finding the package name
;;; for the thing at point.  But it may be good enough if you don't mind
;;; manually correcting the package name sometimes.  These alternative packages
;;; likely implement doc lookups in a technically superior way:
;;;     `go-eldoc', `eglot', 'lsp-mode', `go-mode'
;;; But you may still choose to use this package despite the flaws.
;;; --`go-eldoc' requires installing a program "gocode".  I don't have that
;;; program and wasn't sure how to install it.
;;; --`go-mode' provides fn `godoc-at-point'.  But it seems to break if
;;; external program "godef" is not installed.
;;; --`eglot' and `lsp-mode' are likely the best options for doc lookup. They
;;; should solve the "package name" scraping problem as they won't rely on raw
;;; text searching. In addition they work on non-built-in things.  But they
;;; require installing/using an lsp server for Go.  Which may drain more laptop
;;; battery. Also when funcalls are nested outer(inner(a,b,c)) you only get
;;; the docs for the "outer" in eglot!
;;; --This package only relies on [go doc] which is installed alongside Go
;;; itself. Nothing runs in the background.  You manually invoke a doc lookup.
;;;
;;; NOTE: lexical binding is used as a potential micro-optimization for
;;; variable look-ups.  This package *should* work the same whether lexical or
;;; dynamic binding is used.  (ie no closures capturing variables)


;;; Installation:
;;; Place `my-go-doc.el' in folder `/your/chosen/folder'.
;;; Add the following text to your .emacs or init.el file:
;;;
;;; (add-to-list 'load-path "/your/chosen/folder")
;;; (autoload #'my-go-doc-local "my-go-doc" nil t)
;;; (autoload #'my-go-doc-website "my-go-doc" nil t)
;;; (autoload #'my-go-doc-website-overview "my-go-doc" nil t)
;;; (with-eval-after-load 'go-mode
;;;   ;; recommended key binds.  Mimic SLIME key binds.
;;;   (define-key go-mode-map (kbd "C-c C-d d") #'my-go-doc-local)
;;;   (define-key go-mode-map (kbd "C-c C-d C-d") #'my-go-doc-local))


;;; Code:
(require 'thingatpt)
(require 'cl-lib)
(require 'seq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper functions

;; (defun my-go-doc-assert-dependencies ()
;;   "Check for required dependencies.  Warn the user if any are missing."
;;   (unless (executable-find "go")
;;     (display-warning
;;      'my-go-doc
;;      "my-go-doc requires go to be installed."
;;      :error)))

;; ;; Invoke dependency check at load time of my-go-doc.
;; ;; Only one check.  Don't prevent use of the feature.  Just warn then let the
;; ;; chips fall where they may.
;; (my-go-doc-assert-dependencies) ;; don't check for now.

(defgroup my-go-doc nil
  "Group for `my-go-doc'."
  :prefix "my-go-doc-"
  :group 'docs)

(defcustom my-go-doc-specify-go-ver-p t
  "When true use the locally installed [go version] for website docs.
When viewing docs on website the go version can be specified.
Otherwise the website docs default to the most recent version of Go.

In general it's a good idea to keep this set to true.  Usually you will want
the website docs to match your installed Go version."
  :type 'boolean
  :group 'my-go-doc)

(defcustom my-go-doc-assume-pkg-correct-p nil
  "Immediately jump to the docs without manually verifying the package name."
  :type 'boolean
  :group 'my-go-doc)

;; NOTE: `my-go-doc-get-version' will break if [go version] changes the format
;; of its output string.
(cl-defun my-go-doc-get-version ()
  "Get go version by parsing the string returned from [go version].
Returns nil if go version is not working."
  (interactive)
  (let ((str (shell-command-to-string "go version")))
    ;; GUARD: go version is not installed.
    (when (string-match-p "command not found" str)
      (cl-return-from my-go-doc-get-version nil))
    ;; custom string parsing to extract version num.
    (seq-subseq (cl-third (split-string str " "))
                2)))

(defvar my-go-doc-ver (my-go-doc-get-version)
  "Go version used to construct the URL to web docs.
By default this is your installed version of Go.
If go is not installed defaults to nil.")


(defvar my-go-doc--built-in-types
  '("string" "bool" "int8" "uint8" "byte" "int16" "uint16" "int32" "rune"
    "uint32" "int64" "uint64" "int" "uint" "uintptr" "float32" "float64"
    "complex64" "complex128" "nil" "Type" "IntegerType" "FloatType"
    "ComplexType"
    ;; var
    "iota"
    ;; interfaces
    "any" "comparable" "error"
    ;; ???
    ;; function names
    "append" "copy" "delete" "len" "cap" "make" "new" "complex"
    "real" "imag" "close" "panic" "recover" "print" "println"))

(defun my-go-doc--built-in-type-p (txt)
  "Return non-nil if TXT is a built in go type."
  (member txt my-go-doc--built-in-types))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package name scraping

(defun my-go-doc-get-dot-loc (bounds)
  "Return location of previous dot (.) relative to the thing at point.
Searches exactly 1 character before thing at point.  No big search backwards as
that can find the wrong dot, not related to thing at point.
BOUNDS represents thing at point.
Return nil if dot is not found at previous character."
  (let* ((start (car bounds))
         (prev (1- start))
         (prev-char (buffer-substring-no-properties prev start)))
    (if (string-equal prev-char ".")
        prev
      nil)))

(cl-defun my-go-doc-scrape-package (bounds)
  "Scrape out the package name for the thing at point.
Basically searching backwards for a dot (.)
Only search 1 char back for the dot using BOUNDS of thing at point.
Then grab the text before the dot.
Returns nil if no package found.
WARNING:
The implementation is imperfect and may grab a package alias or class name
instead of the actual package name.  Or it may grab something completely
incorrect."
  (let ((bol (line-beginning-position)))
    (save-excursion
      (let ((dot (my-go-doc-get-dot-loc bounds)))
        (when (null dot)
          (cl-return-from my-go-doc-scrape-package nil))
        (goto-char dot)
        ;; Now point is on the dot ".". Find begging of package name.
        ;; Search backward for all of these symbols (ie don't stop after first match)
        ;; Use the match closest to the dot as `pack-begin'.
        (let ((pack-begin nil)
              (begin-chars '("(" "[" "{" " " "	"))) ;; tab
          (cl-loop for c in begin-chars
                   do
                   (let ((tmp-begin (save-excursion (search-backward c bol t))))
                     ;; only set pack-begin if it's the nearest match so far.
                     (when (or (null pack-begin) ;; if not set yet
                               (and (not (null tmp-begin))
                                    (> tmp-begin pack-begin)))
                       (setq pack-begin tmp-begin))))

          ;; get the sub-string that is package name.
          (when (and pack-begin dot) ;; found both start/end
            (cl-incf pack-begin) ;; +1 to go forward past the delimiter we matched on.
            (buffer-substring-no-properties pack-begin dot)))))))

(cl-defun my-go-doc-guess-package (txt bounds)
  "Guess the package based on some heuristics and text searching.
TXT is the thing at point.
BOUNDS is the boundary of TXT.  A cons cell of format (start . end)."

  ;; search against a hard coded list for built in types.
  (when (my-go-doc--built-in-type-p txt)
    (cl-return-from my-go-doc-guess-package "builtin"))

  ;; scrape out package name by searching backwards for a "."
  (or (my-go-doc-scrape-package bounds)
      ;; else thing-at-point is itself a package. So blank out pack.
      ""))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display functions. local or web

(defun my-go-doc--open-local (pack txt)
  "Show doc for thing at point in an Emacs buffer.
Uses command line tool [go doc].  Passing in PACK and TXT."
  (let ((doc (shell-command-to-string (concat "go doc --all "
                                              (if (and pack (> (length pack) 0))
                                                  (concat pack ".")
                                                "")
                                              txt)))
        (orig-window (get-buffer-window (current-buffer))))
    (switch-to-buffer-other-window (get-buffer-create "*go-doc*"))
    (erase-buffer)
    (insert doc)
    ;; go to top of doc buffer
    (goto-char (point-min))
    ;; remain in the original buffer window
    (select-window orig-window)))

;; NOTE: if the format/layout of this website changes then the logic of constructing
;; the URL with anchors will need to change.
(defvar my-go-doc-base-url "https://pkg.go.dev"
  "Website for go documentation.")

(defun my-go-doc--open-website (pack thingatpoing-txt)
  "Show doc for thing at point in in a browser.
Uses URL `https://pkg.go.dev'.  Passing in PACK and THINGATPOING-TXT."

  ;; when the "thing at point" itself is a package, then we need to juggle some
  ;; values around
  (when (or (null pack) (string-equal pack ""))
    (setq pack thingatpoing-txt)
    (setq thingatpoing-txt nil))

  ;; URL sample with ver: https://pkg.go.dev/builtin@go1.19.4#make
  ;; URL sample no ver:   https://pkg.go.dev/builtin#make
  (let ((url (concat my-go-doc-base-url
                     "/" pack
                     ;; add extra section to URL if using specific go version
                     (if (and my-go-doc-specify-go-ver-p
                              my-go-doc-ver)
                         (concat "@go" my-go-doc-ver)
                       ;; else
                       "")
                     (if thingatpoing-txt
                         (concat "#" thingatpoing-txt)
                       ""))))
    (browse-url url)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cross roads fn.  Ties everything together.
;;; Sits between the user entry point funcs and view-doc funcs.
;;; Gets text at point. Scrapes package name.
;;; Then shows the doc via the specified view-type.

(defun my-go-doc (view-type)
  "Show doc for thing at point.
Displays the doc in a view defined by VIEW-TYPE.
Possible values: `local', `website'"
  (let* ((txt (thing-at-point 'symbol 'no-properties))
         (bounds (bounds-of-thing-at-point 'symbol))
         ;; imperfect attempt to scrape package name from text
         (pack (my-go-doc-guess-package txt bounds)))

    (unless my-go-doc-assume-pkg-correct-p
      ;; If thing-at-point is a builtin type then pack is "builtin". This is
      ;; pretty much always correct (not guessed) so skip the manual correction.
      (unless (or (string-equal pack "builtin")
                  ;; If thing-at-point is a package, then pack is "". Free functions also
                  ;; have "" so don't skip manual correction until i get a better
                  ;; detection.
                  ;; (string-equal pack "")
                  )
        ;; manual correction
        (setq pack (completing-read
                    "package: "
                    '() nil nil pack))))

    ;; display in the specified view
    (cond ((eq view-type 'local)
           (my-go-doc--open-local pack txt))
          ((eq view-type 'website)
           (my-go-doc--open-website pack txt))
          ;; whoops!
          (t (message (format "Unsupported view-type %s"
                              (symbol-name (or view-type 'unspecified))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry point functions for the user.

;;;###autoload
(defun my-go-doc-local ()
  "Show doc for thing at point in an Emacs buffer.
Uses command line tool [go doc]."
  (interactive)
  (my-go-doc 'local))

;;;###autoload
(defun my-go-doc-website ()
  "Show doc for thing at point in in a browser.
Uses URL `https://pkg.go.dev'."
  (interactive)
  (my-go-doc 'website))

;;;###autoload
(defun my-go-doc-website-overview ()
  "Show top level docs in a browser.
Uses URL `https://pkg.go.dev'."
  (interactive)
  (browse-url (concat my-go-doc-base-url "/std")))


(provide 'my-go-doc)

;;; my-go-doc.el ends here
