;;; my-go-doc.el --- Show doc for thing at point -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Keywords: docs
;;; Package-Requires: ((emacs "25.1"))
;;; Version: 0.1.0
;;; URL: https://www.github.com/miketz/TODOuploadThis


;;; Commentary:
;;; Show go documentation for thing at point.  Works for built-in functions
;;; and data types.
;;; Not a mode, just some elisp functions that can be bound to keys.
;;; Typically used while in a go source buffer with `go-mode'.
;;;
;;; Funcs will attempt to guess or scrape the package name text.  You must then
;;; manually confirm or fix the "guessed" package name.
;;; Overall this package is flawed and hacky.  But it gets the job done for
;;; many cases.
;;; Function `my-go-doc-local' requires the go tooling "go doc" to be installed.
;;; Function `my-go-doc-website' requires a web browser and internet connection.
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
;;;   ;; reccomended key binds. Using SLIME's doc key binds.
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
  "Group for my-go-doc."
  :prefix "my-go-doc-"
  :group 'docs)

(defcustom my-go-doc-use-installed-go-ver-p t
  "When true use the locally installed [go version] for website docs.
When viewing docs on website the go version can be specified.
Otherwise the website docs defualt to the most recent version of Go.

In general it's a good idea to keep this set to true.  But you may view Go code
written for a more recent version than you have installed. In this case set
this var to nil."
  :type 'boolean
  :group 'my-go-doc)

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

;; NOTE: `my-go-doc-get-version' will break if [go version] changes the format of
;; its output string. If it becomes a problem then just change this to use a
;; hard coded version.
(defvar my-go-doc-ver (my-go-doc-get-version)
  "Go version used to construct the URL to web docs.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package name scraping

(cl-defun my-go-doc-scrape-package ()
  "Scrape out the package name for the thing at point.
Basically searching backwards for a dot (.)
Then grab the text before the dot.
Returns nil if no package found.
The implpementation is imperfect and may grab a package alias instead of the
acutal package name."
  (let ((bol (line-beginning-position)))
    (save-excursion
      (let ((dot (search-backward "." bol t)))
        ;; GUARD: dot not found
        (when (null dot)
          (cl-return-from my-go-doc-scrape-package nil))
        ;; now point is on the dot ".". Search backwards for the begging of package name.
        (let ((pack nil))
          (when (null pack) ;; try (
            (setq pack (save-excursion (search-backward "(" bol t))))
          (when (null pack) ;; try [
            (setq pack (save-excursion (search-backward "[" bol t))))
          (when (null pack) ;; try {
            (setq pack (save-excursion (search-backward "{" bol t))))
          (when (null pack) ;; try space
            (setq pack (save-excursion (search-backward " " bol t))))
          (when (null pack) ;; try tab
            (setq pack (save-excursion (search-backward "	" bol t))))

          ;; get the substring that is package name.
          (when (and pack dot) ;; found both start/end
            (setq pack (+ 1 pack)) ;; +1 to go forward past the delimiter we matched on.
            (buffer-substring-no-properties pack dot)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display functions. local or web

(defun my-go-doc--open-local (pack txt)
  "Show doc for thing at point in an Emacs buffer.
Uses command line tool [go doc].  Passsing in PACK and TXT."
  (let ((doc (shell-command-to-string (concat "go doc --all "
                                              (if (and pack (> (length pack) 0))
                                                  (concat pack ".")
                                                "")
                                              txt))))
    (switch-to-buffer-other-window (get-buffer-create "*go-doc*"))
    (erase-buffer)
    (insert doc)))

;; NOTE: if the format/layout of this website changes then the logic of constructing
;; the url with anchors will need to change.
(defvar my-go-doc-base-url "https://pkg.go.dev"
  "Website for go documentation.")

(defun my-go-doc--open-website (pack txt)
  "Show doc for thing at point in in a browser.
Uses URL 'https://pkg.go.dev'.  Passsing in PACK and TXT."

  ;; full-url sample with ver: https://pkg.go.dev/builtin@go1.19.4#make
  ;; full-url sample no ver:   https://pkg.go.dev/builtin#make
  (let ((full-url (concat my-go-doc-base-url
                          "/" pack
                          ;; add extra section to URL if using specific go version
                          (when (and my-go-doc-use-installed-go-ver-p
                                     my-go-doc-ver)
                            (concat "@go" my-go-doc-ver))
                          "#" txt)))
    (browse-url full-url)))


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
         (pack (completing-read "package: "
                                '() nil nil
                                ;; imperfect attempt to scrape package name from text
                                (or (my-go-doc-scrape-package)
                                    ;; types like int will not have a package name in the source.
                                    ;; use "builtin" for the doc lookup in this case.
                                    "builtin"))))
    ;; display in the specifed view
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
Uses URL 'https://pkg.go.dev'."
  (interactive)
  (my-go-doc 'website))

;;;###autoload
(defun my-go-doc-website-overview ()
  "Show top level docs in a browser.
Uses URL 'https://pkg.go.dev'."
  (interactive)
  (browse-url (concat my-go-doc-base-url "/std")))


(provide 'my-go-doc)

;;; my-go-doc.el ends here
