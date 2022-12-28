;;; my-go-doc.el --- Show doc for thing at point -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Keywords: convenience
;;; Package-Requires: ((s "1.13.1"))
;;; Version: 0.1.0
;;; URL: n/a

;;; Commentary:
;;; Show go documentation for thing at point.
;;; It will attempt to guess or scrape the text for the package name.  You must
;;; then manually confirm or fix the "guessed" package name.
;;; Function `my-go-doc-local' requires the go tooling "go doc" to be installed.
;;; Function `my-go-doc-website' requires a web browser and internet connection.


;;; Installation:
;;; Place `my-go-doc.el' in folder `/your/chosen/folder'.
;;; Add the following text to your .emacs or init.el file:
;;;
;;; (add-to-list 'load-path "/your/chosen/folder")
;;; (autoload #'my-go-doc-local "my-go-doc" nil t)
;;; (autoload #'my-go-doc-website "my-go-doc" nil t)
;;; (autoload #'my-go-doc-website-overview "my-go-doc" nil t)
;;; (with-eval-after-load 'go-mode
;;;   ;; reccomended key binds
;;;   (define-key go-mode-map (kbd "C-c C-d d") #'my-go-doc-local)
;;;   (define-key go-mode-map (kbd "C-c C-d C-d") #'my-go-doc-local))


;;; Code:
(require 'thingatpt)
(require 's)


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

(cl-defun my-go-get-version ()
  "Get go version by parsing the string returned from [go version].
Returns nil if go version is not working."
  (interactive)
  (require 's)
  (require 'cl-lib)
  (let ((str (shell-command-to-string "go version")))
    (when (s-contains-p "command not found" str)
      (cl-return-from my-go-get-version nil))
    ;; custom string parsing to extract version num.
    (seq-subseq (cl-third (s-split " " str))
                2)))

(defvar my-go-ver (or (my-go-get-version)
                      "1.19.4")) ;; default. latest version at time of writing.


(defun my-thing-at-point ()
  "Return the text at point."
  (interactive)
  (let ((str (thing-at-point 'symbol 'no-properties)))
    (print str)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package name scraping

(cl-defun my-go-doc-scrape-package ()
  "Scrape out the package name for the thing at point.
Basically searching backwards for a dot (.)
Then grab the text before the dot.
Returns nil if no package found.
The implpementation is imperfect and may grab a package alias instead of the
acutal package name."
  (let* ((bol (save-excursion
                (beginning-of-line)
                (point))))
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

  ;; full-url sample: https://pkg.go.dev/builtin@go1.19.4#make
  (let ((full-url (concat my-go-doc-base-url
                          "/" pack "@go" my-go-ver "#" txt)))
    (browse-url full-url)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Central X-roads fn.
;;; Sits between the user entry point funcs and view-doc funcs.
;;; Gets text at point. Scrapes package name.
;;; Then shows the doc via the specified view-type.

(defun my-go-doc (view-type)
  "Show doc for thing at point.
Displays the doc in a view defined by VIEW-TYPE.
Possible values: `local', `website'"
  (let* ((txt (my-thing-at-point))
         (pack (completing-read "package: "
                                '() nil nil
                                (or (my-go-doc-scrape-package) ;; imperfect attempt to scrape package from text
                                    "builtin"))))
    (cond ((eq view-type 'local)
           (my-go-doc--open-local pack txt))
          ((eq view-type 'website)
           (my-go-doc--open-website pack txt))
          (t (message (format "Unsupported view-type %s"
                              (symbol-name (or view-type 'unspecified))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry point functions used by the user.

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
