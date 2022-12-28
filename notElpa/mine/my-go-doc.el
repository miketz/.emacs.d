;;; my-go-doc.el --- Show doc for thing at point -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Keywords: convenience
;;; Package-Requires: ((s "1.13.1"))
;;; Version: 0.1.0
;;; URL: n/a

;;; Commentary:
;;; Show doc for thing at point.
;;; It will attempt to guess or scrape the text for the package name.  You must
;;; then manually confirm or fix the "guessed" package name.


;;; Installation:
;;; Place `my-go-doc.el' in folder `/your/chosen/folder'.
;;; Add the following text to your .emacs or init.el file:
;;;
;;; (add-to-list 'load-path "/your/chosen/folder")
;;; (autoload #'my-go-doc "my-go-doc" nil t)
;;; (autoload #'my-go-doc-website "my-go-doc" nil t)
;;; (autoload #'my-go-doc-website-overview "my-go-doc" nil t)
;;; (with-eval-after-load 'go-mode
;;;   ;; reccomended key binds
;;;   (define-key go-mode-map (kbd "C-c C-d d") #'my-go-doc)
;;;   (define-key go-mode-map (kbd "C-c C-d C-d") #'my-go-doc))


;;; Code:
(require 'thingatpt)
(require 's)

(defun my-go-doc-assert-dependencies ()
  "Check for required dependencies.  Warn the user if any are missing."
  (unless (executable-find "go")
    (display-warning
     'my-go-doc
     "my-go-doc requires go to be installed."
     :error)))

;; Invoke dependency check at load time of my-go-doc.
;; Only one check.  Don't prevent use of the feature.  Just warn then let the
;; chips fall where they may.
(my-go-doc-assert-dependencies)

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

;;;###autoload
(defun my-go-doc ()
  "Show doc for thing at point in an Emacs buffer.
Uses command line tool [go doc]."
  (interactive)
  (let* ((txt (my-thing-at-point))
         (pack (completing-read "package: "
                                '() nil nil
                                (or (my-go-doc-scrape-package) ;; imperfect attempt to scrape package from text
                                    "builtin")))
         (doc (shell-command-to-string (concat "go doc --all "
                                               (if (and pack (> (length pack) 0))
                                                   (concat pack ".")
                                                 "")
                                               txt))))
    (switch-to-buffer-other-window (get-buffer-create "*go-doc*"))
    (erase-buffer)
    (insert doc)))


(defvar my-go-doc-base-url "https://pkg.go.dev")

;;;###autoload
(defun my-go-doc-website ()
  "Show doc for thing at point in in a browser.
Uses website https://pkg.go.dev"
  (interactive)
  (let* ((txt (my-thing-at-point))
         (pack (completing-read "package: "
                                '() nil nil
                                (or (my-go-doc-scrape-package) ;; imperfect attempt to scrape package from text
                                    "builtin")))
         ;; url format: https://pkg.go.dev/builtin@go1.19.4#make
         (full-url (concat my-go-doc-base-url
                           "/" pack "@go" my-go-ver "#" txt)))
    (browse-url full-url)))

;;;###autoload
(defun my-go-doc-website-overview ()
  "Show top level docs in a browser.
Uses website https://pkg.go.dev"
  (interactive)
  (browse-url (concat my-go-doc-base-url "/std")))


(provide 'my-go-doc)

;;; my-go-doc.el ends here