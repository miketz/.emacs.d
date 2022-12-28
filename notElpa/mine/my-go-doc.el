;;; my-go-doc.el --- Show doc for thing at point -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Keywords: convenience
;;; Package-Requires: ((s "1.13.1"))
;;; Version: 0.1.0
;;; URL: n/a

;;; Commentary:
;;; Show doc for thing at point


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

(defvar my-go-ver (or (my-go-get-version) "1.19.4"))


(defun my-thing-at-point ()
  "Return the text at point."
  (interactive)
  (let ((str (thing-at-point 'symbol 'no-properties)))
    (print str)))

(cl-defun my-go-doc-scrape-package ()
  "Try to scrape out the package name.
Basically searching backwards for a peroid (.).
Then grabs the text before the (.).
Returns nil if no package found."
  (let* ((bol (save-excursion
                (beginning-of-line)
                (point))))
    (save-excursion
      (let ((dot (search-backward "." bol t)))
        ;; GUARD: dot not found
        (when (null dot)
          (cl-return-from my-go-doc-scrape-package nil))
        (let ((pack (save-excursion
                      (search-backward " " bol t)))) ;; space
          (when (null pack) ;; try search again using tab
            (setq pack (search-backward "	" bol t))) ;; tab
          (when (and pack dot) ;; found both
            (setq pack (+ 1 pack)) ;; +1 to go forward past the space.
            (buffer-substring-no-properties pack dot)))))))

;;;###autoload
(defun my-go-doc ()
  "Show doc for thing at point in an Emacs buffer.
Uses command line tool [go doc]."
  (interactive)
  (let* ((txt (my-thing-at-point))
         ;; TODO: scrap package out of text. Could be hard with aliases.
         (pack (completing-read "package: "
                                '() nil nil
                                (or (my-go-doc-scrape-package)
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
         ;; TODO: scrap package out of text. Could be hard with aliases.
         (pack (completing-read "package: " '() nil nil "builtin"))
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
