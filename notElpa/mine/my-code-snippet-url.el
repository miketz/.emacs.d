;;; my-code-snippet-url.el --- Code snippet url -*- lexical-binding: t -*-

;;; Commentary:
;;; Helper function for hosting code snippets.

;;; Code:

(defvar paste-sites '((debian . "http://paste.debian.net/")
                      (mozilla . "https://pastebin.mozilla.org/")
                      (bpa . "https://bpa.st/"))
  "Websites that provide code snippet hosting.")

(defvar default-paste-site 'debian
  "Website to use.")

;;;###autoload
(defun my-get-paste-url ()
  "Get the url for the current paste site."
  (cdr (assoc default-paste-site paste-sites)))

;; TODO: find a way to automatically paste a selected region of code into
;;       the text box in the browser..
;;;###autoload
(defun my-code-snippet-url ()
  "Open the paste site in a browser."
  (interactive)
  (browse-url (my-get-paste-url)))


(provide 'my-code-snippet-url)
;;; my-code-snippet-url.el ends here
