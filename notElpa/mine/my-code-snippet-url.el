;;; my-code-snippet-url.el --- Mode on region -*- lexical-binding: t -*-


;;; Code:

(defvar default-past-site 'debian)

(defvar paste-sites '((debian . "http://paste.debian.net/")
                      (mozilla . "https://pastebin.mozilla.org/"))
  "Websites that provide code snippet hosting.")


;; TODO: find a way to automatically paste a selected region of code into
;;       the text box.
(defun my-code-snippet-url ()
  (interactive)
  (browse-url (cdr (assoc default-past-site paste-sites))))


(provide 'my-code-snippet-url)

;;; my-code-snippet-url.el ends here
