;;; my-devdocs-helpers.el --- helper funcs for devdocs pkg -*- lexical-binding: t -*-

(require 'devdocs)
(require 'thingatpt)
;; (require 'browse-url) ; `browse-url' is autoloaded

;;;###autoload
(defun my-devdocs-lookup ()
  "Wrapper over `devdocs-lookup'.
Default input to `thing-at-point'."
  (interactive)
  (let ((txt (thing-at-point 'symbol 'no-properties)))
    (devdocs-lookup nil txt)))


;;;###autoload
(defun my-devdocs-lookup-browser ()
  "Wrapper over `devdocs-lookup'.
Default input to `thing-at-point'.
Open in browser as some docs have content that won't render in emacs."
  (interactive)
  (let ((txt (thing-at-point 'symbol 'no-properties)))
    ;; (devdocs-lookup nil txt)
    (let* ((entry (devdocs--read-entry "Go to documentation: "
                                       (devdocs--relevant-docs nil ;ask-docs
                                                               )
                                       txt))
           (html-file (my-devdocs-get-file entry)))
      (browse-url html-file))))


;;; sample entry object
;; ((doc
;;   (name . "HTML")
;;   (slug . "html")
;;   (type . "mdn")
;;   (links
;;    (home . "https://developer.mozilla.org/en-US/docs/Web/HTML")
;;    (code . "https://github.com/mdn/content/tree/main/files/en-us/web/html"))
;;   (mtime . 1724178303)
;;   (db_size . 5644889)
;;   (attribution . "&copy; 2005&ndash;2024 MDN contributors.<br>
;;       Licensed under the Creative Commons Attribution-ShareAlike License v2.5 or later."))
;;  (name . "select")
;;  (path . "element/select")
;;  (type . "Elements"))

(defun my-devdocs-get-file (entry)
  "Extract the html file (local disk) from the devdocs ENTRY object."
  (let-alist entry
    (let* ((shr-external-rendering-functions (append
                                              (alist-get \.doc.slug
                                                         devdocs--rendering-functions
                                                         nil nil #'string=)
                                              (alist-get \.doc.type
                                                         devdocs--rendering-functions
                                                         nil nil #'string=)
                                              (alist-get t devdocs--rendering-functions)
                                              shr-external-rendering-functions))
           (file (expand-file-name (format "%s/%s.html"
                                           \.doc.slug
                                           (url-hexify-string (devdocs--path-file \.path)))
                                   devdocs-data-dir)))
      file)))

;;;###autoload
(defun my-devdocs-install-docs ()
  "Install docs for languages I use."
  (interactive)
  (let ((docs '("c" "go" "bash" "html" "javascript" "css" "perl~5.38"
                "lua~5.4")))
    (cl-loop for d in docs
             do
             (devdocs-install d))))