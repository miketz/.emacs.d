;;; my-csharp-helpers.el --- helper funcs for C# code -*- lexical-binding: t -*-

(require 'thingatpt)
(require 'rg)

;;;###autoload
(defun my-cs-find-class ()
  "Find class/struct definition."
  (interactive)
  ;; shadow `rg-command-line-flags' for duration this let statement.
  (let* ((rg-command-line-flags rg-command-line-flags)
         (cursor-txt (thing-at-point 'symbol 'no-properties))
         (class (completing-read "class/struct: " '() nil nil
                                  ;; default to text under cursor
                                  cursor-txt))
         ;; acutally a single \. double \\ is for the elisp string escape.
         (regex (concat "(class|struct) " class)))
    ;; TODO: look into extra ignore options.
    ;; (add-to-list 'rg-command-line-flags "--glob '!*_test.go'")
    ;; run search
    (rg regex
        (rg-read-files)
        (read-directory-name "dir: " nil nil t))))

;;;###autoload
(defun my-cs-find-method ()
  "Find method definition."
  (interactive)
  ;; shadow `rg-command-line-flags' for duration this let statement.
  (let* ((rg-command-line-flags rg-command-line-flags)
         (cursor-txt (thing-at-point 'symbol 'no-properties))
         (fn (completing-read "method: " '() nil nil
                                  ;; default to text under cursor
                                  cursor-txt))
         ;; acutally a single \. double \\ is for the elisp string escape.
         (regex (concat " p.+ " fn)))
    ;; TODO: look into extra ignore options.
    ;; (add-to-list 'rg-command-line-flags "--glob '!*_test.go'")
    ;; run search
    (rg regex
        (rg-read-files)
        (read-directory-name "dir: " nil nil t))))

;;; my-csharp-helpers.el ends here