;;; my-rust-stuff ---   -*- lexical-binding: t -*-

;;; Commentary:
;;; Various rust related helpers functions.  Open op the book, docs, etc.

;;; Code:

(defvar my-curr-computer)

(defvar my-rust-doc-dir
  (cond ((eq my-curr-computer 'wild-dog)
         "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/")
        ((eq my-curr-computer 'mac-mini-2021)
         "~/.rustup/toolchains/stable-aarch64-apple-darwin/share/doc/")
        ((eq my-curr-computer 'work-laptop-2019)
         "C:/users/mtz/.rustup/toolchains/stable-x86_64-pc-windows-msvc/share/doc/"))
  "Root folder of local rust docs.")

;;;###autoload
(defun my-rust-open-top-level-docs ()
  "Open the top level doc page."
  (interactive)
  (browse-url (concat my-rust-doc-dir
                      "rust/html/index.html")))

;;;###autoload
(defun my-rust-open-the-book ()
  "Open the book."
  (interactive)
  ;; ;; Don't use eww becuase it doens't hanlde the left-side chapter nav well.
  ;; (progn
  ;;   (delete-other-windows)
  ;;   (split-window-horizontally)
  ;;   (shrink-window-horizontally 10)
  ;;   (eww-open-file (concat my-rust-doc-dir "rust/html/book/index.html")))
  (browse-url (concat my-rust-doc-dir "rust/html/book/index.html")))


(provide 'my-rust-stuff)

;;; my-rust-stuff.el ends here