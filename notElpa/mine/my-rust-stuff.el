;;; my-rust-stuff ---   -*- lexical-binding: t -*-

;;; Commentary:
;;; Various rust related helpers functions.  Open op the book, docs, etc.

;;; Code:

(defvar my-curr-computer)

(defvar my-rust-doc-dir
  (cond ((eq my-curr-computer 'wild-dog)
         "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/"))
  "Root folder of local rust docs.")


(defvar my-rust-the-book-dir (concat my-rust-doc-dir
                                     "rust/html/book/"))
(defvar my-rust-the-book-index (concat my-rust-the-book-dir
                                       "index.html"))

;;;###autoload
(defun my-rust-open-the-book ()
  "Open the book."
  (interactive)
  ;; ;; Don't use eww becuase it doens't hanlde the left-side chapter nav well.
  ;; (progn
  ;;   (delete-other-windows)
  ;;   (split-window-horizontally)
  ;;   (shrink-window-horizontally 10)
  ;;   (eww-open-file my-rust-the-book-index))
  (browse-url my-rust-the-book-index))


(provide 'my-rust-stuff)

;;; my-rust-stuff.el ends here