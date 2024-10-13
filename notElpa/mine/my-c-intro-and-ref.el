;;; my-c-intro-and-ref.el --- View C book from rms -*- lexical-binding: t -*-

;;; Commentary:
;;; Helper functions to view the book c-intro-and-ref.html
;;; Also support booksmarks tailored for `eww'.  Line number based.
;;; Using namespace prefix "cir" short for "C Intro and Ref".

;;; Code:
(require 'f)
(defvar my-curr-computer) ; defined in init.el

;; folder where the book lives
(defvar cir-folder (cond ((eq my-curr-computer 'mac-mini-m1-2021)
                          "~/books/C/")
                         ((eq my-curr-computer 'work-laptop-2019)
                          "c:/users/mtz/books/")
                         (t nil)))


;; filename of the book
(defvar cir-filename "c-intro-and-ref.html")

(defvar cir-filepath (concat cir-folder
                             cir-filename))


;; put the bookmark file in the same folder as the html book.
(defvar cir-bookmark (when cir-folder
                       (concat cir-folder
                               "c-intro-and-ref-bookmark.txt")))

(defun cir-bookmark-save ()
  "Create a bookmark for c intro and ref book."
  (interactive)
  ;; delete existing file
  (when (and (not (null cir-bookmark))
             (file-exists-p cir-bookmark))
    (delete-file cir-bookmark))

  ;; replace with new file and line number
  (f-write-text (number-to-string (line-number-at-pos))
                'utf-8
                cir-bookmark))

(defun cir--bookmark-get-line-num ()
  "Get the linenumber of the book mark for c intro and ref book."
  (let ((file cir-bookmark))
    (when (file-exists-p file)
      (string-to-number (f-read file 'utf-8)))))

(defun cir--goto-bookark ()
  "Jump to the bookmark."
  (let ((line (cir--bookmark-get-line-num)))
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))))

(cl-defun cir-goto-bookmark ()
  "Jump to the bookmark.
Assumes the book is already open in an eww buffer."
  (interactive)
  (unless (string-equal "*eww*"
                        (buffer-name (current-buffer)))
    (message "You must be in an *eww* buffer viewing c-intro-and-ref.html.")
    (cl-return-from cir-goto-bookmark))

  (cir--goto-bookark))

;;;###autoload
(cl-defun cir-open-book ()
  "Open the book c intro and ref.
And jump to a saved bookmark if it is found."
  (interactive)
  ;; GUARD: make sure book is set up on this computer.
  (unless (file-exists-p cir-filepath)
    (message "Book not set up on this computer.")
    (cl-return-from cir-open-book))

  ;; set up window splits
  (delete-other-windows)
  (switch-to-buffer "c-test")
  (c-mode)
  (split-window-horizontally)
  (shrink-window-horizontally 10)

  ;; open the book
  (eww-open-file cir-filepath)

  ;; jump to bookmark if it has one.
  (cir--goto-bookark))

;; I have a lot of helper functions that open books with a "my-proj" prefix.
;; Maintain that here with an alias.
(defalias 'my-proj-c-intro-and-ref 'cir-open-book)

(provide 'my-c-intro-and-ref)

;;; my-c-intro-and-ref.el ends here
