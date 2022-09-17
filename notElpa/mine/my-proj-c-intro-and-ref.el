;;; helper functions to view the book c-intro-and-ref.html

(require 'f)

;; folder where the book lives
(defvar c-intro-book-folder (cond ((eq my-curr-computer 'mac-mini-m1-2021)
                                   "~/books/")
                                  ((eq my-curr-computer 'work-laptop-2019)
                                   "c:/users/mtz/books/")
                                  (t nil)))


;; filename of the book
(defvar c-intro-book-filename "c-intro-and-ref.html")

(defvar c-intro-book-filepath (concat c-intro-book-folder
                                      c-intro-book-filename))


;; put the bookmark file in the same folder as the html book.
(defvar c-intro-bookmark-filepath (when c-intro-book-folder
                                    (concat c-intro-book-folder
                                            "c-intro-and-ref-bookmark.txt")))

;;;###autoload
(defun my-bookmark-save ()
  (interactive)
  ;; delete existing file
  (when (and (not (null c-intro-bookmark-filepath))
             (file-exists-p c-intro-bookmark-filepath))
    (delete-file c-intro-bookmark-filepath))

  ;; replace with new file and line number
  (f-write-text (number-to-string (line-number-at-pos))
                'utf-8
                c-intro-bookmark-filepath))

;;;###autoload
(defun my-bookmark-get-line-num ()
  (interactive)
  (let ((file c-intro-bookmark-filepath))
    (when (file-exists-p file)
      (string-to-number (f-read file 'utf-8)))))

;;;###autoload
(cl-defun my-proj-c-intro-and-ref ()
  (interactive)
  ;; GUARD: make sure book is set up on this computer.
  (unless (file-exists-p c-intro-book-filepath)
    (message "Book not set up on this computer.")
    (cl-return-from my-proj-c-intro-and-ref))

  ;; set up window splits
  (delete-other-windows)
  (switch-to-buffer "c-test")
  (c-mode)
  (split-window-horizontally)
  (shrink-window-horizontally 10)

  ;; open the book
  (eww-open-file c-intro-book-filepath)

  ;; show line numbers to facilitate an ad-hoc bookmarking system.
  (setq display-line-numbers t)
  ;; make window larger to account for the line numbers
  (enlarge-window-horizontally 7)

  ;; jump to bookmark if it has one.
  (let ((line (my-bookmark-get-line-num)))
    (when line
      (goto-line line))))

(provide 'my-proj-c-intro-and-ref)