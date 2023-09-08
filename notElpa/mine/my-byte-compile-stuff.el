;;; -*- lexical-binding: t -*-

;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(defun my-delete-elc-files (dir-str)
  "Delete all elc files in folder DIR-STR."
  (let ((elc-files (directory-files-recursively
                    dir-str
                    "\.elc$")))
    ;; Optionally, on linux manually delete. First verify file list with:
    ;;   find . -name "*.elc" -type f
    ;; then run:
    ;;   find . -name "*.elc" -type f -delete
    (cl-loop for f in elc-files
             do
             (delete-file f))))

(defun my-byte-compile-curr-dir ()
  "Byte compile all elisp files in the current directory."
  (interactive)
  ;; delete all "elc" files first. Sometimes there are issues where an
  ;; elc is loaded which breaks compliation of an el file. Rare, but makes an
  ;; impossible to hunt down issue when it happens.
  (my-delete-elc-files default-directory) ; current directory

  (byte-recompile-directory
   default-directory   ; current directory
   0                   ; 0 means compile .el files if .elc is missing.
   t) ; t means force re-compile even if the .elc is up-to-date. May be
  )

(provide 'my-byte-compile-stuff)