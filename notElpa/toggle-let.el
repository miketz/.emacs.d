;; toggle-let.el -*- lexical-binding: t; -*-

;; from sauntcartas on reddit
;; https://old.reddit.com/r/emacs/comments/1qyp1rk/toggle_between_let_and_let/

(defun toggle-let* ()
  "Make the closest enclosing let or let* form a let* or let form, respectively,
then reindent that form's bindings. "
  (interactive)
  (save-excursion
    (while (progn
             (condition-case nil
                 (backward-up-list nil t)
               (scan-error (error "Not within a let form")))
             (not (search-forward-regexp (rx point "(" (* space) "let" (?  "*") symbol-end) nil t))))
    (if (= ?* (char-before))
        (backward-delete-char 1)
      (insert "*"))
    (indent-region (point) (progn (forward-sexp) (point)))))
