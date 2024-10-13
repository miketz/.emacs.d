;;; my-auto-fill-helpers.el --- helper funcs for auto-fill -*- lexical-binding: t -*-

(require 'simple)
(require 'cl-lib)


;;;###autoload
(cl-defun my-auto-fill-turn-on-comments-only (fc)
  "Turn on auto-fill-mode. And affect comments only, not code."
  ;; populate `fc' in interactive block. using this instead of the "n"
  ;; flag so i can put in a default value.
  (interactive
   (list (completing-read "fill-column: " nil nil nil
                          ;; default input to current `fill-column'
                          (number-to-string fill-column))))

  ;; GUARD: fc must be a positive integer
  (unless (string-match-p "^[0-9]+$" fc)
    (message "fill-column must be a positive integer")
    (cl-return-from my-auto-fill-turn-on-comments-only))

  (setq fill-column (string-to-number fc)) ; buffer local
  (turn-on-auto-fill)
  ;; force buffer local
  (setq-local comment-auto-fill-only-comments t)
  (message "auto-fill-mode on"))

;;;###autoload
(defun my-auto-fill-turn-off-comments-only ()
  "Turn off auto-fill-mode. And revert the comments-only flag."
  (interactive)
  (turn-off-auto-fill)
  ;; force buffer local
  (setq-local comment-auto-fill-only-comments nil))


;;; my-auto-fill-helpers.el ends here