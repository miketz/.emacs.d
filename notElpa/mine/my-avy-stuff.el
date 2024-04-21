;;; my-avy-stuff.el --- extra funcs for avy -*- lexical-binding: t -*-

(require 'avy)

(defvar my-avy-keys-short)

;;;###autoload
(defun my-avy-isearch ()
  (interactive)
  ;; use more limited but easier to type keys for isearch jumps
  ;; becuase isearch jumps tend to have fewer matches so less likely to need 2 keys.
  (let ((avy-keys my-avy-keys-short))
    (avy-isearch)))


;;; my-avy-stuff.el ends here