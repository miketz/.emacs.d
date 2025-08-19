;;; no-mode-line.el --- Hide mode line -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Keywords: convenience
;;; Version: 0.1.0
;;; URL: TODO

;;; Commentary:
;;; Hide mode line.
;;; Use window-divider-mode so window edges can still be seen when there are mutliple windows.
;;; Like feebleline but no attempt to reproduce modeline in echo area.

;;; TODO: make this a minor mode, rather than just a pair of enable/disable functions.

;;; Code:
(defvar no-mode-line-original-format mode-line-format
  "Stores the original mode line format.")


;;;###autoload
(defun no-mode-line-enable ()
  "Hide mode line. Turn on `window-divider-mode'."
  (interactive)
  (setq-default mode-line-format nil)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places 'bottom-only)
  (window-divider-mode t))

(defun no-mode-line-disable ()
  "Restore the mode line. Turn off `window-divider-mode'."
  (interactive)
  (setq-default mode-line-format no-mode-line-original-format)
  (window-divider-mode 0))


(provide 'no-mode-line)

;;; no-mode-line.el ends here
