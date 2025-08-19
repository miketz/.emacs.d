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
(require 'cl-lib)

(defvar no-mode-line-original-format mode-line-format
  "Stores the original mode line format.")


;;;###autoload
(defun no-mode-line-enable ()
  "Hide mode line. Turn on `window-divider-mode'."
  (interactive)

  ;; hide mode line
  (setq-default mode-line-format nil)
  ;; seting default value of `mode-line-format' works for future buffers, but not current buffers!
  ;; have to set it for every exisitng buffer
  (cl-loop for b in (buffer-list)
           do
           (with-current-buffer b
             (setq mode-line-format nil)))

  ;; make horizontal window splits visible with a line via window-divider-mode
  (when (display-graphic-p) ;; TODO: figure something out for terminal display
    (setq window-divider-default-bottom-width 1)
    (setq window-divider-default-places 'bottom-only)
    (window-divider-mode t)))


(defun no-mode-line-disable ()
  "Restore the mode line. Turn off `window-divider-mode'."
  (interactive)

  ;; restore mode line
  (setq-default mode-line-format no-mode-line-original-format)
  ;; seting default value of `mode-line-format' works for future buffers, but not current buffers!
  ;; have to set it for every exisitng buffer
  (cl-loop for b in (buffer-list)
           do
           (with-current-buffer b
             (setq mode-line-format no-mode-line-original-format)))

  ;; turn off window-divider-mode. the mode line itself funcitons as a horizontal divider between windows.
  (when (display-graphic-p) ;; TODO: figure something out for terminal display
    (window-divider-mode 0)))


(provide 'no-mode-line)

;;; no-mode-line.el ends here
