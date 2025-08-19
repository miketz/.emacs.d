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


(defun no-mode-line-enable ()
  "Hide mode line. Turn on `window-divider-mode'."

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

  ;; restore mode line
  (setq-default mode-line-format no-mode-line-original-format)
  ;; seting default value of `mode-line-format' works for future buffers, but not current buffers!
  ;; have to set it for every exisitng buffer
  (cl-loop for b in (buffer-list)
           do
           (with-current-buffer b
             (setq mode-line-format no-mode-line-original-format)))

  ;; turn off window-divider-mode. the mode line itself functions as a horizontal divider between windows.
  (when (display-graphic-p) ;; TODO: figure something out for terminal display
    (window-divider-mode 0)))

;;;###autoload
(define-minor-mode no-mode-line-mode
  "Hide mode line. And try to solve the horizontal split blending issue with window-divider-mode."
  :require 'no-mode-line
  ;; global mode. Currently it needs to consider all existing buffers when disabling/enablling the mode to
  ;; properly apply the `mode-line-format'.
  :global t
  ;; this mode hides mode lines, so for now just use empty string. C-h m would be a better way to observe modes.
  :lighter ""
  (if no-mode-line-mode ; turn on?
      (no-mode-line-enable)
    (no-mode-line-disable)))


(provide 'no-mode-line)

;;; no-mode-line.el ends here
