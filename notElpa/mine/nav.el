;;; nav.el --- back/forward in mark ring and xref. -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Keywords: convenience
;;; Package-Requires: ((emacs "24.1"))
;;; Version: 0.1.0
;;; URL: TODO

;;; Commentary:
;;; Attempt to have a general "go back/forward" through marks and xref
;;; jumps.

;;; Installation:

;;; Code:

(defun nav-forward-global-mark ()
  "Go forward through global-mark-ring.
The counterpart to `pop-global-mark' for going back."
  (interactive)
  (let* ((m (car (last mark-ring)))
         (buffer (marker-buffer m))
         (position (marker-position m)))
    (setq mark-ring (nconc (list m)
                           (nbutlast mark-ring)))
    (set-buffer buffer)
    (or (and (>= position (point-min))
             (<= position (point-max)))
        (if widen-automatically
            (widen)
          (error "Global mark position is outside accessible part of buffer %s"
                 (buffer-name buffer))))
    (goto-char position)
    (switch-to-buffer buffer)
    ))

(global-set-key (kbd "C-M-j") (lambda ()
                                (interactive)
                                (let ((current-prefix-arg '(4)))
                                  (call-interactively #'set-mark-command))))
(global-set-key (kbd "C-M-k") #'nav-forward-global-mark)

;;; nav.el ends here
