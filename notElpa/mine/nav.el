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
(require 'hydra)
(require 'cl-lib)

(setq mark-ring-max 4) ; small for testing purposes.

(defun nav-forward ()
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
    (switch-to-buffer buffer))
  (nav-draw-ring))

;; (defun nav-back ()
;;   (interactive)
;;   (let ((current-prefix-arg '(4)))
;;     (call-interactively #'set-mark-command))
;;   (nav-draw-ring))

(defun nav-back ()
  "Implemented by scraping logic from `pop-global-mark'."
  (interactive)
  (let* ((m (car (last mark-ring)))
         (buffer (marker-buffer m))
         (position (marker-position m)))
    (setq mark-ring (nconc (cdr mark-ring)
				           (list (car mark-ring))))
    (set-buffer buffer)
    (or (and (>= position (point-min))
             (<= position (point-max)))
        (if widen-automatically
            (widen)
          (error "Global mark position is outside accessible part of buffer %s"
                 (buffer-name buffer))))
    (goto-char position)
    (switch-to-buffer buffer))
  (nav-draw-ring))


(defun nav-draw-ring ()
  (interactive)
  (let ((curr-b (current-buffer))
        (output "")
        (output-b (get-buffer-create "*mark-ring*")))
    (with-current-buffer curr-b
      (cl-loop for m in mark-ring
               do
               (if (and (not (null m))
                        (not (null (marker-position m))))
                   (let ((str (concat (buffer-name (marker-buffer m)) " "
                                      (number-to-string (marker-position m)) "\n")))
                     (setq output (concat output str)))
                 ;; else bad marker in ring? in theory will never happen.
                 (setq output (concat output "bad marker\n")))))
    (with-current-buffer output-b
      (erase-buffer)
      (insert output))
    (display-buffer output-b)))



;;;----------------------------------------------------------------------------
;;; UI
;;;----------------------------------------------------------------------------
;;;###autoload
(defhydra nav-hydra (:color amaranth :hint nil)
  "
_j_, _h_: back
_k_, _l_: forward
_q_, _C-g_: quit"
  ("j" nav-back)
  ("h" nav-back)
  ("k" nav-forward)
  ("l" nav-forward)
  ("C-g" nil nil)
  ("q" nil))

(global-set-key (kbd "C-c n") #'nav-hydra/body)
;; (global-set-key (kbd "C-M-j") #'nav-back)
;; (global-set-key (kbd "C-M-k") #'nav-forward)

(provide 'nav)

;;; nav.el ends here
