;;; my-line-nums --- Helper FNs for line numbers -*- lexical-binding: t -*-

;;; Commentary:
;;; Helper FNs for line numbers

;;; Code:

(defun my-line-numbers-on ()
  "Turn on absolute line numbers."
  (interactive)
  (setq display-line-numbers t))

(defun my-line-numbers-relative-on ()
  "Turn on relative line numbers."
  (interactive)
  (setq display-line-numbers 'relative))

(defun my-line-numbers-off ()
  "Turn off line numbers."
  (interactive)
  (setq display-line-numbers nil))




(defvar-local my-curr-line-style nil
  "The current line number style used in a buffer.
Possible values: `relative', `absolute', `off'.")

;; line number display styles. lexically bound.
(let ((styles '(relative absolute off)))
  (defun my-line-numbers-cycle ()
    "Cycle line number display styles. relative, absolute, off.
Closure over `styles'."
    (interactive)
    (setq my-curr-line-style (car (or (cdr (memq my-curr-line-style styles))
                                      styles)))
    (setq display-line-numbers (cond
                                ((eq my-curr-line-style 'relative) 'relative)
                                ((eq my-curr-line-style 'absolute) t)
                                ((eq my-curr-line-style 'off) nil)))
    (message "line numbers: %s" my-curr-line-style)))


;; ;; Attempt at turning on relative line numbers in visual mode.
;; (when my-use-evil-p
;;   (let ((cache nil)) ; lexically bound `cache'.
;;     (defun my-vis-entry ()
;;       (setq cache display-line-numbers)
;;       (unless (or (eq display-line-numbers 'relative)
;;                   ;; don't do anything if visual is from expand-region.
;;                   (memq last-command
;;                         '(er/expand-region
;;                           er/contract-region
;;                           hydra-expand-region/er/expand-region
;;                           hydra-expand-region/er/contract-region)))
;;         (setq display-line-numbers 'relative)))

;;     (defun my-vis-exit ()
;;       (unless (eq display-line-numbers cache)
;;         (setq display-line-numbers cache))))

;;   (add-hook 'evil-visual-state-entry-hook #'my-vis-entry)
;;   (add-hook 'evil-visual-state-exit-hook #'my-vis-exit))


(provide 'my-line-nums)

;;; my-line-nums.el ends here
