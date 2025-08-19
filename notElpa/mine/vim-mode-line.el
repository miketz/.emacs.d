;;; vim-mode-line.el --- Hide mode line when 1 window -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Keywords: convenience
;;; Package-Requires: ((emacs "27.1"))
;;; Version: 0.1.0
;;; URL: TODO

;;; Commentary:
;;; Hide mode line when 1 window, like in Vim.

;;; Code:
(require 'cl-lib)

(defvar vim-mode-line-original-format mode-line-format
  "Stores the original mode line format.")


(defun vim-mode-line-hide-when-single-buffer ()
  "Hide the mode line if only one window is visible, otherwise show it."
  (let* ((one-win-p (= (length (window-list)) 1))
         (mlf (if one-win-p nil vim-mode-line-original-format)))
    (cl-loop for w in (window-list)
             do
             (with-current-buffer (window-buffer w)
               (setq-local mode-line-format mlf)))
    ;; minibuffer sometimes leaves behind a phantom mode line when ido content does not fit on screen horizontally.
    ;; no longer need this now that new hook avoids temporarly modeline popup on minibuffer use.
    ;; (redraw-display)
    ))




(defun vim-mode-line-enable ()
  "Add hook(s) to hide/show the mode line."
  ;; (add-hook 'window-state-change-hook #'vim-mode-line-hide-when-single-buffer)
  (add-hook 'window-configuration-change-hook #'vim-mode-line-hide-when-single-buffer)

  ;; TODO: see NEWS.27 for Window change function info. use a finer grained hook(s) if possible to reduce
  ;; uneccesary runs.
  )

(defun vim-mode-line-disable ()
  "Remove the hook(s) which hide/show the mode line.
Also restore the mode line in all buffers."
  ;; (remove-hook 'window-state-change-hook #'vim-mode-line-hide-when-single-buffer)
  (remove-hook 'window-configuration-change-hook #'vim-mode-line-hide-when-single-buffer)

  (cl-loop for b in (buffer-list)
           do
           (with-current-buffer b
             (setq mode-line-format vim-mode-line-original-format))))


;;;###autoload
(define-minor-mode vim-mode-line-mode
  "Hide mode line when 1 window, like in Vim."
  :require 'vim-mode-line
  ;; global mode. it acts on visible window buffers not just current buffer. It also needs to consider all buffers
  ;; when disabling the mode so there are no stray buffers left behind without their mode line restored.
  :global t
  ;; this mode hides mode lines, so for now just use empty string. C-h m would be a better way to observe modes.
  :lighter ""
  (if vim-mode-line-mode ; on?
      (vim-mode-line-enable)
    (vim-mode-line-disable)))

(provide 'vim-mode-line)

;;; vim-mode-line.el ends here