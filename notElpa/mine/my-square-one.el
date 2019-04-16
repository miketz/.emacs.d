;;; my-square-one.el --- Kill all buffers -*- lexical-binding: t -*-

;;; Commentary:
;;; Kills all the open buffers.  But excludes buffers based on a white list and
;;; major mode detection.

;;; Code:

(require 'cl-lib)
;; (require 'cl-seq)


(defun my-buffer-mode (buffer-or-string)
  "Return the major mode associated with BUFFER-OR-STRING."
  (with-current-buffer buffer-or-string
    major-mode))

;; Buffers to keep alive, even when wiping all buffers.
(let ((keep-buffers
       '("*scratch*" "*Messages*"
         ;; "*Compile-Log*"
         "*Minibuf-1*"
         "*Minibuf-0*" "*code-conversion-work*" "*Echo Area 0*"
         "*Echo Area 1*" "*helm mini*"))
      (keep-modes '(erc-mode)))

  (defun my-square-one ()
    "Switch to the scratch buffer, then delete all other buffers.

NOTE: `keep-buffers' contains buffers to keep alive.
Emacs tends to crash when some of the basic buffers are absent.
I'm not certain which absences cause the crash.

`keep-modes' contains a white list of modes to keep buffers alive.  Supplying
a prefix arg via `C-u' will bypass this whitelist.

It seems killing buffers gives cleanup of other things for free!
ie closing running processes (slime/swank, omnisharp, etc) and helm-cmd-t
caches.
TODO: look into an explicit way to clean up non-buffer things in case there are
edge cases not covered by buffer killing."
    (interactive)
    (switch-to-buffer "*scratch*")
    (delete-other-windows)
    ;; ;; cl-set-difference does not work on strings.
    ;; ;; so use a set of buffer pointers, not buffer names
    ;; (let ((to-kill (cl-set-difference (buffer-list)
    ;;                                   (mapcar #'get-buffer
    ;;                                           keep-buffers))))
    ;;   (mapc 'kill-buffer to-kill))

    (cl-loop for b in (buffer-list)
             do
             (let* ((keep? (member (buffer-name b) keep-buffers))
                    (mode? (and (not keep?)
                                ;; C-u bypasses mode white list.
                                (and (not current-prefix-arg)
                                     (memq (my-buffer-mode b) keep-modes))))
                    (mini? (and (not keep?) (minibufferp b))))
               (unless (or keep? mode? mini?)
                 (kill-buffer b))))))

(provide 'my-square-one)

;;; my-square-one.el ends here
