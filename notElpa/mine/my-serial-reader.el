;;; -*- lexical-binding: t -*-
;;; for _0x4aV on #emacs irc
;;; wanted something like https://accelareader.com/ in Emacs.
;;; Display some text only 1 word at a time. Show the next word after a delay.

;;; TODO: bigger font option?
;;; TODO: centered view option? something like darkroom-mode?
;;; TODO: stop timer if user forcefully kills the output buffer. buffer kill hook?
;;; TODO: define a mode for use in the output buffer.
;;;       should allow keybinds in the output buffer.
;;;       As well as hooks to cancel the timer if the output buffer is killed.

(require 'cl-lib)

(defvar my-buff-name "*serial-reader*")

(defvar my-delay-seconds 0.4)

(defvar my-timer nil)

;;;###autoload
(defun my-serial-reader (start end)
  "Entry point function.
Display current buffer text 1 word at a time in new buffer `my-buff-name'.
Uses selected region if available, otherwise the entire buffer text."
  (interactive "r")

  ;; use selected region by default. otherwise use entire buffer text.
  (unless (use-region-p)
    (setq start (point-min))
    (setq end (point-max)))

  ;; TODO: find a way to get the words as a "stream" instead of a giant list
  (let* ((txt (buffer-substring-no-properties start end))
         (words (split-string txt))
         (buff (get-buffer-create my-buff-name)))

    (switch-to-buffer-other-window buff)

    ;; add a fancy header to the buffer. With info on how to abort.
    (with-current-buffer buff
      (set (make-local-variable 'header-line-format)
           (substitute-command-keys
            "serial reader     [Abort]: \\[my-stop-serial-reader]")))

    ;; stop any running serial reader from a previous invocation.
    (my-stop-serial-reader)

    (setq my-timer (run-with-timer
                    0 my-delay-seconds
                    (let ((i 0))
                      (lambda ()
                        ;; NOTE: use (get-buffer-create) not the cached `buff' var.
                        ;; In case user kills the buffer it will be re-created so
                        ;; we still have an output target. Although in theory we should
                        ;; cancel the timer if the target buffer is killed.
                        (with-current-buffer (get-buffer-create my-buff-name)
                          (erase-buffer)
                          (insert (nth i words))
                          (cl-incf i)
                          (when (and (>= i (length words))
                                     (timerp my-timer))
                            (cancel-timer my-timer)))))))))


(defun my-stop-serial-reader ()
  "Stop the display of text into buffer `my-buff-name'.
Cancels `my-timer'.
Call this if the serial display is taking too long."
  (interactive)
  (when (timerp my-timer)
    (cancel-timer my-timer)))


(provide 'my-serial-reader)