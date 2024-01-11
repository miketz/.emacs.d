;;; -*- lexical-binding: t -*-
;;; for _0x4aV on #emacs irc
;;; wanted something like https://accelareader.com/ in Emacs.
;;; Display some text only 1 word at a time. Show the next word after a delay.

;;; TODO: bigger font option?
;;; TODO: centered view option? something like darkroom-mode?
;;; TODO: stop timer if user forcefully kills the output buffer. buffer kill hook?

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
         (words (split-string txt)))

    (switch-to-buffer-other-window (get-buffer-create my-buff-name))

    ;; add a fancy header to the buffer. With info on how to abort.
    (set (make-local-variable 'header-line-format)
         (substitute-command-keys
          "serial reader     [Abort]: \\[my-stop-serial-reader]"))

    ;; stop any running serial reader from a previous invocation.
    (my-stop-serial-reader)

    (setq my-timer (run-with-timer
                    0 my-delay-seconds
                    (let ((i 0))
                      (lambda ()
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