;;; -*- lexical-binding: t -*-
;;; for _0x4aV on #emacs irc
;;; wanted something like https://accelareader.com/ in Emacs.
;;; Display some text only 1 word at a time. Show the next word after a delay.

;;; TODO: bigger font option?
;;; TODO: centered view option? something like darkroom-mode?

(require 'cl-lib)

(defvar my-buff-name "*serial-reader*")

(defvar my-delay-seconds 0.4)

(defvar my-timers '()
  "All timers we create will be stored in this list.
This makes it easier to cancel them and clean things up.")


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
  ;;       split-string is slow on huge buffers.
  (let* ((buffer-txt (buffer-substring-no-properties start end))
         (words (split-string buffer-txt))
         (next-fn-delay 0))

    (switch-to-buffer-other-window (get-buffer-create my-buff-name))

    ;; add a fancy header to the buffer. With info on how to abort.
    (set (make-local-variable 'header-line-format)
         (substitute-command-keys
          "serial reader     [Abort]: \\[my-stop-serial-reader]"))

    ;; stop any running serial reader from a previous invocation.
    (my-stop-serial-reader)

    (cl-loop for w in words
             do
             (let* ((word w)
                    (fn (lambda ()
                          (with-current-buffer (get-buffer-create my-buff-name)
                            (erase-buffer)
                            (insert word))))
                    ;; TODO: find a better way than an increasing delay?
                    ;; it may not be guaranteed to execute in the correct order.
                    ;; i'm not familiar with elsip timers so just doing the first
                    ;; thing that works.
                    (timer (run-with-timer next-fn-delay 0 fn)))
               ;; keep track of the timer in my-timers so we can cancel it
               ;; later if needed.
               (push timer my-timers))
             (cl-incf next-fn-delay my-delay-seconds))))


(defun my-stop-serial-reader ()
  "Stop the display of text into buffer `my-buff-name'.
Cancels all the timers.
Call this if the serial display is taking too long."
  (interactive)
  (cl-loop for timer in my-timers
           do
           (cancel-timer timer))
  (setq my-timers '()))


(provide 'my-serial-reader)