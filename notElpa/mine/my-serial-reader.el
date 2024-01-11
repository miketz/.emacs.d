;;; -*- lexical-binding: t -*-
;;; for _0x4aV on #emacs irc
;;; wanted something like https://accelareader.com/ in Emacs.
;;; Display some text only 1 word at a time. Show the next word after a delay.

;;; URL: https://raw.githubusercontent.com/miketz/.emacs.d/master/notElpa/mine/my-serial-reader.el

;;; TODO: centered view option? something like darkroom-mode?

(require 'cl-lib)

(defvar my-buff-name "*serial-reader*")

(defvar my-delay-seconds 0.4)

(defvar my-sr-font-scale-level 4
  "Number of steps to scale font size.
Positive numbers will increase font size.
0 will have no effect on font size.
Negative numbers will decrease font size which you probably don't want.")

(defvar my-timer nil)

(define-minor-mode my-serial-reader-mode
  "This minor mode is just to support key binds."
  :lighter " serial-reader"
  ;; Ideally users should choose their own key binds. But it is important they
  ;; be able to STOP the serial reader easily. So I'm taking the liberty of
  ;; binding a key for them. This binding will be shown to the user in the
  ;; header of the output buffer.
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c q") #'my-stop-serial-reader)
            map))

;;;###autoload
(cl-defun my-serial-reader (&optional start end)
  "Entry point function.
Display current buffer text 1 word at a time in new buffer `my-buff-name'.
Uses selected region if available, otherwise the entire buffer text."

  ;; NOTE: avoiding (interactive "r"). It breaks in the case where Emacs has
  ;; just started up with no mark set yet.
  (interactive (if (use-region-p)
                   ;; use selected region for `start' and `end'
                   (list (region-beginning) (region-end))
                 ;; else use entire buffer
                 (list (point-min) (point-max))))

  ;; stop any running serial reader from a previous invocation.
  ;; for this style of display users can only read 1 buffer at a time, so
  ;; there is little reason to allow multiple serial readers to run at the same time.
  (unless (null my-timer)
    (my-stop-serial-reader))

  ;; TODO: find a way to get the words as a "stream" instead of a giant list
  (let* ((txt (buffer-substring-no-properties start end))
         (words (split-string txt))
         (buff (get-buffer-create my-buff-name)))

    ;; GUARD: there must be at least 1 word to display.
    (when (= (length words) 0)
      (message "No words to display.")
      (cl-return-from my-serial-reader))

    (switch-to-buffer-other-window buff)

    (with-current-buffer buff
      ;; scale font size to configured value
      (text-scale-set my-sr-font-scale-level)

      ;; turn on mode. supports key binds, and the kill-buffer-hook
      (my-serial-reader-mode)

      ;; add a fancy header to the buffer. With info on how to abort.
      (set (make-local-variable 'header-line-format)
           (substitute-command-keys
            "serial reader     [Abort]: \\[my-stop-serial-reader]")))

    ;; show a word every `my-delay-seconds' via a timer.
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
  (message "attempting to stop serial reader")
  (if (timerp my-timer)
      (progn
        (cancel-timer my-timer)
        ;; Set to nil to make the timer object "extra canceled" and eligible for garbage collection.
        (setq my-timer nil)
        (message "stopped serial reader!"))
    ;; else
    (message "serial reader was already stopped.")))


;; use a hook to cancel the timer if output buffer is killed
(add-hook 'my-serial-reader-mode-hook
          (lambda ()
            (message (format "adding clean up hook to buffer %s"
                             (buffer-name (current-buffer))))
            (add-hook 'kill-buffer-hook
                      #'my-stop-serial-reader
                      nil
                      ;; only add hook to the output buffer, not all buffers!
                      'make-it-local)))


(provide 'my-serial-reader)
