;;; -*- lexical-binding: t -*-
;;; for _0x4aV on #emacs irc
;;; wanted something like https://accelareader.com/ in Emacs.
;;; Display some text only 1 word at a time. Show the next word after a delay.

(require 'cl-lib)

(defvar my-buff-name "*serial-reader*")

(defvar my-delay-seconds 0.4)

(defun my-serial-reader ()
  "Entry point function.
Display text in current buffer 1 word at a time."
  (interactive)
  (let* ((buffer-txt (buffer-substring-no-properties (point-min) (point-max)))
         (words (split-string buffer-txt))
         (next-fn-delay 0))

    (switch-to-buffer-other-window (get-buffer-create my-buff-name))

    (cl-loop for w in words
             do
             (let* ((word w)
                    (fn (lambda ()
                          (with-current-buffer (get-buffer-create my-buff-name)
                            (erase-buffer)
                            (insert word)))))
               (run-with-timer next-fn-delay 0 fn))
             (cl-incf next-fn-delay my-delay-seconds))))


(provide 'my-serial-reader)