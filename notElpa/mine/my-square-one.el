;;; -*- lexical-binding: t -*-

(defvar my-keep-buffers
  '("*scratch*" "*Messages*"
    ;; "*Compile-Log*"
    "*Minibuf-1*"
    "*Minibuf-0*" "*code-conversion-work*" "*Echo Area 0*"
    "*Echo Area 1*" "*helm mini*")
  "Buffers to keep alive, even when wiping all buffers.")

(defun my-square-one ()
  "Switch to the scratch buffer, then delete all other buffers.

NOTE: `my-keep-buffers' contains buffers to keep alive.
Emacs tends to crash when some of the basic buffers are absent.
I'm not certain which absences cause the crash.

It seems killing buffers gives cleanup of other things for free!
ie closing running processes (slime/swank, omnisharp, etc) and helm-cmd-t
caches.
TODO: look into an explicit way to clean up non-buffer things in case there are
edge cases not covered by buffer killing."
  (interactive)
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  ;;cl-set-difference does not work on strings.
  ;;so use a set of buffer pointers, not buffer names
  (let ((to-kill (cl-set-difference (buffer-list)
                                    (mapcar 'get-buffer my-keep-buffers))))
    (mapc 'kill-buffer to-kill)))
