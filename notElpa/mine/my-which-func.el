;;; my-which-func.el --- Show function name -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Keywords: convenience
;;; Package-Requires: ((pos-tip "0.4.6"))
;;; Version: 0.1.0
;;; URL: n/a

;;; Commentary:
;;; My alternative to `which-function-mode'.
;;; `which-function-mode' is OK, but it turns on the mode globally for all
;;; buffers which is annoying.  And if the function fits on screen then it's
;;; just wasted modeline space.  It also seems to be constantly polling and
;;; slow?
;;;
;;; This is an alternative fn which jumps to the function header via C-M-a
;;; `beginning-of-defun'.  Then prints that line of text.
;;;
;;; NOTE: lexical binding is used as a potential micro-optimization for
;;; variable look-ups.  This package *should* work the same whether lexical or
;;; dynamic binding is used.  (ie no closures)


;;; Installation:
;;; Place `my-which-func.el' in folder `/your/chosen/folder'.
;;; Add the following text to your .emacs or init.el file:
;;;
;;; (add-to-list 'load-path "/your/chosen/folder")
;;; (autoload #'my-which-func "my-which-func" nil t)
;;; ;; prefer popup display if pos-tip is installed.
;;; (setq my-which-func-use-postip t)
;;; ;; reccomended key binds
;;; (define-key prog-mode-map (kbd "C-c w") #'my-which-func)
;;; (define-key prog-mode-map (kbd "C-c C-w") #'my-which-func)


;;; Code:
(require 'pos-tip "pos-tip" 'noerror) ; optional dependency for popup

(defvar my-which-func-use-postip (featurep 'pos-tip)
  "When t use pos-tip to display text in a popup.
A popup is nice becuase your
eyes don't have to travel to a different location on the screen.")

(defun my-which-func ()
  "Print the name of the function the cursor is currently in.
Type \\[universal-argument] before calling this fn to provide a `prefix-arg'.
If a prefix arg is found it will insert the text into the buffer."
  (interactive)
  (let ((txt (save-excursion
               ;; jump to function's first line. This approach is flawed as sometimes
               ;; fn defintions are split across lines such as in the GNU C style. But
               ;; should work OK most of the time.
               (beginning-of-defun)
               (buffer-substring-no-properties (line-beginning-position)
                                               (line-end-position)))))
    ;; always display in echo area. even if popups are enabled.
    (message txt)
    ;; extra sugar popup display. Could use one of several popup implmenetations.
    (cond ((and my-which-func-use-postip
                (fboundp #'pos-tip-show)
                ;; pos-tip breaks in terminal on mac. maybe breaks on other terms too?
                (display-graphic-p))
           (pos-tip-show txt)))
    ;; if fn invoked with C-u prefix insert text. Like in emacs-lisp-mode.
    (when current-prefix-arg
      (move-end-of-line 1)
      (newline)
      (insert txt))))



(provide 'my-which-func)

;;; my-which-func.el ends here
