;;; my-which-func.el --- Show function name -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Keywords: convenience
;;; Package-Requires: ()
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
;;; (autoload #'my-which-func "my-which-func" nil t)
;;; ;; reccomened key binds
;;; (global-set-key (kbd "C-c w") #'my-which-func)
;;; (global-set-key (kbd "C-c C-w") #'my-which-func)


;;; Code:

(defun my-which-func ()
  "Print the name of the function the cursor is currently in."
  (interactive)
  (save-excursion
    ;; jump to function's first line. This approach is flawed as sometimes
    ;; fn defintions are split across lines such as in the GNU C style. But
    ;; should work OK most of the time.
    (beginning-of-defun)
    (let ((fn-first-line-txt (buffer-substring (line-beginning-position)
                                               (line-end-position))))
      (message fn-first-line-txt))))



(provide 'my-which-func)

;;; my-which-func.el ends here
