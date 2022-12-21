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
;;; ;; reccomended key binds
;;; (define-key prog-mode-map (kbd "C-c w") #'my-which-func)
;;; (define-key prog-mode-map (kbd "C-c C-w") #'my-which-func)


;;; Code:
(when (featurep 'pos-tip) ;; optional depenecy for display sugar
  (require 'pos-tip))

(defvar my-which-func-use-postip (featurep 'pos-tip)
  "When t use pos-tip to display text in a popup.
Otherwise display text in the echo area.")

(defun my-which-func ()
  "Print the name of the function the cursor is currently in."
  (interactive)
  (let ((txt (save-excursion
               ;; jump to function's first line. This approach is flawed as sometimes
               ;; fn defintions are split across lines such as in the GNU C style. But
               ;; should work OK most of the time.
               (beginning-of-defun)
               (buffer-substring (line-beginning-position)
                                 (line-end-position)))))
    (cond (my-which-func-use-postip
           (pos-tip-show txt))
          (t ;; default
           (message txt)))))



(provide 'my-which-func)

;;; my-which-func.el ends here
