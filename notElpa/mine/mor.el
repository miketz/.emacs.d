;;; mor.el --- Mode on region -*- lexical-binding: t -*-

;;; Commentary:
;;; Copy highlighted region to a new buffer, then turn on a mode.
;;; Copy the text back with `mor-copy-back'

;;; Code:
(require 'cl-lib)
(require 'rx)

(defvar mor-counter 0
  "Sequential counter used to generate unique names for temp buffers.")
(defvar mor-start nil "Start of region.") ; TODO: use buffer local
(defvar mor-end nil "End of region.")     ; TODO: use buffer local
(defvar mor-orig-buffer nil)              ; TODO: use buffer local

(defconst mor-prefix "tmp-"
  "Prefix used for temp buffer names.")

(defun mor--gen-buffer-name ()
  "Generate a unique buffer name."
  (prog1
      (concat mor-prefix
              (buffer-name (current-buffer))
              "-"
              (int-to-string mor-counter))
    (cl-incf mor-counter)))

(defun mor--str-starts-with-p (string prefix)
  "Return t if STRING begins with PREFIX."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(defun mor-kill-tmp-buffers ()
  "Delete the junk tmp buffers."
  (interactive)
  (dolist (b (buffer-list))
    (when (mor--str-starts-with-p (buffer-name b) mor-prefix)
      (kill-buffer b))))

;;;###autoload
(defun mor-mode-on-region (start end)
  "Switch to a new buffer with the highlighted text.
Turn on the selected mode.
Region is between START and END inclusive."
  (interactive "r")
  (mor--mode-on-region start
                       end
                       (intern (completing-read
                                "Mode: "
                                (mapcar (lambda (e)
                                          (list (symbol-name e)))
                                        (apropos-internal "-mode$" #'commandp))
                                nil t))))

;;;###autoload
(defun mor-emacs-lisp-mode-on-region (start end)
  "Same as `mor-mode-on-region' but use `emacs-lisp-mode'.
Region is between START and END inclusive."
  (interactive "r")
  (mor--mode-on-region start end #'emacs-lisp-mode))

(defun mor--mode-on-region (start end mode-fn)
  "The core function to copy region to a new buffer.
Region is between START and END.
MODE-FN the function to turn on the desired mode."

  ;; save buffer and region coordinates to copy the text back in later.
  (setq mor-orig-buffer (current-buffer)
        mor-start start
        mor-end end)

  (kill-ring-save start end) ;; copy higlihgted text
  (deactivate-mark)

  (switch-to-buffer-other-window (mor--gen-buffer-name))
  (yank)              ;; paste text
  (funcall mode-fn)   ;; interactively select mode, turn it on.
  (mark-whole-buffer) ;; for auto indenting
  (call-interactively #'indent-region) ;; interactive gets region autmoatically
  )

(defun mor-copy-back ()
  "Copies the temp buffer text back the original buffer.
WARNING: Overwrites the original text."
  (interactive)
  (kill-ring-save (point-min) (point-max)) ;; mode buffer text.
  (switch-to-buffer-other-window mor-orig-buffer)
  ;; highlight the region to overwrite
  (goto-char mor-start)
  (delete-char (- mor-end mor-start)) ;; (set-mark mor-end)
  ;; paste new text, overwriting old text.
  (yank))

(provide 'mor)

;;; mor.el ends here
