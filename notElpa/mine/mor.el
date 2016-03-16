;;; mor.el --- Mode on region -*- lexical-binding: t -*-


;;; Commentary:
;;;
;;; Use function `mor-emacs-lisp-mode-on-region' to copy a highlighted region
;;; to a new buffer and turn on a mode.
;;;
;;; Copy the text back with `mor-copy-back'
;;;
;;; NOTE: lexical binding is used as a potential micro-optimization for
;;; variable lookups.  This package *should* work whether lexical or dynamic
;;; binding is used.


;;; Installation:
;;; Place `mor.el' in folder `/your/choosen/folder'.
;;; Add the following text to your .emacs or init.el file:
;;;
;;;   (add-to-list 'load-path "/your/choosen/folder")
;;;   (autoload #'mor-mode-on-region "mor" nil t)
;;;   (autoload #'mor-emacs-lisp-mode-on-region "mor" nil t)
;;;   ;; configure
;;;   (setq mor-format-automatically-p t)


;;; Code:
(require 'cl-lib)
(require 'rx)


(defvar mor-format-automatically-p nil
  "When t automatically format the copied text via `indent-region'.")

(defvar mor-switch-buff-fn #'switch-to-buffer-other-window
  "Function used to switch to the tmp buffer (and back again).
Choices: `switch-to-buffer-other-window' or `switch-to-buffer'")



(defconst mor--prefix "mor-tmp-"
  "Prefix used for temp buffer names.")
(defvar mor--counter 0
  "Sequential counter used to generate unique names for temp buffers.")


(defvar-local mor--orig-buffer nil
  "The original buffer you highted some text in.
Used in tmp buffer to transfer the modified text back to the original buffer.")
(defvar-local mor--start nil
  "Start of region.
Used in tmp buffer to transfer the modified text back to the original buffer.")
(defvar-local mor--end nil
  "End of region.
Used in tmp buffer to transfer the modified text back to the original buffer.")


(defun mor--gen-buffer-name ()
  "Generate a unique buffer name."
  (prog1
      (concat mor--prefix
              (buffer-name (current-buffer))
              "-"
              (int-to-string mor--counter))
    (cl-incf mor--counter)))

(defun mor--starts-with-p (string prefix)
  "Return t if STRING begins with PREFIX."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(defun mor-kill-tmp-buffers ()
  "Delete the junk tmp buffers."
  (interactive)
  (dolist (b (buffer-list))
    (when (mor--starts-with-p (buffer-name b) mor--prefix)
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
  (let ((orig-buff (current-buffer))
        (tmp-buff (mor--gen-buffer-name)))


    (kill-ring-save start end) ;; copy higlihgted text
    (deactivate-mark)

    (funcall mor-switch-buff-fn tmp-buff)
    (yank)              ;; paste text
    (funcall mode-fn)   ;; interactively select mode, turn it on.
    (with-current-buffer tmp-buff
      ;; NOTE: these buffer-local vars must be set AFTER `mode-fn' is
      ;; called. Becuase major modes wipe buffer local vars.
      (setq mor--orig-buffer orig-buff
            mor--start start
            mor--end end))
    (when mor-format-automatically-p
      (mark-whole-buffer)
      ;; interactive gets region autmoatically
      (call-interactively #'indent-region))))

;; TODO: find a way to make the existence of this function buffer-local so I
;; don't need a guarding check.
(defun mor-copy-back ()
  "Copy the temp buffer text back the original buffer.

WARNING:
Overwrites the original text.
May not work correclty if original buffer has been modified since the tmp
buffer was created.  If in doubt, just manually copy the text back.

TODO: Use a more full-proof technqiue to identity the start/end location to
overwrite."
  (interactive)
  (if (null mor--orig-buffer) ; guard
      (message "You must be in a mor-tmp buffer for this to work.")
    (progn
      (let ((tmp-buff (current-buffer)))
        (kill-ring-save (point-min) (point-max)) ;; mode buffer text.
        (mor--copy-back-orig mor--start
                             mor--end
                             mor--orig-buffer)
        ;; kill the tmp buffer becuase mulitple attempts to copy back text
        ;; will be wrong due to the static start/end location. Will need
        ;; to use a better way to track start/end before we can allow the
        ;; tmp buffer to live longer for mulitple copies.
        (kill-buffer tmp-buff)))))

(defun mor--copy-back-orig (start end orig-buff)
  "Copy the tmp buffer text back to the original buffer.
START of the region.
END of the region.
ORIG-BUFF to copy to."
  ;; NOTE: start, end, and orig-buff must be parameters becuase the buffer
  ;; local values don't exist in the original buffer (which we are now in).
  (funcall mor-switch-buff-fn orig-buff)
  ;; highlight the region to overwrite
  (goto-char start)
  (delete-char (- end start))
  ;; paste new text, overwriting old text.
  (yank))

(provide 'mor)

;;; mor.el ends here
