;;; fugitive.el --- Clone of vim fugitive -*- lexical-binding: t -*-

;;; License: GPL version 3
;;; Package-Requires: ((emacs "24.4"))
;;; Version: 0.0.0
;;; URL: todo

;;; Commentary:

;;; Installation:

;;; Code:
(require 'vc)
(require 'cl-lib)

(defvar fugitive-buff-name "*fugitive*")

(defun fugitive-str-starts-with-p (string prefix)
  "Return t if STRING begins with PREFIX."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))


(let ((seq 0))
  (defun fugitive-new-output-buffer ()
    (setq seq (+ 1 seq))
    (get-buffer-create (concat "*fugitive-"
                               (int-to-string seq)
                               "*"))))


(defun fugitive-delete-buffers ()
  "Delete fugitive related buffers."
  (interactive)
  (cl-loop for b in (buffer-list)
           do
           (when (fugitive-str-starts-with-p (buffer-name b) "*fugitive-")
             (kill-buffer b))))


;;;###autoload
(defun fugitive-shell-command (&optional cmd buff)
  (interactive)
  (when (null cmd)
    ;; read-shell-command supports command line completion
    (setq cmd (read-shell-command "cmd: " "git "))
    ;; (setq cmd (read-string "cmd: " "git "))
    )

  (let* ((buff (or buff (fugitive-new-output-buffer)))
         ;; shadow var to prevent mini buffer display
         (max-mini-window-height 0)
         (log-p (fugitive-str-starts-with-p cmd "git log"))
         (diff-p (and (not log-p)
                      (fugitive-str-starts-with-p cmd "git diff"))))

    ;; run command
    (shell-command cmd buff)
    ;; show output
    (switch-to-buffer-other-window buff)

    ;; turn on a specialized mode for the output type
    (with-current-buffer buff
      (cond (log-p (log-view-mode) ; TODO: fix. doesn't work right.
                   ;; (vc-git-log-view-mode)
                   )
            (diff-p (diff-mode))))

    buff ; return output buffer
    ))

;;;###autoload
(defun fugitive-find-local-only-branches-ediff ()
  "Use an ediff session to help find branches without a remote tracking branch."
  (interactive)
  (let ((buff-local (fugitive-shell-command "git branch"))
        (buff-remote (fugitive-shell-command "git branch -r")))
    (ediff-buffers buff-local buff-remote)))

;;;###autoload
(defun fugitive-find-local-only-branches-direct ()
  "Use an ediff session to help find branches without a remote tracking branch."
  (interactive)
  ;; this command works on mac. maybe not windows
  ;; { git branch -vv | grep -v origin & git branch -vv | grep ": gone]"; }
  (fugitive-shell-command "{ git branch -vv | grep -v origin & git branch -vv | grep \": gone]\"; }"
                          (get-buffer-create fugitive-buff-name)))



;; (defun fugitive-log ()
;;   "View git log output in an Emacs buffer."
;;   (interactive)
;;   (fugitive-shell-command "git log -n 3"
;;                           (get-buffer-create fugitive-buff-name)))

;; (defun fugitive-branch ()
;;   (interactive)
;;   (fugitive-shell-command "git branch -r"
;;                           (get-buffer-create fugitive-buff-name)))


(provide 'fugitive)

;;; fugitive.el ends here