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
    "Create a new output buffer with a unique name."
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
                      (or (fugitive-str-starts-with-p cmd "git diff")
                          (fugitive-str-starts-with-p cmd "git show")))))

    ;; run command
    (shell-command cmd buff)
    ;; show output
    (switch-to-buffer-other-window buff)

    ;; turn on a specialized mode for the output type
    (with-current-buffer buff
      (cond (log-p
             ;; (log-view-mode) ; TODO: fix. doesn't work right.
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
  "Find branches without a remote tracking branch by grepping remote data.
Flawed implementation:
   Doesn't handle multiple remotes.
   Assumes alias name origin.
   Depends on grep. It may not be available on windows."
  (interactive)
  ;; this command works on mac. maybe not windows
  ;; { git branch -vv | grep -v origin & git branch -vv | grep ": gone]"; }
  (fugitive-shell-command "{ git branch -vv | grep -v origin & git branch -vv | grep \": gone]\"; }"
                          (get-buffer-create fugitive-buff-name)))

;;;###autoload
(cl-defun fugitive-log-between (&optional rev1 rev2)
  (interactive)
  ;; get rev1, rev2 from user if needed
  (when (or (null rev1) (null rev2))
    (let ((revs (fugitive-get-branches-and-tags)))
      (push "HEAD" revs) ;; TODO: look into this
      (when (null rev1)
        (setq rev1 (completing-read "rev1: " revs nil nil)))
      (when (null rev2)
        (setq rev2 (completing-read "rev2: " revs nil nil)))))
  ;; GUARD: return early if user failed to supply rev1 or rev2
  (when (or (null rev1) (null rev2)
            (string-equal rev1 "")
            (string-equal rev2 ""))
    (message "rev1 and rev2 are required.")
    (cl-return-from fugitive-log-between))
  ;; run command
  (fugitive-shell-command (format "git log %s..%s" rev1 rev2)))

;;;###autoload
(cl-defun fugitive-diff-between (&optional rev1 rev2)
  (interactive)
  ;; get rev1, rev2 from user if needed
  (when (or (null rev1) (null rev2))
    (let ((revs (fugitive-get-branches-and-tags)))
      (push "HEAD" revs) ;; TODO: look into this
      (when (null rev1)
        (setq rev1 (completing-read "rev1: " revs nil nil)))
      (when (null rev2)
        (setq rev2 (completing-read "rev2: " revs nil nil)))))
  ;; GUARD: return early if user failed to supply rev1 or rev2
  (when (or (null rev1) (null rev2)
            (string-equal rev1 "")
            (string-equal rev2 ""))
    (message "rev1 and rev2 are required.")
    (cl-return-from fugitive-diff-between))
  ;; run command
  (fugitive-shell-command (format "git diff %s %s" rev1 rev2)))


(defun fugitive-cmd-to-list (cmd)
  "Run a git command which returns a string list as output.
Convert the string-list to an elisp list."
  (let* ((str (shell-command-to-string cmd))
         (trimmed (string-trim-right str))
         ;; apostrophe's added on windows?
         (cleaned (string-replace "'" "" trimmed))
         (lst (string-split cleaned "\n")))
    lst))

(defun fugitive-get-remote-aliases ()
  "Return a list of remote alias names."
  (fugitive-cmd-to-list "git remote"))

(defun fugitive-get-branches-and-tags ()
  "Return a list of branches."
  (fugitive-cmd-to-list "git for-each-ref --format='%(refname:short)' refs/"))

(defun fugitive-get-branches ()
  "Return a list of branches."
  (fugitive-cmd-to-list "git for-each-ref --format='%(refname:short)' refs/heads/ refs/remotes/"))

(defun fugitive-get-tags ()
  "Return a list of tags."
  (fugitive-cmd-to-list "git for-each-ref --format='%(refname:short)' refs/tags/"))



;; ;; test
;; (let* ((default-directory "~/.emacs.d/notElpaYolo/magit"))
;;   (fugitive-get-branches))

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