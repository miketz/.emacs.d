;;; fugitive.el --- Clone of vim fugitive -*- lexical-binding: t -*-

;;; License: GPL version 3
;;; Package-Requires: ((emacs "24.4") (xterm-color "2.0"))
;;; Version: 0.0.0
;;; URL: todo

;;; Commentary:

;;; Installation:

;;; Code:
(require 'cl-lib)
(require 'xterm-color)
(require 'thingatpt)
(require 'xref) ; for `xref-pulse-momentarily'

(defcustom fugitive-auto-inject-color-flag t
  "If t, inject --color flag to some git commands.
Just logs for now.")

;; making this limit quite large by default. Should feel like there is no limit
;; for typical use, but small enough to prevent Emacs from crashing.
(defcustom fugitive-auto-inject-n-log-limit 8000
  "Integer for auto inection of -n NUM to git log commands.
Nil for no injection.

Needed because paging is not used with `shell-commmand'.
Large --graph logs can crash Emacs.")

(defcustom fugitive-auto-jump-to-first-parent t
  "When t auto jump the the first parent.
When nil allow the user to select a parent via `completing-read'.")


(defvar fugitive-buff-name "*fugitive*")

(defun fugitive-str-starts-with-p (string prefix)
  "Return t if STRING begins with PREFIX."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))


(let ((seq 0)) ; private var. requires lexical binding
  (defun fugitive-new-output-buffer ()
    "Create a new output buffer with a unique name."
    (setq seq (+ 1 seq))
    (get-buffer-create (concat "*fugitive-"
                               (int-to-string seq)
                               "*"))))


(defun fugitive-delete-buffers ()
  "Delete fugitive related buffers."
  (interactive)
  (let ((del-cnt 0))
    (cl-loop for b in (buffer-list)
             do
             (when (fugitive-str-starts-with-p (buffer-name b) "*fugitive-")
               (kill-buffer b)
               (cl-incf del-cnt))
             finally
             (message "%d fugitive buffer%s deleted."
                      del-cnt
                      (if (or (>= del-cnt 2)
                              (= del-cnt 0))
                          "s" "")))))


;;;###autoload
(defun fugitive-shell-command (&optional cmd buff force-read-p)
  "Run a git command.
Display output in an Emacs buffer.
Attempt to detect output type: log, diff, etc.
Possibly turn on a mode or colorize buffer depending on output type.

CMD is the command. Intended to be a git command but it doesn't have to be.
If nil, the user will be prompted for a command.

BUFF is the buffer to display output in. A new buffer is automatcially created
if nil.

FORCE-READ-P will delay execution of the git command and allow the user to
edit/supply it, even if cmd has a value."
  (interactive)
  (when (or (null cmd) force-read-p)
    ;; read-shell-command supports command line completion
    (setq cmd (read-shell-command "cmd: " (or cmd "git ")))
    ;; (setq cmd (read-string "cmd: " "git "))
    )

  (let* ((buff (or buff ; buff passed in
                   (fugitive-new-output-buffer)))
         ;; shadow var to prevent mini buffer display
         (max-mini-window-height 0)
         (log-p (fugitive-str-starts-with-p cmd "git log"))
         (diff-p (and (not log-p)
                      (or (fugitive-str-starts-with-p cmd "git diff")
                          (fugitive-str-starts-with-p cmd "git show"))))
         (blame-p (and (not diff-p)
                       (fugitive-str-starts-with-p cmd "git blame"))))
    ;; inject --color flag for logs. For diffs, diff-mode does a good job with colors
    (when (and fugitive-auto-inject-color-flag
               log-p)
      ;; inject --color immediately after "git log"
      (let* ((i (length "git log")))
        (setq str (concat (substring-no-properties cmd 0 i)
                          " --color "
                          (substring-no-properties cmd i nil)))
        (setq cmd str)))

    ;; inject limit to # of logs returned. Emacs chokes on massive graph logs.
    (when (and log-p
               fugitive-auto-inject-n-log-limit
               ;; only inject if -n filter was not already supplied
               (null (string-search "-n" cmd)))
      ;; inject -n 4000 immediately after "git log"
      (let* ((i (length "git log")))
        (setq str (concat (substring-no-properties cmd 0 i)
                          " -n "
                          (int-to-string fugitive-auto-inject-n-log-limit)
                          " "
                          (substring-no-properties cmd i nil)))
        (setq cmd str)))



    ;; ;; run command
    ;; (shell-command cmd buff)

    ;; run cmd. use process to avoid freezing emacs.
    ;; make cmd-complete-fn a closure to capture variables: log-p, diff-p, blame-p
    (let ((cmd-complete-fn (lambda (p msg)
                             ;; GUARD
                             (unless (memq (process-status p) '(exit signal))
                               (cl-return-from fugitive--cmd-complete))

                             ;;(message (concat (process-name p) " - " msg))
                             (let ((buff (process-buffer p)))
                               (with-current-buffer buff
                                 ;; (print `(:log-p ,log-p :diff-p ,diff-p :blame-p ,blame-p))

                                 ;; alwyas call (xterm-color-colorize-buffer) now that i'm using the process sentinel more commands
                                 ;; seem to have speical colors, and diff is using git-delta colors!
                                 ;; (xterm-color-colorize-buffer)
                                 ;; TURN on a specialized mode for specific output types
                                 (cond (log-p
                                        ;; turn on this first before buffer becomes read-only via fugitive-log-mode
                                        (xterm-color-colorize-buffer)
                                        (fugitive-log-mode) ; mode tailored for logs
                                        ;; (log-view-mode) ; TODO: fix. doesn't work right.
                                        ;; (vc-git-log-view-mode)
                                        )
                                       (diff-p (diff-mode)
                                               ;; turn on colors *after* diff mode or it doesnt' work right with git-delta colors
                                               (xterm-color-colorize-buffer))
                                       (blame-p (xterm-color-colorize-buffer)
                                        )))

                               ;; show output
                               (display-buffer buff)

                               ;; (goto-char (point-max)) ;; end of buffer
                               ;; (insert output-str) ;; this is done already by `start-process-shell-command'.
                               ;; don't message complete for now. it wipes out command output
                               ;; (message "cmd complete")
                               ;; return otuput buffer
                               buff))))
     (set-process-sentinel (start-process-shell-command "fugitive-proc" buff cmd)
                           cmd-complete-fn))

    ;; escape % characters as `message' thinks they are message params!!!
    (message (string-replace "%" "%%" cmd)) ; echo final cmd actually run. may have --color injected.
    ;; show output
    ;; (display-buffer buff)
    ;; (switch-to-buffer-other-window buff)

    ;; (with-current-buffer buff
    ;;   ;; TURN on a specialized mode for specific output types
    ;;   (cond (log-p
    ;;          ;; turn on this first before buffer becomes read-only via fugitive-log-mode
    ;;          (xterm-color-colorize-buffer)
    ;;          (fugitive-log-mode) ; mode tailored for logs
    ;;          ;; (log-view-mode) ; TODO: fix. doesn't work right.
    ;;          ;; (vc-git-log-view-mode)
    ;;          )
    ;;         (diff-p (diff-mode))
    ;;         (blame-p (xterm-color-colorize-buffer))))

    ;; return output buffer. Likely nil due to the new async stuff. Must sleep if you
    ;; want to capture this. TODO: look into hwo to wait (ie WaitGroups) in the calling
    ;; code in elisp.
    buff
    ))

;;;###autoload
(defun fugitive-find-local-only-branches-ediff ()
  "Use an ediff session to help find branches without a remote tracking branch."
  (interactive)
  (let ((buff-local (fugitive-shell-command "git branch"))
        (buff-remote (fugitive-shell-command "git branch -r")))
    ;; now that fugitive-shell-command works in an aync way it's returning buff
    ;; before the cmd is complete! For now just sleep as a ham-fisted way to make
    ;; it work. Branch commands should complete within a fraction of a second for sure.
    (let ((duration (if (eq system-type 'windows-nt)
                        1.0 ; TODO: test to find out how much time windows needs.
                      0.2)))
      (sleep-for duration))
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

(defun fugitive-curr-filename ()
  "Get file name of current buffer.
Empty string if buffer does not visit a file."
  (let ((filename (buffer-file-name)))
    (if (null filename)
        ""
      ;; else
      (file-name-nondirectory filename))))

;;;###autoload
(defun fugitive-blame ()
  "Prepare the git command with common options for blame."
  (interactive)
  ;; git blame --color-lines --color-by-age -- init.el
  (let ((cmd (concat
              "git blame --color-lines --color-by-age -- "
              (fugitive-curr-filename))))
    (fugitive-shell-command cmd nil t)))

;;;###autoload
(defun fugitive-log-graph-compact ()
  "Prepare the git command with common options for graph view."
  (interactive)
  (fugitive-shell-command "git log --oneline --graph -n 2000 " nil t))


;; TODO: look into why medium and long graph logs don't colorize the hash.

;;;###autoload
(defun fugitive-log-graph-medium ()
  "Prepare the git command with common options for graph view."
  (interactive)
  (fugitive-shell-command "git log --graph -n 2000 --pretty=format:\"%h%x09%an%x09%s\" " nil t))

;;;###autoload
(defun fugitive-log-graph-long ()
  "Prepare the git command with common options for graph view."
  (interactive)
  ;; (fugitive-shell-command "git log --oneline --graph -n 2000 " nil t)

  ;; format output to include author, date/time.
  ;; git log --pretty=format:"%h%x09%an%x09%ad%x09%s" --graph --date=format:"%-m-%-d-%Y %-I:%M%p"
  ;; %h = abbreviated commit hash
  ;; %x09 = tab (character for code 9)
  ;; %an = author name
  ;; %ad = author date (format respects --date= option)
  ;; %s = subject
  ;; From kernel.org/pub/software/scm/git/docs/git-log.html (PRETTY FORMATS section)
  (let* ((date-arg (if (eq system-type 'windows-nt)
                       "--date=short"
                     ;; TODO: find a date format that works in windwos
                     "--date=format:\"%-m-%-d-%Y %I:%M%p\""))
         (cmd (concat "git log --graph -n 2000 --pretty=format:\"%h%x09%an%x09%ad%x09%s\" " date-arg " ")))
    (fugitive-shell-command cmd nil t)))


(defvar fugitive-log-graph-fn #'fugitive-log-graph-compact
  "Default fn to use for graph in my hydra.")

;;;###autoload
(defun fugitive-log-graph ()
  "Prepare the git command with common options for graph view.
Use the default fn configured in `fugitive-log-graph-fn'."
  (interactive)
  (funcall fugitive-log-graph-fn))



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
  (fugitive-shell-command (format "git log %s..%s" rev1 rev2) nil t))

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
  (fugitive-shell-command (format "git diff %s %s" rev1 rev2) nil t))


(defun fugitive-cmd-to-list (cmd)
  "Run a git command which returns a string list as output.
Convert the string-list to an elisp list."
  (let* ((str (shell-command-to-string cmd))
         (trimmed (string-trim-right str))
         ;; apostrophe's added on windows?
         (cleaned (string-replace "'" "" trimmed))
         (lst (string-split cleaned "\n")))
    ;; remove empty strings. the case when command produces no output.
    (cl-delete-if (lambda (str)
                    (string-equal str ""))
                  lst)))

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

(defun fugitive-get-parent-commits-list (commit)
  "Return a list of parent commit hashes for COMMIT."
  (interactive)
  ;; need double quotes around hash to avoid breaker on Windows.
  ;; it doesn't like ^@ characters?
  (fugitive-cmd-to-list (format "git rev-parse \"%s^@\"" commit)))



;;;###autoload
(defun fugitive-parent-commits-jump-to (&optional commit)
  "Jump to the parent commit of the specified COMMIT.
You may want to call this fn while in a log buffer, with point on a commit hash."
  (interactive)
  (let* ((commit (or commit
                     (thing-at-point 'symbol 'no-properties)))
         (parents (fugitive-get-parent-commits-list commit))
         ;; stop completing-read from sorting hashes.
         ;; from stack overflow post: https://emacs.stackexchange.com/questions/41801/how-to-stop-completing-read-ivy-completing-read-from-sorting
         (completion-table (lambda (string pred action)
                             (if (eq action 'metadata)
                                 '(metadata (display-sort-function . identity)
                                            (cycle-sort-function . identity))
                               (complete-with-action
                                action parents string pred))))
         ;; (par (completing-read "parent: " parents nil t))
         (par (if (or fugitive-auto-jump-to-first-parent
                      (= 1 (length parents)))
                  (car parents) ; skip completion
                (completing-read "parent: " completion-table nil t)))
         ;; some log outputs only show 7 chars of the hash. which would mess up
         ;; searching on the complete hash.
         ;; TODO: hash display length appears to be dynamic. someitmes 8 or 9. will break
         ;;       if 7. handle this.
         (par-short (substring-no-properties par 0 7))
         (found-p (re-search-forward par-short
                                     nil ; no bounds on search
                                     t ; do not trigger an error if no search match
                                     )))
    (if found-p
        (progn
          (backward-word) ; go to start of hash
          (xref-pulse-momentarily))
      ;; else, failure message
      (message "failed to find hash %s" par-short))))

;; (length "f2db9fa3f") 9

;;;###autoload
(defun fugitive-parent-commits-jump-to-first (&optional commit)
  "Jump to the parent commit of the specified COMMIT.
You may want to call this fn while in a log buffer, with point on a commit hash.

Same as `fugitive-parent-commits-jump-to' but always choose the first parent
regardless of config var `fugitive-auto-jump-to-first-parent'."
  (interactive)
  ;; shadow var to t.
  (let ((fugitive-auto-jump-to-first-parent t))
    (fugitive-parent-commits-jump-to commit)))


;;;###autoload
(defun fugitive-parent-commits (&optional commit)
  "Get the parent commit(s) of the specified COMMIT.
You may want to call this fn while in a log buffer, with point on a commit hash."
  (interactive)
  (let* ((commit (or commit
                     (thing-at-point 'symbol 'no-properties)))
         ;; need double quotes around hash to avoid breaker on Windows.
         ;; it doesn't like ^@ characters?
         (cmd (read-shell-command "cmd: " (format "git rev-parse \"%s^@\"" commit))))
    ;; TODO: doesn't work on windows. fix.
    ;; maybe issue with shell-command itself as the same cmd from git bash works
    (fugitive-shell-command cmd)))
;; first parent: git rev-parse commit^
;; nth parent: git rev-parse commit^1
;; all parents: git rev-parse commit^@


;;;###autoload
(defun fugitive-show (&optional commit)
  "Show the specified COMMIT.
You may want to call this fn while in a log buffer, with point on a commit hash."
  (interactive)
  (let* ((commit (or commit
                     (thing-at-point 'symbol 'no-properties)))
         ;; (cmd (read-shell-command "cmd: " (format "git show %s" commit)))
         (cmd (format "git show %s" commit)))
    ;; TODO: doesn't work on windows. fix.
    ;; maybe issue with shell-command itself as the same cmd from git bash works
    (fugitive-shell-command cmd)))

(defvar fugitive-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") #'fugitive-parent-commits-jump-to)
    map))

(define-derived-mode fugitive-log-mode special-mode "fugitive-log"
  "Mode for the log results buffer.
Mostly just to support key binds."
  :lighter " fugi-log"
  :keymap (let ((map (make-sparse-keymap)))
            ;; No default bindings for now. User will choose them.
            map)
  ;; (read-only-mode 1) ; inherit this from special-mode
  )


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