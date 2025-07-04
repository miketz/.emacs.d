;;; fugitive.el --- Clone of vim fugitive -*- lexical-binding: t -*-

;;; License: GPL version 3
;;; Package-Requires: ((emacs "24.4") (xterm-color "2.0"))
;;; Version: 0.0.0
;;; URL: todo

;;; Commentary:

;;; Installation:


;;; TODO:
;;; -- log performance seems worse since moving to async style code. look into
;;; it.

;;; Code:
(require 'cl-lib)
(require 'xterm-color)
(require 'thingatpt)
(require 'xref) ; for `xref-pulse-momentarily'

(defcustom fugitive-turn-on-diff-mode-p t
  "When t, turn on `diff-mode' for git diff/show commands.
You may want to configure this to nil if you already have diff coloring from
git-delta or other git output modifiers.")

(defcustom fugitive-auto-inject-color-flag t
  "If t, inject --color flag to some git commands.
Just logs for now.")

(defcustom fugitive-colorize-buffer-p t
  "If t, colorize the buffer via `xterm-color-colorize-buffer'.")

(defcustom fugitive-auto-inject-n-log-limit 1000
  "Integer for auto inection of -n NUM to git log commands.
Nil for no injection.

Needed because paging is not used with `shell-commmand'.
Large --graph logs can crash Emacs.
Or large logs can just be slow and you typically only need recent logs.")

(defcustom fugitive-auto-jump-to-first-parent t
  "When t auto jump the the first parent.
When nil allow the user to select a parent via `completing-read'.")

(defcustom fugitive-warn-quick-commit-p t
  "When t warn about the dangers of `fugitive-quick-commit'.
Prompt user before proceeding.")



(defcustom fugitive-juggle-home-env-var-p nil
  "When t set the environment var HOME to `fugitive-home-env-var' for the duration of a call to `fugitive-shell-command'.
Useful on Windows where you want HOME to be C:/Users/Username/ rather than C:/Users/Username/AppData/Roaming/ so permissions work right.")

;; (defvar fugitive-home-bak nil
;;   "Backup of the current value of environment variable HOME.
;; Used in conjection with `fugitive-juggle-home-env-var-p'.")

;; (defvar fugitive-home nil
;;   "Value to use for environment variable HOME that allows git settings to be found.
;; Used in conjection with `fugitive-juggle-home-env-var-p'.")

(defvar fugitive-home-env-var (if (eq system-type 'windows-nt)
                                  (concat "HOME=C:/Users/" (user-login-name) "/")
                                nil)
  "Environment variable HOME string that allows git settings to be found.
On Windows you may want HOME=C:/Users/Username/")




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

(defun fugitive-delete-buffers-except-current ()
  "Like `fugitive-delete-buffers' but skip active visited fugitive buffer."
  (interactive)
  (let ((del-cnt 0)
        (curr-buff (current-buffer)))
    (cl-loop for b in (buffer-list)
             do
             (when (and (not (eq curr-buff b))
                        (fugitive-str-starts-with-p (buffer-name b) "*fugitive-"))
               (kill-buffer b)
               (cl-incf del-cnt))
             finally
             (message "%d fugitive buffer%s deleted."
                      del-cnt
                      (if (or (>= del-cnt 2)
                              (= del-cnt 0))
                          "s" "")))))


;;;###autoload
(defun fugitive-shell-command (&optional cmd buff force-read-p hide-output-p)
  "Run a git command.
Display output in an Emacs buffer.
Attempt to detect output type: log, diff, etc.
Possibly turn on a mode or colorize buffer depending on output type.

CMD is the command. Intended to be a git command but it doesn't have to be.
If nil, the user will be prompted for a command.

BUFF is the buffer to display output in. A new buffer is automatcially created
if nil.

FORCE-READ-P will delay execution of the git command and allow the user to
edit/supply it, even if cmd has a value.

HIDE-OUTPUT-P will avoid popping up the output buffer BUFF. Useful for quick
rapid fire commands like `fugitive-quick-commit'."
  (interactive)

  (let ((process-environment (if fugitive-juggle-home-env-var-p
                                 (cons fugitive-home-env-var process-environment)
                               ;; else just use process-environment as-is
                               process-environment)))

    ;; (when fugitive-juggle-home-env-var-p
    ;;   (setenv "HOME" fugitive-home))

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
      (let (;; setting process-connection-type to nil avoids a "hang" on macOS
            ;; see https://www.reddit.com/r/emacs/comments/17wklf7/how_do_i_speed_up_output_from_an/?rdt=58905
            (process-connection-type nil)
            ;; TODO: look into the below 2 settings for possible improved performance
            (read-process-output-max 4194304) ;(* 4 1024 1024)
            (process-adaptive-read-buffering nil)
            (cmd-complete-fn (lambda (p msg)
                               ;; GUARD
                               (when (memq (process-status p) '(exit signal))
                                 ;;(message (concat (process-name p) " - " msg))
                                 (let ((buff (process-buffer p)))
                                   (with-current-buffer buff
                                     ;; (print `(:log-p ,log-p :diff-p ,diff-p :blame-p ,blame-p :buffer-name ,(buffer-name buff)))

                                     ;; alwyas call (xterm-color-colorize-buffer) now that i'm using the process sentinel more commands
                                     ;; seem to have speical colors, and diff is using git-delta colors!
                                     ;; (xterm-color-colorize-buffer)
                                     ;; TURN on a specialized mode for specific output types
                                     (cond (log-p
                                            ;; turn on this first before buffer becomes read-only via fugitive-log-mode
                                            (when fugitive-colorize-buffer-p
                                              (xterm-color-colorize-buffer))
                                            (fugitive-log-mode) ; mode tailored for logs
                                            ;; log outputtype is useful so `fugitive-hash' can correctly search for the commit hash on current line.
                                            (setq-local fugitive-log-type (fugitive-guess-log-output-type cmd))
                                            ;; (log-view-mode) ; TODO: fix. doesn't work right.
                                            ;; (vc-git-log-view-mode)

                                            ;; turn off word wrap
                                            (unless truncate-lines
                                              (toggle-truncate-lines)))

                                           (diff-p (when fugitive-turn-on-diff-mode-p
                                                     (diff-mode))
                                                   ;; turn on colors *after* diff mode or it doesnt' work right with git-delta colors
                                                   (when fugitive-colorize-buffer-p
                                                     (xterm-color-colorize-buffer)))
                                           (blame-p (when fugitive-colorize-buffer-p
                                                      (xterm-color-colorize-buffer))
                                                    ;; turn off word wrap
                                                    (unless truncate-lines
                                                      (toggle-truncate-lines))))

                                     ;; disable native line numbers.
                                     ;; NOTE: must set this AFTER any major modes like `fugitive-log-mode' are turned on as
                                     ;; major modes wipe out buffer local vars.
                                     (setq display-line-numbers nil))

                                   ;; show output
                                   (unless hide-output-p
                                     (display-buffer buff))

                                   (when (and (boundp 'evil-mode) evil-mode)
                                     ;; When using evil-mode and emacs 30+ the cursor becomes a bar | even when the buffer is in normal mode.
                                     ;; Switching to the buffer and calling (redisplay t) fixes it but is slow. evil-refresh-cursor is faster.
                                     (with-current-buffer buff
                                       (evil-refresh-cursor)))

                                   ;; (goto-char (point-max)) ;; end of buffer
                                   ;; (insert output-str) ;; this is done already by `start-process-shell-command'.
                                   ;; don't message complete for now. it wipes out command output
                                   ;; (message "cmd complete")

                                   ;; (when fugitive-juggle-home-env-var-p
                                   ;;   ;; TODO: handle case where git command hangs and never completes, thus never rolling back the HOME env var.
                                   ;;   ;;       for example git commit currenlty doesn't work as the editor is never accessed and forever hangs.
                                   ;;   (setenv "HOME" fugitive-home-bak))

                                   ;; return otuput buffer
                                   buff)))))
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
      buff)))

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

;; Overall i prefer `vc-next-action' over `fugitive-quick-commit'. Becuase vc is fast
;; enough and gives more time to review the diff, write a commit message, abort, etc.
;; But this quick fn is good as an expiriment for truly rapid fire commits.
;;;###autoload
(cl-defun fugitive-quick-commit ()
  "Save, stage, and commit the current buffer/file.

This is similar in spirit to `vc-next-action' in that it is focused on the current file.
But this fn does not assume the goal, it has 1 goal: make a save point commit for
the current file.

The commit message defaults to WIP (work in progress) to emphasize the quick save.

WARNING: If the staging area has changes those will be commited too!

It is reccomended to only call this fn while working in a feature branch where you
will squash away all the junk commits later. Or in your personal files where you
don't care about a narrative history and just want to make roll back points."
  (interactive)

  ;; GUARD: warn user of potential issues of rapid fire quick commits
  (when fugitive-warn-quick-commit-p
    (if (yes-or-no-p "WARNING: Ensure your staging area is empty as this command will commit
all staged changes.

It is reccomended to only run this command in feature branches where you plan to squash junk
commits later. Or in your personal files where you don't care about a narrative history and
just want to generate save points.

Proceed?")
        ;; avoid subsequent warning prompts after user confirms.
        (setq fugitive-warn-quick-commit-p nil)
      ;; else abort
      (cl-return-from fugitive-quick-commit)))

  ;; save the current buffer
  (basic-save-buffer)
  (let ((buff (fugitive-new-output-buffer))
        (filename (fugitive-curr-filename)))
    ;; stage. not async as we need this to complete before proceeding.
    (shell-command (concat "git add " filename)
                   buff)
    ;; commit
    (fugitive-shell-command (concat "git commit -m \"WIP: " filename "\"")
                            buff nil t)))

;;; TODO: make this fn more general. with prefix arg allow user to
;;;       choose branch other than current and remote other than default
;;;       to fetch and log against.
;;;###autoload
(cl-defun fugitive-fetch-n-log ()
  "Fetch from default remote.
Then show a delta log between current local branch..remote/branch. "
  (interactive)
  (let ((buff (fugitive-new-output-buffer)))
    ;; fetch. not async as we need this to complete before proceeding.
    (shell-command "git fetch" buff)
    ;; show delta log. branch..remote/branch
    (let* ((curr-branch (fugitive-get-curr-branch-str))
           ;; git config branch.<name>.remote
           (remote (fugitive-get-remote-for-branch curr-branch)))

      ;; GUARD: can't delta curr branch if it's a local only brnach (ie no remote)
      (when (string-equal remote "")
        (message "No corresponding remote branch for %s. Abort delta log."
                 curr-branch)
        (cl-return-from fugitive-fetch-n-log))

      ;; delta log. branch..remote/branch
      (fugitive-log-between curr-branch
                            (concat remote "/" curr-branch)
                            buff))))

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
(defun fugitive-curr-branch ()
  "Get the current branch"
  (interactive)
  (fugitive-shell-command "git rev-parse --abbrev-ref HEAD"))

;;;###autoload
(defun fugitive-select-branch-or-tag ()
  "Select a branch or tag via completing read.
Use curr branch as the initial input."
  (interactive)
  (let ((curr-branch (string-trim-right
                      (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
        (all-revs (fugitive-get-branches-and-tags)))
    (completing-read "rev: " all-revs nil nil curr-branch)))

;;;###autoload
(defun fugitive-log-fast ()
  "Prepare the git log command with common options for speed.
A normal log with no --graph or --first-parent.
It should finsih relatively quickly even for larger logs."
  (interactive)
  (fugitive-shell-command "git log --oneline --decorate=short -n 1000 " nil t))


;;;###autoload
(defun fugitive-log-faster ()
  "Like `fugitive-log-fast' but also disable colorization for even more speed."
  (interactive)
  (let ((fugitive-auto-inject-color-flag nil)
        (fugitive-colorize-buffer-p nil))
    (fugitive-shell-command "git log --oneline --decorate=short -n 1000 " nil t)))

;;;###autoload
(defun fugitive-log-first-parent ()
  "Prepare the git command with common options for a first-parent log.
This might be a bit faster than --graph logs as it doesn't need to render branches
as they are collapsed under the parent merge commit.
But likely still slower than normal logs with no --graph or --first-parent."
  (interactive)
  (fugitive-shell-command "git log --oneline --decorate=short --first-parent -n 1000 " nil t))

;;;###autoload
(defun fugitive-log-first-parent-no-color ()
  "Like `fugitive-log-first-parent' but also disable colorization for even more speed."
  (interactive)
  (let ((fugitive-auto-inject-color-flag nil)
        (fugitive-colorize-buffer-p nil))
   (fugitive-shell-command "git log --oneline --decorate=short --first-parent -n 1000 " nil t)))


;;;###autoload
(defun fugitive-log-file ()
  "Prepare the git command with for a log of a single file.
Uses the file of the current buffer."
  (interactive)
  (fugitive-shell-command (concat "git log --oneline -n 1000 --pretty=format:\"%C(auto)%h %ad %C(cyan)%an%C(auto)%d %s\" " fugitive-date-format " -- " (fugitive-curr-filename))
                          ;; (concat "git log --oneline --decorate=short -n 1000 -- " (fugitive-curr-filename))
                          nil t))

;;;###autoload
(defun fugitive-log-graph-compact ()
  "Prepare the git command with common options for graph view."
  (interactive)
  (fugitive-shell-command "git log --oneline --decorate=short --graph -n 1000 " nil t))


;; TODO: look into why medium and long graph logs don't colorize the hash.

;;;###autoload
(defun fugitive-log-graph-medium ()
  "Prepare the git command with common options for graph view."
  (interactive)
  (fugitive-shell-command "git log --graph -n 1000 --pretty=format:\"%C(auto)%h %C(cyan)%an%C(auto)%d %s\" " nil t))

;; just documenting some possible date formats.
;; "--date=short"
;; "--date=format:\"%-Y-%-m-%d\""
;; "--date=format:\"%-m-%-d-%Y %I:%M%p\""
;; "--date=format:\"%-Y-%-m-%d %I:%M%p\""


;; formats used for the --date format in git log.
;; powered by strftime and OS specific which flags are supported.
;; TODO: verify darwin format works for all other emacs supported OS systems.
;; %y = 2 digit year. good for reducing width of the log output.
;; %_H = 24 hour clock, space padded single digit hour
;; %p = am/pm in the date-format
(defvar fugitive-date-formats
  '((windows-nt . "--date=format:\"%y%m%d %H:%M%z\"") ;"--date=format:\"%Y%m%d %I:%M%p%z\"" ;"--date=short"
    (darwin .     "--date=format:\"%-y%m%d %_H:%m%z\""))) ;"--date=format:\"%-y-%-m-%d %I:%m%p\""


(defun fugitive-get-date-format-for-os ()
  "Get the date format to use based on OS."
  (let ((format (cdr (assoc system-type fugitive-date-formats))))
    (when (null format)
      ;; default to darwin format
      (setq format (cdr (assoc 'darwin fugitive-date-formats))))
    format))

;; the date format to use.
(defvar fugitive-date-format (fugitive-get-date-format-for-os))

;;;###autoload
(defun fugitive-log-graph-long ()
  "Prepare the git command with common options for graph view."
  (interactive)
  ;; (fugitive-shell-command "git log --oneline --graph -n 1000 " nil t)

  ;; format output to include author, date/time.
  ;; git log --pretty=format:"%h%x09%an%x09%ad%x09%s" --graph --date=format:"%-m-%-d-%Y %-I:%M%p"
  ;; %C(auto) = turns color back on. can also do somehting like C(red) to flip color from that point onwards.
  ;; %h = abbreviated commit hash
  ;; %x09 = tab (character for code 9)
  ;; %an = author name
  ;; %ad = author date (format respects --date= option)
  ;; %d = tag name, branch name, HEAD, etc.
  ;; %s = subject
  ;; From kernel.org/pub/software/scm/git/docs/git-log.html (PRETTY FORMATS section)
  (let* ((cmd (concat "git log --graph -n 1000 --pretty=format:\"%C(auto)%h %ad %C(cyan)%an%C(auto)%d %s\" " fugitive-date-format " ")))
                   ;; "git log --graph -n 1000 --pretty=format:\"%C(auto)%h %ad %C(cyan)%an%C(#90ee90)%d%C(reset) %s\"
    (fugitive-shell-command cmd nil t)))


(defvar-local fugitive-log-type nil
  "Buffer local in git log output buffers.
Calculated by `fugitive-guess-log-output-type'.
Used by `fugitive-hash' to assist in finding the commit hash on curr line.

The log output types are:

`normal' via: git log.
Commit hashes are prefixed by \"^commit \".

`normal-one-line' via: git log --oneline
Commit hashes are are the first item on each line, no prefix.

`graph' via: git log --graph
Commit hashes are prefixed by \"*.+commit \".

`graph-one-line' via: git log --graph --oneline
Commit hashes are prefixed by a star, wild card range, then the hash (no commit text).
  \"*.+ \
")

(defvar fugitive-log-graph-fn #'fugitive-log-graph-long
  "Default fn to use for graph in my hydra.")

;;;###autoload
(defun fugitive-log-graph ()
  "Prepare the git command with common options for graph view.
Use the default fn configured in `fugitive-log-graph-fn'."
  (interactive)
  (funcall fugitive-log-graph-fn))



;;;###autoload
(cl-defun fugitive-log-between (&optional rev1 rev2 buff)
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
  (fugitive-shell-command (format "git log %s..%s" rev1 rev2) buff t))

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

;;;###autoload
(cl-defun fugitive-list-files (&optional rev1 rev2)
  "Show files modified in a commit range."
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
    (cl-return-from fugitive-list-files))
  ;; run command
  (fugitive-shell-command (format "git diff --name-only %s %s" rev1 rev2) nil t))


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

(defun fugitive-get-curr-branch-str ()
  "Return the currently checked out branch name.
Returns \"HEAD\" if in a detached HEAD state."
  (string-trim-right
   (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))
  ;; for git ver 2.22+. Will return "" instead of "HEAD" on detatched head state.
  ;; (string-trim-right
  ;;  (shell-command-to-string "git branch --show-current"))
  )

(defun fugitive-get-remote-for-branch (branch)
  "Return the configured remote for BRANCH.
Returns emtpy string if no remote (ie local only branch)."
  (string-trim-right
   (shell-command-to-string
    (format "git config branch.%s.remote"
            branch))))

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
(cl-defun fugitive-parent-commits-jump-to (&optional commit)
  "Jump to the parent commit of the specified COMMIT.
You may want to call this fn while in a log buffer, with point on a commit hash."
  (interactive)
  (let ((commit (or commit
                    (fugitive-hash-or-next-line))))

    ;; GUARD: no commit hash found on current line
    (when (null commit)
      (cl-return-from fugitive-parent-commits-jump-to))

    (let* ((parents (fugitive-get-parent-commits-list commit))
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
        (message "Failed to find hash %s. If your log was filtered to single file or folder that may be the reason the parent commit is not listed in the log buffer." par-short)))))

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
(cl-defun fugitive-parent-commits (&optional commit)
  "Get the parent commit(s) of the specified COMMIT.
You may want to call this fn while in a log buffer, with point on a commit hash."
  (interactive)
  (let* ((commit (or commit
                     (fugitive-hash))))

    ;; GUARD: no commit hash found on current line
    (when (null commit)
      (message "commit hash is required.")
      (cl-return-from fugitive-parent-commits))

    ;; need double quotes around hash to avoid breaker on Windows.
    ;; it doesn't like ^@ characters?
    (let ((cmd (read-shell-command "cmd: " (format "git rev-parse \"%s^@\"" commit))))
      ;; TODO: doesn't work on windows. fix.
      ;; maybe issue with shell-command itself as the same cmd from git bash works
      (fugitive-shell-command cmd))))
;; first parent: git rev-parse commit^
;; nth parent: git rev-parse commit^1
;; all parents: git rev-parse commit^@


;;;###autoload
(defun fugitive-show (&optional commit)
  "Show the specified COMMIT.
You may want to call this fn while in a log buffer, with point on a commit hash."
  (interactive)
  (let ((commit (or commit
                    (fugitive-hash-or-next-line)))
        ;; (cmd (read-shell-command "cmd: " (format "git show %s" commit)))
        ;; (cmd (format "git show %s" commit))
        )
    (when (not (null commit))
      (if (fugitive-merge-commit-p commit)
          ;; show output tailored for "merge" commit
          (fugitive-show-merge-commit commit)
        ;; else, normal show
        (fugitive-shell-command (format "git show %s" commit))))))

(defun fugitive-show-merge-commit (&optional commit)
  "Like `fugitive-show', but tailor output for merge commits.
Inlcude affected files, no diffs."
  (interactive)
  (let ((commit (or commit
                    (fugitive-hash-or-next-line))))
    (when (not (null commit))
      (fugitive-shell-command (format "git show %s -m --name-only" commit)))))

(defun fugitive-merge-commit-p (commit)
  "Return t if commit is a merge commit. ie has multiple parents."
  (let* ((parents (fugitive-get-parent-commits-list commit))
         (merge-p (> (length parents) 1))) ; if more than 1 parent
    merge-p))

;; TODO: verify this works as expected when git-delta is not installed.
;;       I have git-delta installed which may affect the log output.
(defun fugitive-guess-log-output-type (cmd)
  "Attempt to guess the log output type of a git log CMD.
Once the log output type is known it's easier to search for the commit hash
via a regex.

There are a few types of log output:

`normal' via: git log.
Commit hashes are prefixed by \"^commit \".

`normal-one-line' via: git log --oneline
Commit hashes are are the first item on each line, no prefix.

`graph' via: git log --graph
Commit hashes are prefixed by \"*.+commit \".

`graph-one-line' via: git log --graph --oneline
Commit hashes are prefixed by a star, wild card range, then the hash (no commit text).
  \"*.+ \"
"
  (let* ((oneline-p (string-match "--oneline" cmd))
         (graph-p (string-match "--graph" cmd)))
    (cond ((and oneline-p graph-p) 'graph-one-line)
          (graph-p 'graph)
          (oneline-p 'normal-one-line)
          (t 'normal))))

;; TODO: clean up the logic/flow. it's a bit convoluted and grew from trial/error
(cl-defun fugitive-hash ()
  "In a log buffer, search for commit hash on current line, return hash.
If no hash found return nil."
  (interactive)
  (save-excursion
    (let* ((case-fold-search t) ; case insensitive search
           (line-end (progn
                       (move-end-of-line 1)
                       (point)))
           (line-start (move-beginning-of-line 1))
           ;; handle both graph and traditioanl log formats
           (found-log-header-p (or (re-search-forward "commit"
                                                   (+ line-start (length "commit"))
                                                   t ; don't error on no match
                                                   )
                                   (re-search-forward "\* "
                                                   line-end
                                                   t ; don't error on no match
                                                   )
                                   ))
           ;; assumes hash is the first thing after the star "*" or "commit"
           (found-hash-p (re-search-forward "[0-9a-fA-F]+"
                                            line-end
                                            t ; don't error on no match
                                            )))

      ;; special handling for `normal-one-line' log outputs
      (when (eq fugitive-log-type 'normal-one-line)
        ;; hash is very first thing on line, no prefix.
        (move-beginning-of-line 1)
        (let ((found-hash-p (re-search-forward "[0-9a-fA-F]+"
                                               (+ line-start 7) ; 7 digit hash minimum
                                               t ; don't error on no match
                                               )))
          (if found-hash-p
              (progn
                (backward-word)
                (cl-return-from fugitive-hash (thing-at-point 'symbol 'no-properties)))
            ;; else
            (cl-return-from fugitive-hash nil))))

      (unless found-log-header-p
        (cl-return-from fugitive-hash nil))
      (if found-hash-p
          (progn
            (backward-word)
            (thing-at-point 'symbol 'no-properties))
        ;; else
        nil))))

(defun fugitive-hash-or-next-line ()
  "Same as `fugitive-hash'.
But if no hash found on current line, goto `next-line' as a side effect."
  (interactive)
  (let ((hash (fugitive-hash)))
    (when (null hash)
      (next-line))
    hash))



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