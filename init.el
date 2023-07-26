;;; init.el --- My emacs config. -*- lexical-binding: t -*-

;;; Commentary:

;;; NOTES:
;;; Windows: registry entry for "open with" option:
;;;   Computer\HKEY_CLASSES_ROOT\Applications\runemacs.exe\shell\open\command

;;;----------------------------------------------------------------------------
;;; Git incantations:
;;;----------------------------------------------------------------------------
;;; Create a new github repo from an existing local repo:
;;;     Go to github.com and set up a new *empty* repo. Use local folder name
;;;     ".emacs.d" for the repo name.
;;;     git remote add origin https://github.com/miketz/.emacs.d.git
;;;     git push -u origin master
;;;
;;; Download from github to a new computer:
;;;     git clone --recurse-submodules https://github.com/miketz/.emacs.d.git
;;;
;;; Get latest changes from github:
;;;     git pull origin master
;;;     git submodule update --init --recursive
;;; Alternatively, to avoid a manual git pull in each submodule folder?
;;;     git submodule update --remote --merge
;;;
;;; Push local changes up to github:
;;;     git push origin master
;;;
;;; Revert changes to modified files.
;;;     git reset --hard
;;;
;;; Remove all untracked files and directories.
;;;     git clean -fd
;;;
;;; Add a private local branch to github:
;;;     git push -u origin branchName

;;;----------------------------------------------------------------------------
;;; Git work flow with submodule:
;;;----------------------------------------------------------------------------
;;; With ~/.emacs.d/notElpa/swiper as an example
;;;
;;; Fork on github
;;;
;;; Create a submodule from fork
;;;     cd ~/.emacs.d/notElpa
;;;     git submodule add https://github.com/miketz/swiper
;;;
;;; Make changes
;;; Push to fork
;;;     cd ~/.emacs.d/notElpa/swiper
;;;     git push origin master
;;; Make pull request on github
;;;
;;; Download from upstream -> local. Then push local -> fork
;;;     cd ~/.emacs.d/notElpa/swiper
;;;     git remote add upstream https://github.com/abo-abo/swiper
;;;     git fetch upstream
;;;     # to see new changes.
;;;     git diff master upstream/master
;;;     # then: (like "git pull" which is fetch + merge)
;;;     git merge upstream/master master
;;;
;;;     # or, better, replay your local work on top of the fetched branch
;;;     # like a "git pull --rebase"
;;;     git rebase upstream/master
;;;
;;;     # push changes to remote fork
;;;     git push origin master

;;;----------------------------------------------------------------------------
;;; Initialize a git submodule
;;;----------------------------------------------------------------------------
;;; Clone recursively
;;;     git clone --recursive <URL-OF-REPOSITORY>
;;;
;;; Or initialize manually. (cd to the folder first???)
;;;     git submodule init
;;;     git submodule update
;;; Then after a git pull
;;;     git submodule update --init --recursive
;;; If submodules folders are *still* empty for some reason try this.
;;; Note that it may not check out the correct branch, so avoid if possible.
;;;     git submodule update --init --force --remote

;;;----------------------------------------------------------------------------
;;; remove, then re-add a submodule where the folder mysteriously disappears?
;;;----------------------------------------------------------------------------
;;; git rm --cached path_to_submodule (no trailing slash)
;;;
;;; rm -rf path_to_submodule
;;;
;;; Delete the relevant lines from the .gitmodules file. e.g. delete these:
;;; [submodule "path_to_submodule"]
;;;     path = path_to_submodule
;;;     url = https://github.com/path_to_submodule
;;;
;;; Delete the relevant section from .git/config. e.g. delete these:
;;; [submodule "path_to_submodule"]
;;;     url = https://github.com/path_to_submodule
;;;
;;; rm -rf .git/modules/path_to_submodule
;;;
;;; Then, you can finally:
;;; git submodule add https://github.com/path_to_submodule

;;;----------------------------------------------------------------------------
;;; Making a local branch, pushing it to remote server.
;;;----------------------------------------------------------------------------
;;; Create a new branch:
;;;     git checkout -b feature_branch_name
;;; Push your branch to the remote repository:
;;;     git push -u origin feature_branch_name
;;;
;;; To view both remote-tracking branches and local branches, run the command:
;;;     git branch -a
;;; If you already have a branch and you want to track a remote branch:
;;;     git branch --set-upstream-to origin/BRANCH

;;;----------------------------------------------------------------------------
;;; rename a remote
;;;----------------------------------------------------------------------------
;;; observe current remotes:
;;;     git remote -v
;;; rename the remote
;;;     git remote rename currentName newName

;;;----------------------------------------------------------------------------
;;; Change default remote to push/pull to.
;;;----------------------------------------------------------------------------
;;; within the submodule:
;;;     observe current remotes:
;;;         git remote -v
;;;     frist fetch latest from the remote you plan to swtich to
;;;         git fetch RemoteName
;;;     set new default:
;;;         git branch --set-upstream-to=RemoteName/BranchName BranchName

;;;----------------------------------------------------------------------------
;;; Change the remote a git submodule is wired up to.
;;;----------------------------------------------------------------------------
;;; case: initially used upstream repo, then later switch to fork
;;; Edit the .gitmodules file to update the URL
;;; Then run
;;;     git submodule sync --recursive
;;; to reflect that change to the superproject and your working copy.
;;; Then go to the .git/modules/path_to_submodule dir and change the config
;;; file to update git path.
;;; If repo history is different then you need to checkout new branch manually:
;;;     git submodule sync --recursive
;;;     cd <submodule_dir>
;;;     git fetch
;;;     git checkout origin/master
;;;     git branch master -f
;;;     git checkout master


;;;----------------------------------------------------------------------------
;;; To keep init files in Local instead of Roaming on ms-windows.
;;;----------------------------------------------------------------------------
;;; Create a "stub" init file at C:/Users/???/AppData/Roaming/.emacs.d/init.el
;;; put the follwing code in init.el:
;;; (setq user-init-file "C:/Users/???/AppData/Local/.emacs.d/init" ; no .el
;;;       user-emacs-directory "C:/Users/???/AppData/Local/.emacs.d/"
;;;       default-directory "c:/Users/???/")
;;; (setenv "HOME" "C:/Users/???/AppData/Local/") ; so ~ expands correctly
;;; (load user-init-file)

;;; Code:

(progn ;; JUMPrestore
  ;; tricks to improve startup time.
  ;; from https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_st
  ;; eps_to_speed_up_emacs_start/
  ;; TODO: verify exactly how much these help.

  (defvar gc-cons-threshold-backup gc-cons-threshold)
  (setq gc-cons-threshold (if (version< emacs-version "24.4")
                              200000000
                            2000000000))

  (defvar file-name-handler-alist-backup file-name-handler-alist)
  (setq file-name-handler-alist nil)

  ;; restore original values at end of init.el.
  ;; sort of like my own dynamic binding. I don't want to wrap the entire
  ;; config in a giant let.
  )

;; Turn off mouse interface early in startup to avoid momentary display
(when (version< emacs-version "27.0") ; in early-init.el for emacs 27+
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1)))



;;;----------------------------------------------------------------------------
;;; minimum emacs version
;;;----------------------------------------------------------------------------
(defconst my-minimal-emacs "24.1"
  "Minimum Emacs version needed to run this init.el.
This version introduced lexical binding.")

(defun my-assert-dependencies ()
  "Check for required dependencies.  Warn the user if any are missing."
  (when (version< emacs-version
                  my-minimal-emacs)
    (display-warning
     'my-init
     (format "my init.el requires Emacs >= %s, you are using %s."
             my-minimal-emacs emacs-version)
     :error)))

;; Don't prevent use of the init.
;; Just warn then let the chips fall where they may.
(my-assert-dependencies)



;; turn off warnings when a fn is redefined by `defadvice'
(setq ad-redefinition-action 'accept)

;;for compatibility with < 24.4 emacs, define `with-eval-after-load'
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
    (declare (indent 1) (debug t))
    `(eval-after-load ,file
       '(progn
          ,@body))))

;;;----------------------------------------------------------------------------
;;; bidi stuff for performance
;;;----------------------------------------------------------------------------
;; sacrifice proper display of right-to-left languages for performance.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;;;----------------------------------------------------------------------------
;;; defvars
;;; This config uses lexical binding. So any dynamic vars from packages not yet
;;; loaded will be incorrectly lexically bound in any `let' statements.
;;; Making a redundant defvar here will ensure they are bound dynamically.
;;;
;;; NOTE: To find dynamic vars that may be accidentally lexically bound, byte
;;; compile init.el then look for the warning "Unused lexical variable".
;;; NOTE: It's useful to include a redundant defvar even if the var is not used
;;; in a `let'. Just to remove the warnings about free variables.
;;; NOTE: buffer local dynamic vars are also defvar'd here. Since no value is
;;; provided it should not affect their local status when they are
;;; "defvarlocal'd" later.
;;;----------------------------------------------------------------------------
;; let bound dynamic vars. ensure they are bound dynamically
(defvar mor-format-automatically-p)
(defvar mor-fix-whitespace-p)
(defvar mor-switch-buff-fn)
(defvar mor-readonly-for-extra-protection-p)
(defvar mor-mode-fn)
(defvar mor-tmp-buffer-mode-map)
(defvar mor-allow-tmp-files-p)
(defvar mor-modes-to-create-tmp-files)
(defvar mor-auto-delete-tmp-files-p)
(defvar helm-candidate-number-limit)
(defvar ivy-height)
(defvar w3-default-homepage)
(defvar w3--args)
(defvar lispy-do-pprint)
(defvar charcoal-color-cnt)
(defvar company-selection-wrap-around)

;; dynamic vars not let bound (yet). Just suppressing the free var warnings.
(defvar evil-normal-state-map)
(defvar key-chord-two-keys-delay)
(defvar key-chord-one-key-delay)
(defvar helm-map)
(defvar evil-insert-state-map)
(defvar evil-visual-state-map)
(defvar key-chord-mode)
(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)
(defvar evil-insert-state-cursor)
(defvar evil-visual-state-cursor)
(defvar evil-operator-state-cursor)
(defvar evil-replace-state-cursor)
(defvar evil-motion-state-cursor)
(defvar evil-flash-delay)
(defvar evil-motion-state-map)
(defvar evil-insert-state-message)
(defvar evil-emacs-state-message)
(defvar evil-visual-state-message)
(defvar evil-motion-state-message)
(defvar evil-normal-state-message)
(defvar evil-operator-state-message)
(defvar evil-replace-state-message)
(defvar evil-mode-line-format)
(defvar evil-default-cursor)
(defvar slime-complete-symbol*-fancy)
(defvar slime-complete-symbol-function)
(defvar slime-mode-map)
(defvar slime-repl-mode-map)
(defvar slime-header-line-p)
(defvar slime-startup-animation)
(defvar slime-default-lisp)
(defvar slime-lisp-implementations)
(defvar slime-words-of-encouragement)
(defvar common-lisp-hyperspec-root)
(defvar slime-macroexpansion-minor-mode-map)
(defvar company-mode-map)
(defvar company-active-map)
(defvar company-candidates-length)
(defvar company-tooltip-minimum-width)
(defvar company-candidates) ; buffer-local
(defvar company-idle-delay)
(defvar company-minimum-prefix-length)
(defvar company-tooltip-limit)
(defvar company-backends)
(defvar web-mode-map)
(defvar org-startup-indented)
(defvar org-log-done)
(defvar org-agenda-timegrid-use-ampm)
(defvar org-src-preserve-indentation)
(defvar org-todo-keywords)
(defvar org-todo-keyword-faces)
(defvar org-agenda-files)
(defvar org-mode-map)
(defvar org-hide-leading-stars)
(defvar js-indent-level)
(defvar js2-highlight-level)
(defvar js2-bounce-indent-p)
(defvar js2-basic-offset)
(defvar js2-mode-show-strict-warnings)
(defvar js2-warn-about-unused-function-arguments)
(defvar js2-mode-map)
(defvar nxml-slash-auto-complete-flag)
(defvar nxml-mode-map)
(defvar helm-ff-transformer-show-only-basename)
(defvar helm-buffers-fuzzy-matching)
(defvar helm-ff-lynx-style-map)
(defvar helm-input-idle-delay)
(defvar helm-idle-delay)
(defvar helm-display-header-line)
(defvar helm-swoop-pre-input-function)
(defvar helm-swoop-speed-or-color)
(defvar icomplete-compute-delay)
(defvar ido-enable-flex-matching)
(defvar ido-everywhere)
(defvar ido-create-new-buffer)
(defvar ido-file-dir-completion-map)
(defvar ido-use-faces)
(defvar ido-vertical-define-keys)
(defvar ido-vertical-show-count)
(defvar ido-show-dot-for-dired)
(defvar yas-triggers-in-field)
(defvar c-default-style)
(defvar c-mode-map)
(defvar c++-mode-map)
(defvar c-basic-offset)
(defvar dired-mode-map)
(defvar evil-auto-indent)
(defvar paredit-backward-delete-key)
(defvar paredit-mode-map)
(defvar paredit-commands)
(defvar omnisharp-mode-map)
(defvar omnisharp-auto-complete-want-documentation)
(defvar omnisharp--curl-executable-path)
(defvar omnisharp-server-executable-path)
(defvar omnisharp--windows-curl-tmp-file-path)
(defvar omnisharp-host)
(defvar omnisharp-company-do-template-completion)
(defvar omnisharp-company-ignore-case)
(defvar avy-keys)
(defvar avy-background)
(defvar avy-all-windows)
(defvar avy-case-fold-search)
(defvar avy-timeout-seconds)
(defvar avy-style)
(defvar compilation-mode-map)
(defvar woman-mode-map)
(defvar eww-link-keymap)
(defvar eww-mode-map)
(defvar custom-mode-map)
(defvar aw-keys)
(defvar aw-background)
(defvar irony-mode-map)
(defvar evil-buffer-regexps)
(defvar web-mode-auto-close-style)
(defvar web-mode-markup-indent-offset)
(defvar web-mode-css-indent-offset)
(defvar web-mode-code-indent-offset)
(defvar web-mode-sql-indent-offset)
(defvar eshell-mode-map)
(defvar highlight-tail-colors)
(defvar highlight-tail-steps)
(defvar highlight-tail-timer)
(defvar highlight-tail-posterior-type)
(defvar shr-use-fonts)
(defvar eww-download-directory)
(defvar magit-mode-map)
(defvar magit-completing-read-function)
(defvar magit-diff-refine-hunk)
(defvar magit-log-arguments)
(defvar magit-log-section-commit-count)
(defvar ediff-split-window-function)
(defvar ediff-window-setup-function)
(defvar ediff-diff-program)
(defvar ediff-diff3-program)
(defvar darkroom-margins)
(defvar darkroom-fringes-outside-margins)
(defvar darkroom-text-scale-increase)
(defvar fci-rule-column)
(defvar fci-rule-width)
(defvar fci-always-use-textual-rule)
(defvar fci-dash-pattern)
(defvar fci-rule-use-dashes)
(defvar flycheck-mode-map)
(defvar erc-mode-map)
(defvar guide-key/idle-delay)
(defvar guide-key/guide-key-sequence)
(defvar guide-key/recursive-key-sequence-flag)
(defvar swiper-map)
(defvar swiper-all-map)
(defvar ivy-occur-mode-map)
(defvar ivy-help-file)
(defvar ivy-initial-inputs-alist)
(defvar ivy-re-builders-alist)
(defvar ivy-display-style)
(defvar lispy-avy-style-char)
(defvar lispy-avy-style-paren)
(defvar lispy-avy-style-symbol)
(defvar lispy-mode-map-special)
(defvar lispy-mode-map)
(defvar cider-eval-result-prefix)
(defvar elisp-slime-nav-mode-map)
(defvar nlinum-relative-current-symbol)
(defvar my-date-mode-map)
(defvar iedit-toggle-key-default)
(defvar evil-ex-completion-map)
(defvar evil-read-key-map)
(defvar evil-replace-state-map)
(defvar ivy-mode-map)
(defvar winner-dont-bind-my-keys)
(defvar winner-ring-size)
(defvar winner-mode-map)
(defvar js2-highlight-vars-local-keymap)
(defvar js2--highlight-vars-tokens) ; buffer-local, just flagging as dynamic.
(defvar js2--highlight-vars-current-token-name) ; buffer-local
(defvar js2--highlight-vars-current-token) ; buffer-local
(defvar js2--highlight-vars-post-command-timer) ; buffer-local
(defvar python-mode-map)
(defvar calendar-minimum-window-height)
(defvar lua-indent-level)
(defvar lua-default-application)
(defvar ggtags-mode-map)
(defvar follow-mode-map)
(defvar cider-prompt-for-symbol)
(defvar cider-repl-display-help-banner)
(defvar cider-repl-mode-map)
(defvar geiser-default-implementation)
(defvar geiser-active-implementations)
(defvar bookmark-sort-flag)
(defvar bookmark-save-flag)
(defvar auto-revert-interval)
(defvar auto-revert-stop-on-user-input)
(defvar global-auto-revert-non-file-buffers)
(defvar auto-revert-remote-files)
(defvar typescript-indent-level)
(defvar flycheck-check-syntax-automatically)
(defvar company-tooltip-align-annotations)
(defvar display-line-number-width)
(defvar display-line-numbers-current-absolute)
(defvar display-line-numbers)
(defvar grep-highlight-matches)
(defvar grep-mode-map)
(defvar find-function-C-source-directory)
(defvar browse-url-generic-program)
(defvar browse-url-browser-function)
(defvar display-time-default-load-average)
(defvar display-time-load-average)
(defvar display-time-load-average-threshold)
(defvar display-time-format)
(defvar hl-line-sticky-flag)
(defvar global-hl-line-sticky-flag)
(defvar show-paren-delay)
(defvar w32-pipe-read-delay)
(defvar company-selection) ; buffer-local
(defvar company-tooltip-offset) ; buffer-local
(defvar ibuffer-show-empty-filter-groups)
(defvar ibuffer-saved-filter-groups)
(defvar sql-product)
(defvar sqlind-default-indentation-offsets-alist)
(defvar feebleline-mode-line-text)
(defvar web-mode-enable-current-element-highlight)
(defvar web-mode-enable-current-column-highlight)
(defvar highlight-indent-guides-method)
(defvar highlight-indent-guides-character)
(defvar ido-work-directory-list)
(defvar cquery-executable)
(defvar ibuffer-expert)
(defvar erc-header-line-format)
(defvar ispell-program-name)
(defvar diff-mode-map)
(defvar erc-timestamp-format)
(defvar erc-timestamp-format-right)
(defvar slime-completion-at-point-functions)
(defvar erc-autojoin-channels-alist)
(defvar ccls-executable)
(defvar deadgrep-max-line-length)
(defvar rg-show-columns)
(defvar rg-group-result)
(defvar rg-align-position-numbers)
(defvar rg-mode-map)
(defvar rg-command-line-flags)
(defvar rg-ignore-case)
(defvar eros-eval-result-prefix)
(defvar eros-eval-result-duration)
(defvar unkillable-scratch-behavior)
(defvar unkillable-scratch-do-not-reset-scratch-buffer)
(defvar ido-decorations)
(defvar inhibit-compacting-font-caches)
(defvar use-default-font-for-symbols)
(defvar swiper-isearch-highlight-delay)
(defvar mini-modeline-update-interval)
(defvar *minesweeper-board-width*)
(defvar *minesweeper-board-height*)
(defvar *minesweeper-mines*)
(defvar nov-text-width)
(defvar nov-mode-map)
(defvar nov-unzip-program)
(defvar display-fill-column-indicator-mode)
(defvar flycheck-emacs-lisp-load-path)
(defvar nov-variable-pitch)
(defvar read-process-output-max)
(defvar swiper-use-visual-line-p)
(defvar python-shell-interpreter)
(defvar electric-spacing-operators)
(defvar electric-spacing-control-statement-parens)
(defvar electric-spacing-double-space-docs)
(defvar c-hanging-semi&comma-criteria)
(defvar erc-modules)
(defvar company-prefix) ; buffer-local
(defvar icomplete-fido-mode-map)
(defvar Info-mode-map)
(defvar ggtags-update-on-save)
(defvar ggtags-highlight-tag)
(defvar ggtags-sort-by-nearness)
(defvar ggtags-navigation-mode-lighter)
(defvar ggtags-mode-line-project-name)
(defvar ggtags-oversize-limit)
(defvar ggtags-executable-directory)
(defvar ggtags-enable-navigation-keys)
(defvar package-archives)
(defvar cookie-file)
(defvar rust-mode-map)
(defvar rust-indent-offset)
(defvar inferior-lisp-program)
(defvar icomplete-tidy-shadowed-file-names) ; buffer-local
(defvar icomplete-show-matches-on-no-input) ; buffer-local
(defvar icomplete-hide-common-prefix)       ; buffer-local
(defvar erc-default-server)
(defvar citre-ctags-program)
(defvar citre-readtags-program)
(defvar csharp-mode-map)
(defvar puni-mode-map)
(defvar ruby-mode-map)
(defvar ruby-indent-level)
(defvar ivy-posframe-display-functions-alist)
(defvar smerge-mode-map)
(defvar lua-mode-map)
(defvar evil-escape-key-sequence)
(defvar evil-escape-delay)
(defvar ido-grid-rows)
(defvar ido-grid-bind-keys)
(defvar ido-grid-max-columns)
(defvar ido-grid-start-small)
(defvar ido-grid--rows)
(defvar ido-grid--cols)
(defvar ido-completion-map)
(defvar vertico-count)
(defvar python-indent-offset)
(defvar jsonian-indentation)
(defvar repeat-exit-timeout)
(defvar citre-mode-map)
(defvar go-mode-map)
(defvar my-which-func-use-postip)
(defvar flymake-mode-map)
(defvar eglot-events-buffer-size)
(defvar eglot-autoshutdown)
(defvar eglot-ignored-server-capabilities)
(defvar zig-indent-offset)

;; suppress warnings on functions from files not yet loaded.
(declare-function swiper 'swiper)
(declare-function helm-swoop 'helm-swoop)
(declare-function helm-keyboard-quit 'helm)
(declare-function mor-copy-back 'mode-on-region)
(declare-function mor-close-tmp-buffer 'mode-on-region)
(declare-function js2-mark-defun 'js2-mode)
(declare-function feebleline-mode 'feebleline)
;; TODO: change `suppress' to the actual feature or file.
;;       Just suppressing warnings for now.
(declare-function ivy-completing-read 'suppress)
(declare-function my-setup-slime-repl 'suppress)
(declare-function slime-last-expression 'suppress)
(declare-function slime-eval-async 'suppress)
(declare-function my-slime-eval-last-sexp-display 'suppress)
(declare-function slime-documentation-lookup 'suppress)
(declare-function my-view-hyperspec 'suppress)
(declare-function counsel-cl 'counsel)
(declare-function my-setup-company-web 'suppress)
(declare-function csharp-mode-hook 'suppress)
(declare-function my-setup-csharp-mode 'suppress)
(declare-function my-setup-js 'suppress)
(declare-function js2-next-error 'suppress)
(declare-function js2-display-error-list 'suppress)
(declare-function flymake-goto-next-error 'suppress)
(declare-function flymake-goto-prev-error 'suppress)
(declare-function flymake-popup-current-error-menu 'suppress)
(declare-function my-js2-init 'suppress)
(declare-function my-setup-json-mode 'suppress)
(declare-function helm-buffers-list 'suppress)
(declare-function helm-M-x 'suppress)
(declare-function helm-find-files 'suppress)
(declare-function helm-show-kill-ring 'suppress)
(declare-function helm-imenu 'suppress)
(declare-function helm-mode 'suppress)
(declare-function ido-ubiquitous-mode 'suppress)
(declare-function ido-vertical-mode 'suppress)
(declare-function ido-grid-mode 'suppress)
(declare-function yas-load-directory 'suppress)
(declare-function my-setup-c-mode-common 'suppress)
(declare-function my-setup-c-mode 'suppress)
(declare-function my-setup-c++-mode 'suppress)
(declare-function dired-man 'suppress)
(declare-function s-trim 'suppress)
(declare-function my-setup-sql 'suppress)
(declare-function omnisharp-go-to-definition 'suppress)
(declare-function eww-reload 'suppress)
(declare-function my-proj-progit2-dired 'suppress)
(declare-function my-proj-ydnjs 'suppress)
(declare-function icicle-mode 'suppress)
(declare-function my-setup-web-mode 'suppress)
(declare-function my-setup-skewer-mode 'suppress)
(declare-function eshell/clear-scrollback 'suppress)
(declare-function eshell/clear 'suppress)
(declare-function my-eshell-clear 'suppress)
(declare-function my-tail-colors-for-bg 'suppress)
(declare-function highlight-tail-reload 'suppress)
(declare-function eww-back-url 'suppress)
(declare-function eww-forward-url 'suppress)
(declare-function shr-next-link 'suppress)
(declare-function my-setup-eww 'suppress)
(declare-function w3 'suppress)
(declare-function ediff-setup-windows-plain 'suppress)
(declare-function helm-w32-launcher 'suppress)
(declare-function company-turn-off-fci 'suppress)
(declare-function company-maybe-turn-on-fci 'suppress)
(declare-function flycheck-next-error 'suppress)
(declare-function flycheck-previous-error 'suppress)
(declare-function x/ido-chat-buffer 'suppress)
(declare-function my-setup-erc 'suppress)
(declare-function ivy-switch-buffer 'suppress)
(declare-function counsel-tmm 'counsel)
(declare-function counsel-M-x 'counsel)
(declare-function counsel-find-file 'counsel)
(declare-function counsel-load-theme 'counsel)
(declare-function my-counsel-load-theme 'my-load-theme)
(declare-function counsel-describe-variable 'counsel)
(declare-function counsel-describe-function 'counsel)
(declare-function counsel-yank-pop 'counsel)
(declare-function counsel-git 'counsel)
(declare-function swiper-avy 'suppress)
(declare-function ivy-occur-next-line 'suppress)
(declare-function ivy-occur-previous-line 'suppress)
(declare-function with-ivy-window 'suppress)
(declare-function ivy-mode 'suppress)
(declare-function my-js2-mode-on-region 'suppress)
(declare-function my-focus-javascript2 'suppress)
(declare-function my-unfocus-javascript 'suppress)
(declare-function lispy-mode 'suppress)
(declare-function lispy-set-key-theme 'suppress)
(declare-function lispy-right-p 'suppress)
(declare-function lispy-forward 'suppress)
(declare-function lispy--eval 'suppress)
(declare-function lispy--string-dwim 'suppress)
(declare-function lispy-alt-multiline 'suppress)
(declare-function lispy-left-p 'suppress)
(declare-function lispy-slurp 'suppress)
(declare-function lispy-barf 'suppress)
(declare-function lispy-define-key 'suppress)
(declare-function my-lispy-go-left-barf-or-slurp 'suppress)
(declare-function my-lispy-go-right-barf-or-slurp 'suppress)
(declare-function lispy-newline-and-indent-plain 'suppress)
(declare-function counsel-el 'counsel)
(declare-function my-quit-window-date 'suppress)
(declare-function js2-node-at-point 'suppress)
(declare-function js2-name-node-p 'suppress)
(declare-function js2-node-get-enclosing-scope 'suppress)
(declare-function js2-name-node-name 'suppress)
(declare-function js2-node-abs-pos 'suppress)
(declare-function js2-get-defining-scope 'suppress)
(declare-function js2-visit-ast 'suppress)
(declare-function js2-node-len 'suppress)
(declare-function js2--unhighlight-vars 'suppress)
(declare-function my-setup-python-smart-tabs-mode 'suppress)
(declare-function my-setup-python 'suppress)
(declare-function my-setup-lua-mode 'suppress)
(declare-function my-setup-swift-mode 'suppress)
(declare-function my-setup-clojure-mode 'suppress)
(declare-function cider-repl-clear-buffer 'suppress)
(declare-function my-setup-cider-repl 'suppress)
(declare-function hl-line-flash 'suppress)
(declare-function my-setup-scheme 'suppress)
(declare-function my-setup-geiser-repl 'suppress)
(declare-function my-setup-adoc-mode 'suppress)
(declare-function my-setup-markdown-mode 'suppress)
(declare-function my-setup-typescript-mode 'suppress)
(declare-function my-setup-powershell-mode 'suppress)
(declare-function my-setup-css-mode 'suppress)
(declare-function my-setup-zig-mode 'suppress)
(declare-function my-setup-jsonian 'suppress)
(declare-function my-setup-tide-for-ts 'suppress)

(declare-function my-occur-wild-spaces 'suppress)
(declare-function my-w32-get-code 'suppress)
(declare-function evil-normal-state 'suppress)
(declare-function evil-exit-visual-state 'suppress)
(declare-function evil-next-visual-line 'suppress)
(declare-function evil-previous-visual-line 'suppress)
(declare-function evil-leader/set-leader 'suppress)
(declare-function my-quit-window 'suppress)
(declare-function my-shrink-window-horizontally 'suppress)
(declare-function my-enlarge-window-horizontally 'suppress)
(declare-function evil-visual-block 'suppress)
(declare-function my-w32-run 'suppress)
(declare-function my-toggle-frame-max 'suppress)
(declare-function my-change-font-size 'my-font-stuff)
(declare-function my-load-theme 'my-load-theme)
(declare-function color 'suppress)
(declare-function my-cycle-theme 'my-load-theme)
(declare-function my-cycle-light-bg 'my-load-theme)
(declare-function my-cycle-light-bg-forward 'my-load-theme)
(declare-function my-cycle-light-bg-backward 'my-load-theme)
;; (declare-function my-handle-weird-theme-setups 'suppress)
(declare-function my-load-theme-wrapper 'my-load-theme)
(declare-function my-toggle-inverse-video 'my-load-theme)
(declare-function my-rainbow-parens-dark-bg 'my-color-theme-mods)
(declare-function my-rainbow-parens-dark-bg-bold 'my-color-theme-mods)
(declare-function my-rainbow-parens-light-bg 'my-color-theme-mods)
(declare-function my-rainbow-parens-light-bg2 'my-color-theme-mods)
(declare-function my-rainbow-parens-light-bg3 'my-color-theme-mods)
(declare-function my-color-grandshell 'my-color-theme-mods)
(declare-function my-color-zenburn 'my-color-theme-mods)
(declare-function my-color-github 'my-color-theme-mods)
(declare-function my-color-badger 'my-color-theme-mods)
(declare-function my-color-gruvbox 'my-color-theme-mods)
(declare-function my-color-gruvbox-dark 'my-color-theme-mods)
(declare-function my-color-monokai 'my-color-theme-mods)
(declare-function my-color-tommyh 'my-color-theme-mods)
(declare-function my-color-default 'my-color-theme-mods)
(declare-function my-color-default-fancy 'my-color-theme-mods)
(declare-function my-color-gandalf 'my-color-theme-mods)
(declare-function my-color-leuven 'my-color-theme-mods)
(declare-function my-color-dichromacy 'my-color-theme-mods)
(declare-function my-color-firebelly 'my-color-theme-mods)
(declare-function my-color-molokai 'my-color-theme-mods)
(declare-function my-color-majapahit 'my-color-theme-mods)
(declare-function my-color-deeper-blue 'my-color-theme-mods)
(declare-function my-color-kosmos 'my-color-theme-mods)
(declare-function my-color-niflheim 'my-color-theme-mods)
(declare-function my-color-spacemacs-light 'my-color-theme-mods)
(declare-function my-color-tango-dark 'my-color-theme-mods)
(declare-function my-color-sunburn 'my-color-theme-mods)
(declare-function my-color-overcast 'my-color-theme-mods)
(declare-function my-color-warm-night 'my-color-theme-mods)
(declare-function my-color-avk-daylight 'my-color-theme-mods)
(declare-function my-set-alpha 'suppress)
(declare-function my-change-alpha 'suppress)
(declare-function my-change-alpha-more-solid 'suppress)
(declare-function my-change-alpha-less-solid 'suppress)
;; these 2 like to break?
(declare-function evil-visual-char 'evil-states)
(declare-function evil-define-key 'evil-core)

(declare-function slime-edit-definition 'suppress)
(declare-function evil-append 'suppress)
(declare-function company-select-next 'suppress)
(declare-function company-select-previous 'suppress)
(declare-function company-show-doc-buffer 'suppress)
(declare-function company-next-page 'suppress)
(declare-function company-previous-page 'suppress)
(declare-function company-set-selection 'suppress)
(declare-function my-company-jump-to-first 'suppress)
(declare-function my-company-jump-to-last 'suppress)
(declare-function my--company-set-min-width 'suppress)
(declare-function my-open-main-todo 'suppress)
(declare-function evil-window-left 'suppress)
(declare-function align-let 'suppress)
(declare-function evil-window-down 'suppress)
(declare-function my-js2-prev-error 'suppress)
(declare-function my-hydra-js2-flymake/body 'suppress)
(declare-function hydra-smerge/body 'suppress)
(declare-function my-grep-dwim 'suppress)
(declare-function evil-window-up 'suppress)
(declare-function dired-hide-details-mode 'suppress)
(declare-function my-setup-dired 'suppress)
(declare-function dired-up-directory 'suppress)
(declare-function dired-next-line 'suppress)
(declare-function dired-previous-line 'suppress)
(declare-function evil-forward-word-begin 'suppress)
(declare-function evil-forward-word-end 'suppress)
(declare-function evil-search-next 'suppress)
(declare-function evil-search-previous 'suppress)
(declare-function evil-window-top 'suppress)
(declare-function evil-window-middle 'suppress)
(declare-function evil-window-bottom 'suppress)
(declare-function evil-goto-line 'suppress)
(declare-function dired-copy-filename-as-kill 'suppress)
(declare-function dired-find-file 'suppress)
(declare-function dired-do-hardlink 'suppress)
(declare-function dired-do-chmod 'suppress)
(declare-function dired-do-load 'suppress)
(declare-function dired-sort-toggle-or-edit 'suppress)
(declare-function dired-do-chgrp 'suppress)
(declare-function er/contract-region 'suppress)
(declare-function hydra-expand-region/body "my-hydras")
(declare-function paredit-forward-barf-sexp 'paredit)
(declare-function paredit-forward-slurp-sexp 'paredit)
(declare-function paredit-backward-slurp-sexp 'paredit)
(declare-function paredit-backward-barf-sexp 'paredit)
(declare-function paredit-raise-sexp 'paredit)
(declare-function omnisharp-find-usages 'suppress)
(declare-function omnisharp-find-implementations 'suppress)
(declare-function omnisharp-run-code-action-refactoring 'suppress)
(declare-function omnisharp-fix-code-issue-at-point 'suppress)
(declare-function omnisharp-rename 'suppress)
(declare-function mor-mode-on-region 'suppress)
(declare-function my-proj-dive-python 'suppress)
(declare-function my-proj-pcl 'suppress)
(declare-function my-proj-progit2 'suppress)
(declare-function proj-safetyweb 'suppress)
(declare-function my-proj-safetyweb 'suppress)
(declare-function my-proj-safetyweb-ects 'suppress)
(declare-function my-proj-rsims 'suppress)
(declare-function my-proj-daily-diff 'suppress)
(declare-function my-proj-db-safety 'suppress)
(declare-function my-proj-trighist 'suppress)
(declare-function my-proj-emacs 'suppress)
(declare-function my-proj-cl 'suppress)
(declare-function my-proj-imgtag 'suppress)
(declare-function my-proj-cpp 'suppress)
(declare-function my-proj-tcpl 'suppress)
(declare-function my-proj-ticpp 'suppress)
(declare-function my-open-user-folder 'suppress)
(declare-function my-open-dev-folder 'suppress)
(declare-function my-current-file-path 'suppress)
(declare-function my-current-folder-path 'suppress)
(declare-function my-folder-nav 'suppress)
(declare-function my-choose-hydra "my-hydras")
(declare-function hydra-easyscroll/body "my-hydras")
(declare-function mor-prev-mode-on-region 'suppress)
(declare-function my-window-search 'suppress)
(declare-function lispy-kill 'suppress)
(declare-function evil-find-char 'suppress)
(declare-function evil-backward-word-begin 'suppress)
(declare-function evil-goto-first-line 'suppress)
(declare-function my-eval-last-sexp 'suppress)
(declare-function my-eval-last-sexp-display 'suppress)
(declare-function my-elisp-slime-nav-colored 'suppress)
(declare-function my-real-estate-max 'suppress)
(declare-function my-real-estate-hide-mode-line 'suppress)
(declare-function my-real-estate-hide-fringe 'suppress)
(declare-function my-insert-date-big 'suppress)
(declare-function my-insert-date-short 'suppress)
(declare-function my-insert-date-string-new-buff 'suppress)
(declare-function evil-force-normal-state 'suppress)
(declare-function my-delete-process-at-point 'suppress)
(declare-function my-setup-prog-mode 'suppress)
(declare-function sallet-buffer 'suppress)
(declare-function sunrise-cd 'suppress)
(declare-function winner-undo 'suppress)
(declare-function winner-redo 'suppress)
(declare-function xref-pulse-momentarily 'suppress)
(declare-function my--occur-move 'suppress)
(declare-function my-occur-next 'suppress)
(declare-function my-occur-prev 'suppress)
(declare-function my-occur-mode-goto-occurrence 'suppress)
(declare-function my--occur-jump-to-first-match 'suppress)
(declare-function evil-backward-char 'suppress)
(declare-function evil-window-right 'suppress)
(declare-function my-square-one 'suppress)
(declare-function w32-send-sys-command 'suppress)
(declare-function my-company-cycle-position 'suppress)
(declare-function company--pseudo-tooltip-height 'suppress)
(declare-function my-company-page-size 'suppress)
(declare-function my-company-next-page 'suppress)
(declare-function my-company-previous-page 'suppress)
(declare-function company-complete-common 'suppress)
(declare-function my-setup-ibuffer-mode 'suppress)
(declare-function ibuffer-switch-to-saved-filter-groups 'suppress)
(declare-function my-js2-indent-defun 'suppress)
(declare-function my-cycle-line-position 'my-cycle-line-position)
(declare-function my-next-cycle-pos 'suppress)
(declare-function my-company-complete-common 'suppress)
(declare-function my-find-file-by-name 'suppress)
(declare-function my-proj-paip 'suppress)
(declare-function highlight-tail-mode 'highlight-tail)
(declare-function my-proj-emacs-manual 'suppress)
(declare-function my-ido-find-file 'suppress)
(declare-function my-setup-cquery 'suppress)
(declare-function my-line-numbers-cycle 'suppress)
(declare-function my-cycle-ivy-match-style 'suppress)
(declare-function my-code-snippet-url 'my-code-snippet-url)
(declare-function my-erc-set-data 'offline)
(declare-function my-proj-sicp 'suppress)
(declare-function lsp-cquery-enable 'suppress)
(declare-function dired-details-install 'dired-details)
(declare-function my-cycle-fonts 'suppress)
(declare-function my-cycle-font-forward 'my-font-cycle)
(declare-function my-cycle-font-backward 'my-font-cycle)
(declare-function my-insert-ruler 'my-ruler)
(declare-function my-longest-line 'my-ruler)
(declare-function my-load-everything-for-pdump 'my-pdump)
(declare-function my-make-pdump 'my-pdump)
(declare-function my-delete-brackets 'my-misc)
(declare-function my-inject-newlines 'my-misc)
(declare-function my-list-holidays 'my-misc)
(declare-function my-indent-defun 'my-misc)
(declare-function my-win-count 'my-misc)
(declare-function find-shell 'my-misc)
(declare-function what-face 'my-misc)
(declare-function my-cycle-spacing 'my-misc)
(declare-function my-what-line 'my-misc)
(declare-function my-what-position 'my-misc)
(declare-function my-what-time 'my-misc)
(declare-function my-time-task 'my-misc)
(declare-function my-str-starts-with-p 'my-misc)
(declare-function my-str-ends-with-p 'my-misc)
(declare-function my-turn-on-electric-pair-local-mode 'my-misc)
(declare-function my-cursor-light-bg 'my-color-theme-mods)
(declare-function my-cursor-dark-bg 'my-color-theme-mods)
(declare-function my-set-frame-font-ivy 'my-font-stuff)
(declare-function my-change-font-size-bigger 'my-font-stuff)
(declare-function my-change-font-size-smaller 'my-font-stuff)
(declare-function my-load-theme-make-bold-like-zenburn 'my-load-theme)
(declare-function my-load-theme-inverse 'my-load-theme)
(declare-function my-follow-mode 'my-misc)
(declare-function my-line-numbers-on 'my-line-nums)
(declare-function my-line-numbers-relative-on 'my-line-nums)
(declare-function my-line-numbers-off 'my-line-nums)
(declare-function my-scroll-right 'my-horizontal-scroll)
(declare-function my-scroll-left 'my-horizontal-scroll)
(declare-function evil-forward-char 'evil-commands)
(declare-function evil-change-whole-line 'evil-commands)
(declare-function wgrep-change-to-wgrep-mode 'wgrep)
(declare-function rg-save-search-as-name 'rg)
(declare-function rg-save-search 'rg)
(declare-function my-find-file-omni 'my-misc)
(declare-function eros--eval-overlay 'eros)
(declare-function slime-eval 'slime)
(declare-function -contains-p 'dash)
(declare-function s-starts-with-p 's)
(declare-function s-ends-with-p 's)
(declare-function s-contains-p 's)
(declare-function eros--make-result-overlay 'eros)
(declare-function slime-eval-last-expression-eros 'suppress)
(declare-function vterm 'vterm)
(declare-function nov-goto-toc 'nov)
(declare-function org-html-export-as-html 'ox-html)
(declare-function org-html-export-to-html 'ox-html)
(declare-function my-setup-java-mode 'suppress)
(declare-function tide-setup 'tide)
(declare-function tide-hl-identifier-mode 'tide)
(declare-function ctrlf-mode 'ctrlf)
(declare-function selectrum-prescient-mode 'selectrum-prescient)
(declare-function prescient-persist-mode 'prescient)
(declare-function my-setup-inferior-python-mode 'suppress)
(declare-function c-toggle-hungry-state 'cc-cmds)
(declare-function company-complete-number 'company)
(declare-function company-abort 'company)
(declare-function my-set-jslint-compile-command 'suppress)
(declare-function icomplete-simple-completing-p 'icomplete)
(declare-function my-kbd-sql-delete-brackets 'suppress)
(declare-function my-kbd-sql-move-stuff-left 'suppress)
(declare-function my-kbd-sql-fix-col-1 'suppress)
(declare-function my-kbd-sql-fix-col-n 'suppress)
(declare-function Info-history-back 'info)
(declare-function package-installed-p 'package)
(declare-function rg-list-searches 'rg-ibuffer)
(declare-function ggtags-show-definition 'ggtags)
(declare-function magit-copy-buffer-revision 'magit-extras)
(declare-function my-setup-log-edit-mode 'suppress)
(declare-function slime 'slime)
(declare-function slime-mode 'slime)
(declare-function slime-connect 'slime)
(declare-function slime-selector 'slime)
(declare-function slime-lisp-mode-hook 'slime)
(declare-function slime-setup 'slime)
(declare-function s-split 's)
(declare-function tree-sitter-hl-mode 'tree-sitter-hl)
(declare-function tree-sitter-mode 'tree-sitter)
(declare-function my-turn-off-tree-sitter-hl 'suppress)
(declare-function my-turn-on-tree-sitter-hl 'suppress)
(declare-function my-add-yas-tree-sitter-hooks 'suppress)
(declare-function wgrep-setup "wgrep" ())
(declare-function Info-follow-nearest-node "info")
(declare-function my-setup-ruby-mode "puni")
(declare-function puni-barf-forward "puni")
(declare-function puni-slurp-forward "puni")
(declare-function puni-slurp-backward "puni")
(declare-function puni-barf-backward "puni")
(declare-function puni-raise "puni")
(declare-function puni-split "puni")
(declare-function puni-splice "puni")
(declare-function visual-indentation-mode "visual-indentation-mode")
(declare-function my-setup-fennel-mode "init")
(declare-function lua-send-region "lua-mode")
(declare-function lua-send-buffer "lua-mode")
(declare-function lua-send-defun "lua-mode")
(declare-function lua-send-current-line "lua-mode")
(declare-function my-ido-grid-keybinds-hook "init")
(declare-function my-ido-grid-page-up "init")
(declare-function my-ido-grid-page-down "init")
(declare-function ido-grid-down "ido-grid")
(declare-function ido-grid-up "ido-grid")
(declare-function ido-grid-display-more-rows "ido-grid")
(declare-function ido-grid-down-or-expand "ido-grid")
(declare-function ido-grid-up-or-expand "ido-grid")
(declare-function ido-grid-left "ido-grid")
(declare-function ido-grid-right "ido-grid")
(declare-function ido-grid--shift "ido-grid")
(declare-function citre-peek 'suppress)

(declare-function smerge-kill-current "smerge-mode")
(declare-function smerge-resolve "smerge-mode")
(declare-function smerge-combine-with-next "smerge-mode")
(declare-function smerge-refine "smerge-mode")
(declare-function smerge-diff-base-lower "smerge-mode")
(declare-function smerge-diff-upper-lower "smerge-mode")
(declare-function smerge-diff-base-upper "smerge-mode")
(declare-function smerge-keep-current "smerge-mode")
(declare-function smerge-keep-all "smerge-mode")
(declare-function smerge-keep-lower "smerge-mode")
(declare-function smerge-keep-upper "smerge-mode")
(declare-function smerge-keep-base "smerge-mode")
(declare-function smerge-prev "smerge-mode")
(declare-function smerge-auto-leave "smerge-mode")
(declare-function smerge-next "smerge-mode")

;; TODO: fix wrong use of declare-function above. Replace the symbols with
;; string filenames of lisp files in the `load-path'.

;; silence more byte compiler warnings.
;; NOTE: it shoudln't matter if lib has not be added to `load-path' yet as
;; this code only runs when compiling, not during init.
;; Commenting out. Makes byte compilation very slow on MS-windows
;; (cl-eval-when 'compile (require 'company))
;; (cl-eval-when 'compile (require 'info))
;; (cl-eval-when 'compile (require 'rg))
;; (cl-eval-when 'compile (require 'magit))
;; (cl-eval-when 'compile (require 'python))



;;;----------------------------------------------------------------------------
;;; Helper functions and macros
;;;----------------------------------------------------------------------------
;; eval-when-compile used to prevent flycheck `cl' warning, but only works for
;; macros?
(require 'cl-lib)

(autoload #'my-time-task "my-misc" nil t)
(autoload #'my-str-starts-with-p "my-misc" nil t)
(autoload #'my-str-ends-with-p "my-misc" nil t)
;; for some reason this breaks if it's autoloaded. When setting
;; `my-curr-computer' So defun it right here.
(defun my-get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))
(autoload #'my-turn-on-electric-pair-local-mode "my-misc" nil t)

;;;----------------------------------------------------------------------------
;;; flags used for conditional execution
;;;----------------------------------------------------------------------------
;; (display-graphic-p)
;; system-type
;; my-curr-computer

(defvar my-computers
  '(work-laptop
    work-laptop-bash
    work-laptop-2019
    work-laptop-mac
    raspberry-pi
    utilite
    old-sony-vaio
    a-tower
    a-laptop-old
    a-laptop-faster
    leyna-laptop
    hp-tower-2009
    wild-dog
    mac-mini-m1-2021)
  "The computers I use Emacs on.
Specific configurations may be made for some computers.")

;; currently used computer. (manually set)
;; Used to conditionally set computer specific options, and paths.
;; NOTE: When setting up emacs on a new computer create file
;; ~/.emacs.d/my-curr-computer.txt
;; Then type the name of the symbol (see `my-computers') in the text file.
;; The file should contain 1 line and no whitespace. The text will be converted
;; to a symbol.
(defconst my-curr-computer
  (let ((curr-comp-file "~/.emacs.d/my-curr-computer.txt"))
    (ignore-errors
      (when (file-exists-p curr-comp-file)
        (intern (my-get-string-from-file curr-comp-file)))))
  "The computer running this Emacs.  Identified by a flag file.
nil if computer is unknown.
Specific configs may be made based on the computer.")

(when (memq my-curr-computer '(mac-mini-m1-2021
                               work-laptop-mac))
  ;; so rg is found. and other homebrew programs.
  (push "/opt/homebrew/bin" exec-path))

;;;----------------------------------------------------------------------------
;;; globals
;;;----------------------------------------------------------------------------
(defvar my-indent-width 4
  "An omni-variable serving 3 related purposes.
Because I want them to have same value.
-Preferred indent width for C-like langs.  `c-basic-offset' `js2-basic-offset'
-Number of spaces for a tab.
-How many columns to show for a 'real' tab.  `tab-width'")

(defvar my-graphic-p (display-graphic-p)
  "Caching the result of `display-graphic-p'.
Since it is used everywhere and won't change.")


;; TODO: look into a way to limit the values to evil, emacs, and cua.
;;       Like an enum. defcustom?
;; TODO: support cua.
(defvar my-ui-type 'evil
  "The user interface type I'm currently using.
Choices: `evil' `emacs' `cua'")

(defvar my-use-evil-p (eq my-ui-type 'evil)
  "Whether I'm using evil at the moment or not.
Just a convenience to avoid checks against `my-ui-type'.")

(defvar my-use-js2-highlight-vars-p (not (version< emacs-version "24.4")))

(defvar my-narrow-type
  (cond ((eq my-curr-computer 'wild-dog) 'bare-ido)
        ((eq my-curr-computer 'work-laptop-2019) 'bare-ido)
        ((eq my-curr-computer 'mac-mini-m1-2021) 'bare-ido)
        ((eq my-curr-computer 'work-laptop-mac) 'bare-ido)
        (t 'bare-ido))
  "The package I'm currently using for narrowing completions.
Use nil for the Emacs default.
Use bare-ido for ido without the extra ido packages.
`selectrum' has been removed as an option as it is relaced by vertico.
Choices: ivy fancy-ido grid-ido bare-ido helm icicles sallet fido
icomplete mish-mash vertico nil")

;;TODO: make ivy pop-up it's window on the Linux tty.
(defvar my-use-ivy-p (eq my-narrow-type 'ivy)
  "If I'm using ivy completion at the moment.
Just a convenience to avoid checks against `my-narrow-type'.")

(defvar my-use-helm-p (eq my-narrow-type 'helm)
  "Whether I'm using helm at the moment or not.
Just a convenience to avoid checks against `my-narrow-type'.")
(defvar my-load-helm-on-init-p t
  "Set to t to load helm during start up.
Otherwise postpone loading helm until the first attempted use.")

(defvar my-use-fancy-ido-p (eq my-narrow-type 'fancy-ido)
  "If I'm using ido at the moment.
Just a convenience to avoid checks against `my-narrow-type'.")

(defvar my-use-grid-ido-p (eq my-narrow-type 'grid-ido)
  "If I'm using ido at the moment.
Just a convenience to avoid checks against `my-narrow-type'.")

(defvar my-use-bare-ido-p (eq my-narrow-type 'bare-ido)
  "If I'm using bare-ido at the moment.  Without lots of extra ido packages.
Just a convenience to avoid checks against `my-narrow-type'.")

(defvar my-use-mish-mash-p (eq my-narrow-type 'mish-mash)
  "If I'm using combination of several narrowing packages.
Just a convenience to avoid checks against `my-narrow-type'.")

(defvar my-swoop-fn (cond
                     ;; NOTE: fido breaks swiper. Keep this at the top
                     ((eq my-narrow-type 'fido) #'my-occur-wild-spaces)
                     ;; per computer. keep this above "per completion type".
                     ((eq my-curr-computer 'mac-mini-m1-2021) #'swiper-isearch)
                     ((eq my-curr-computer 'work-laptop-mac) #'swiper-isearch)
                     ;; for now prefer #'swiper-isearch. keeping old logic
                     ;; below just for informational purposes.
                     (t #'swiper-isearch)
                     ;; per completion type
                     (my-use-ivy-p #'swiper-isearch)
                     ;; `ido-occur' is fast but does not split inputs on
                     ;; spaces. use swiper with ido for now.
                     (my-use-fancy-ido-p #'swiper-isearch)
                     (my-use-grid-ido-p #'swiper-isearch)
                     (my-use-bare-ido-p #'my-occur-wild-spaces)
                     (my-use-helm-p #'helm-occur)
                     (my-use-mish-mash-p #'swiper-isearch)
                     ;; `sallet-occur' is unusably slow. Don't use it.
                     ;; `icicle-occur' is unusably slow. Don't use it.
                     (t #'my-occur-wild-spaces))
  "Function for searching with an overview.
Choices: helm-swoop helm-occur swiper swiper-isearch ido-occur sallet-occur
icicle-occur occur my-occur-wild-spaces")
(when my-use-evil-p
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "s") my-swoop-fn)))

(defvar my-use-lispy-p (memq my-curr-computer '(wild-dog
                                                work-laptop-2019
                                                mac-mini-m1-2021
                                                work-laptop-mac))
  "Whether to use lispy or not.
Lispy pulls in ivy as a dependency so avoiding on slow computers.")

(defvar my-native-json-p (functionp 'json-parse-string)
  "True if Emacs was compiled with native json support.
Using the Jansson C library.
This should speed up lsp related modes.")

(defvar my-native-comp-p (and (fboundp 'native-comp-available-p)
                              (native-comp-available-p))
  "True if Emacs is using native elsip compliation.
aka gccemacs.
In master branch now. Was on git branch: feature/native-comp.")

;;;----------------------------------------------------------------------------
;;; Packages. NOTE: with the conversion to git submodules, most of the old
;;; package code is moved to ~/.emacs.d/notElpa/mine/my-package-stuff.el
;;;----------------------------------------------------------------------------

;; not using package.el, but just in case I do...
(with-eval-after-load 'package
  ;; enable the quickstart feature. concats all the autoload.el files for
  ;; faster load.  May need to manually call `package-quickstart-refresh'
  ;; to generate the concatenated file.
  (setq package-quickstart t))

(push "~/.emacs.d/notElpa/" load-path) ; stores elisp files that are
                                       ; not "packages".
(push "~/.emacs.d/notElpa/mine/" load-path)
(setq custom-theme-directory "~/.emacs.d/notElpa/themes/") ;color themes.
;; some themes require helper files so add themes dir to load-path.
(push custom-theme-directory load-path)

(push "~/.emacs.d/notElpa/themes/replace-colorthemes/" custom-theme-load-path)

;; (setq package-quickstart t) ; pre-generates a giant autoloads file


(defvar my-dyn-modules-p (and (functionp 'module-load) ; should be t
                              module-file-suffix)      ; should be non-nil
  "Non-nil if Emacs supports dynamic modules.")

(defvar native-line-numbers-p (boundp 'display-line-numbers)
  "Non-nil if Emacs supports native line number display.")


;; (defvar has-nodejs-p
;;   (memq my-curr-computer '(wild-dog)))

;; TODO: set up clang on more machines.
(defvar my-has-clang-p (memq my-curr-computer
                             '(;; wild-dog
                               hp-tower-2009)))

(defvar my-install-slime-p (memq my-curr-computer
                                 '(wild-dog
                                   utilite
                                   hp-tower-2009
                                   a-laptop-faster
                                   work-laptop-2019
                                   mac-mini-m1-2021)))

(defvar my-install-slime-company-p (and my-install-slime-p
                                        (not (version< emacs-version
                                                       "24.4"))))

(defvar my-has-rg-exe-p (memq my-curr-computer '(wild-dog
                                                 work-laptop-2019
                                                 mac-mini-m1-2021
                                                 work-laptop-mac))
  "Non-nil if rg executable is installed.")
(defvar my-install-rg-p (not (version< emacs-version "24.4"))
  "Whether to install the `rg' package from melpa.")


;;; add melpa.
(with-eval-after-load 'package
  (push '("melpa" . "https://melpa.org/packages/") package-archives))

;; ;; temporarily stop using ssl (https) in the package archive urls.
;; ;; https causes a hang on ms-windows when calling `list-packages'.
;; (setq package-archives
;;       '(("gnu" . "http://elpa.gnu.org/packages/")
;;         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
;;         ("melpa" . "http://melpa.org/packages/")))



;;;----------------------------------------------------------------------------
;;; my-modules
;;;----------------------------------------------------------------------------
(autoload #'my-byte-compile-all-modules "my-modules" nil t)
;; (autoload #'my-byte-compile-module "my-modules" nil t)
(autoload #'my-byte-compile-all-notElpa-folders "my-modules" nil t)
(autoload #'my-setup-all-upstream-remotes-if-missing "my-modules" nil t)
(autoload #'my-fetch-all-upstream-remotes "my-modules" nil t)
(autoload #'my-list-modules-with-upstream-code-to-merge "my-modules" nil t)


;;;----------------------------------------------------------------------------
;;; compat
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/compat.el" load-path)


;;;----------------------------------------------------------------------------
;;; async
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/emacs-async" load-path)
(autoload #'async-start-process "async" nil t)
(autoload #'async-start "async" nil t)
(autoload #'dired-async-mode "dired-async" nil t)
(autoload #'dired-async-do-copy "dired-async" nil t)
(autoload #'dired-async-do-symlink "dired-async" nil t)
(autoload #'dired-async-do-hardlink "dired-async" nil t)
(autoload #'dired-async-do-rename "dired-async" nil t)
(autoload #'async-byte-recompile-directory "async-bytecomp" nil t)
(autoload #'async-bytecomp-package-mode "async-bytecomp" nil t)
(autoload #'async-byte-compile-file "async-bytecomp" nil t)


;;;----------------------------------------------------------------------------
;;; dash
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/dash.el" load-path)
;; NOTE: contains dash-functional.el which is a separate pacakge on melpa.

;;;----------------------------------------------------------------------------
;; bug-hunter
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/elisp-bug-hunter" load-path)
(autoload #'bug-hunter-file "bug-hunter" nil t)
(autoload #'bug-hunter-init-file "bug-hunter" nil t)

;;;----------------------------------------------------------------------------
;;; s
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/s.el" load-path)

;;;----------------------------------------------------------------------------
;;; f
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/f.el" load-path)

;;;----------------------------------------------------------------------------
;;; num3-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/num3-mode" load-path)
(autoload #'num3-mode "num3-mode" nil t)
(autoload #'global-num3-mode "num3-mode" nil t)

;;;----------------------------------------------------------------------------
;;; w32-send-sys codes. Operating system commands. MS Windows only.
;;;----------------------------------------------------------------------------
(when (eq system-type 'windows-nt)
  (let ((my-w32-actions
         '((resize . 61440)
           (move . 61456)
           (min . 61472)
           (max . 61488)
           (next-window . 61504)
           (prev-window . 61520)
           (close-window . 61536)
           (vert-scroll . 61552)
           (horizontal-scroll . 61568)
           (mouse-menu . 61584)
           (activate-menubar . 61696)
           (arrange . 61712)
           (restore-curr-frame . 61728)
           (simulate-start-btn . 61744)
           (screen-saver . 61760)
           (hotkey . 61776))))
    (defun my-w32-get-code (action)
      "Get the numeric code from the action symbol."
      (cdr (assoc action my-w32-actions)))
    (defun my-w32-get-action (code)
      "Get the action symbol from the numeric code."
      (car (cl-rassoc code my-w32-actions)))
    (defun my-w32-run (action)
      "Executes a w32 action."
      (let ((code (my-w32-get-code action)))
        (w32-send-sys-command code)))))

;;;----------------------------------------------------------------------------
;;; key-chord
;;;----------------------------------------------------------------------------
(autoload #'key-chord-mode "key-chord" nil t)
(autoload #'key-chord-define-global "key-chord" nil t)
(autoload #'key-chord-define-local "key-chord" nil t)
(autoload #'key-chord-define "key-chord" nil t)

;; NOTE: "fj" chord slows down movement when in visual mode when pressing "j"
;;       since it is looking for the chord.

(when my-use-evil-p
  (require 'key-chord)

  (with-eval-after-load 'key-chord
    (setq key-chord-two-keys-delay 0.2) ; lower to reduce lag when pressing a
                                        ; key of a chord.
    (setq key-chord-one-key-delay 0.4))

  (with-eval-after-load "helm" ;; TODO: see if this is correct in the latest
    ;; versions of helm.
    ;; must be in eval-after-load so `helm-map' is defined
    (key-chord-define helm-map "fj" #'helm-keyboard-quit))

  ;; (with-eval-after-load "ivy"
  ;;   (key-chord-define ivy-mode-map "fj" #'keyboard-escape-quit))

  (with-eval-after-load 'evil
    ;; must be in eval-after-load so key maps are defined.
    (key-chord-define evil-insert-state-map "fj" #'evil-normal-state)
    (key-chord-define evil-visual-state-map "fj" #'evil-exit-visual-state))

  ;; (with-eval-after-load "lisp-mode"
  ;;   (when nil ;; trying lispy
  ;;     ;; TODO: make sure `hydra-paredit/body' still works after autoload
  ;;     ;;       changes.
  ;;     (key-chord-define lisp-mode-shared-map "df" #'hydra-paredit/body)))

  ;; (with-eval-after-load "smartparens"
  ;;   (load "~/.emacs.d/notElpa/mine/my-hydras.el")
  ;;   (key-chord-define smartparens-mode-map "df" #'hydra-smartparens/body))


  ;; (key-chord-mode 1) ;; autoloaded function

  ;; TODO: use an alternative way to suppress the message. So I don't have to
  ;;       manually re-sync this definition with the latest version of fn
  ;;       `key-chord-mode'.
  ;; NOTE: using lambda instead defun to avoid a junk method that is not useful
  ;; after start-up (just want to avoid the msg at start-up).
  (let ((my-key-chord-mode-fn
         (lambda (arg)
           "Alternative to fn `key-chord-mode'. To suppress the on message."
           (interactive "P")
           (setq key-chord-mode (if arg
                                    (> (prefix-numeric-value arg) 0)
                                  (not key-chord-mode)))
           (cond (key-chord-mode
                  (setq input-method-function 'key-chord-input-method)
                  ;; (message "Key Chord mode on")
                  )
                 (t
                  (setq input-method-function nil)
                  (message "Key Chord mode off"))))))
    ;; turn on key-chord using a function that does not spam a message when
    ;; turning on.
    (funcall my-key-chord-mode-fn 1)))


;;;----------------------------------------------------------------------------
;;; cursor
;;;----------------------------------------------------------------------------
(setq-default cursor-type '(bar . 2))
;; (custom-set-faces
;;  '(cursor ((t (:background "cyan")))))
(setq x-stretch-cursor t) ;; stretch box cursor around a tab \t
(setq-default cursor-in-non-selected-windows nil)
(blink-cursor-mode 0)

(when (and my-use-evil-p
           my-graphic-p)
  ;; set the evil cursor styles. colors not specified yet. colors will be
  ;; specified later in the theme settings. But I want the same cursor styles
  ;; of bar/box/hollow/etc regardless of the color theme.
  (setq evil-emacs-state-cursor    '(bar))
  (setq evil-normal-state-cursor   '(hollow))
  (setq evil-insert-state-cursor   '(bar))
  (setq evil-visual-state-cursor   '(hollow))
  (setq evil-operator-state-cursor '(box))
  (setq evil-replace-state-cursor  '(hbar))
  (setq evil-motion-state-cursor   '(box)))



;;;----------------------------------------------------------------------------
;;; special cursor handling for light/dark backgrounds.
;;;----------------------------------------------------------------------------
(autoload #'my-cursor-light-bg "my-color-theme-mods" nil t)
(autoload #'my-cursor-dark-bg "my-color-theme-mods" nil t)


;;;----------------------------------------------------------------------------
;;; evil-leader
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/evil-leader" load-path)
(autoload #'global-evil-leader-mode "evil-leader" nil t)
(autoload #'evil-leader-mode "evil-leader" nil t)
(autoload #'evil-leader/set-key "evil-leader" nil t)
(autoload #'evil-leader/set-key-for-mode "evil-leader" nil t)

;;;----------------------------------------------------------------------------
;;; goto-chg. dependency of evil.
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/goto-chg" load-path)
(autoload #'goto-last-change "goto-chg" nil t)
(autoload #'goto-last-change-reverse "goto-chg" nil t)

;;;----------------------------------------------------------------------------
;;; evil
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/evil" load-path)
;; NOTE: goto-chg and undo-tree are under /evil/lib/. They are separate
;; packages in melpa.
(push "~/.emacs.d/notElpa/evil/lib/" load-path)
(autoload #'evil-mode "evil-core" nil t)
(autoload #'evil-define-key "evil-core" nil t)

(with-eval-after-load 'evil
  ;; dependency on undo-tree was removed recently from evil. do not require.
  ;; (require 'undo-tree) ; stored in /notElpa/evil/lib


  (declare-function undo-redo 'suppress) ; silence byte-compiler on emacs<28
  (when (fboundp #'undo-redo) ; emacs 28+
    ;; (setq evil-undo-system 'undo-redo)

    ;; this is a weird fn that is used to set `evil-undo-system' via custom.
    ;; TODO: figure out how to properly set `evil-undo-system'.
    (funcall (lambda (sym value)
               (evil-set-undo-system value)
               (set-default sym value))
             'evil-undo-system
             'undo-redo))

  (setq evil-flash-delay 1)

  (when nil ;; don't do this for now.
    ;; make "w" treat words separated by _ and - as 1 word. Like in vim.
    ;; `modify-syntax-entry' is buffer local, so attach to a hook.
    (defun my-setup-w-for-evil ()
      (interactive)
      (modify-syntax-entry ?_ "w")
      (modify-syntax-entry ?- "w"))
    (add-hook 'prog-mode-hook #'my-setup-w-for-evil)
    (add-hook 'text-mode-hook #'my-setup-w-for-evil))

  ;; unset some keys. It seems other modes have trouble overriding them when
  ;; it's set in evil?
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-normal-state-map (kbd "M-,") nil)
  ;; unbind the "K" key. I press it accidentally all the time.
  (define-key evil-normal-state-map (kbd "K") nil)
  (define-key evil-visual-state-map (kbd "K") nil)
  (define-key evil-motion-state-map (kbd "K") nil)

  (define-key evil-normal-state-map (kbd "C-v") #'scroll-up-command)
  (define-key evil-motion-state-map (kbd "C-v") #'scroll-up-command)

  (define-key evil-visual-state-map (kbd "\\") #'indent-region)
  (define-key evil-visual-state-map (kbd "<tab>") #'indent-region)
  ;; TODO: rebind evil-jump-forward from <tab> to something else.


  ;; replace teh default "M" behavior which no longer goes to the middle of the
  ;; lines. TODO: get "M" to work with visual line mode via uppercase "V".
  ;; (define-key evil-normal-state-map (kbd "M") #'my-evil-goto-page-mid)
  ;; (define-key evil-visual-state-map (kbd "M") #'my-evil-goto-page-mid)

  (when my-graphic-p
    ;;prevent minibuffer spam when switching modes.
    ;;Cursor style/color is sufficient to determine mode.
    (setq evil-insert-state-message nil)
    (setq evil-emacs-state-message nil)
    (setq evil-visual-state-message nil)
    (setq evil-motion-state-message nil)
    (setq evil-normal-state-message nil)
    (setq evil-operator-state-message nil)
    (setq evil-replace-state-message nil)

    ;;Don't waste mode line space displaying Evil-states.
    ;;Cursor style/color is sufficient to determine mode.
    ;;but if in a terminal without cursor styles then allow it to exist.
    (setq evil-mode-line-format nil))


  (setq evil-default-cursor t)

  ;;(add-to-list 'load-path "~/.emacs.d/evil") ; only without ELPA/el-get
  ;; (require 'evil)

  ;; TODO: look into replacing evil-leader with 1 of the following:
  ;;       https://github.com/justbur/emacs-bind-map
  ;;       https://github.com/noctuid/general.el
  (require 'evil-leader)
  (global-evil-leader-mode)
  ;; (evil-mode 1)


  ;;(define-key <keymap> key 'function)


  ;; Make j/k movement keys go up/down across wrapped lines.
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>")
    #'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>")
    #'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>")
    #'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>")
    #'evil-previous-visual-line)
  ;;(setq-default evil-cross-lines t) ;; Make horizontal movement cross lines


  ;; make C-k kill to the end of the line in insert mode.
  (define-key evil-insert-state-map (kbd "C-k") #'kill-line)


  ;; Map for Esc.
  ;; The key-chord package causes lag when a key of the chord is pressed.
  ;; So using the the built-in control chords which are fast. Better than the
  ;; awkward C-[ default.
  ;; (evil-define-key 'insert global-map (kbd "C-n") #'evil-normal-state)
  ;; (evil-define-key 'visual global-map (kbd "C-n") #'evil-exit-visual-state)


  (defadvice evil-end-of-line (after do-not-highlight-newline)
    "When in visual mode, press $ to go to the end of the line.
Minus the newline char."
    (when (evil-visual-state-p)
      (evil-backward-char)))
  (ad-activate 'evil-end-of-line)


  ;;leader keys
  (evil-leader/set-leader ",")
  ;; (evil-leader/set-key "w" #'other-window)
  (evil-leader/set-key "q" #'balance-windows)
  (evil-leader/set-key "x" #'maximize-window)
  (evil-leader/set-key "," #'delete-other-windows)
  (evil-leader/set-key "d" #'delete-window)
  (defun my-quit-window ()
    "Kill the buffer and close the window if one was opened specifically for
that buffer."
    (interactive)
    (quit-window t))
  (evil-leader/set-key "k" #'my-quit-window) ;; #'kill-this-buffer
  (evil-leader/set-key "c" #'quit-window) ; buffer left alive
  (defun my-shrink-window-horizontally ()
    (interactive)
    (shrink-window-horizontally 15))
  (defun my-enlarge-window-horizontally ()
    (interactive)
    (enlarge-window-horizontally 15))
  (evil-leader/set-key "<" #'my-shrink-window-horizontally)
  (evil-leader/set-key ">" #'my-enlarge-window-horizontally)
  (evil-leader/set-key "v" #'evil-visual-block)
  ;; (evil-leader/set-key "j" (lambda ()
  ;;                            (interactive)
  ;;                            (shrink-window 10)))
  ;; (evil-leader/set-key "k" (lambda ()
  ;;                            (interactive)
  ;;                            (enlarge-window 10)))

  ;;TODO: look into equivalent resizing for non-Windows machines.
  (when (eq system-type 'windows-nt)
    ;;`maxp' can get out of sync. Hit <Leader>f a 2nd time to re-sync.
    (let ((maxp nil))
      (defun my-toggle-frame-max ()
        "Closure over `maxp'."
        (interactive)
        (let ((flag (if maxp
                        'restore-curr-frame
                      'max)))
          (my-w32-run flag)
          ;; toggle bool flag
          (setq maxp (not maxp)))))

    (evil-leader/set-key "f" #'my-toggle-frame-max)))

;; keeping evil turned off by default now.
;; Enable evil explicitly for certain modes or file types.
;; (add-hook 'prog-mode-hook #'evil-local-mode)

(when my-use-evil-p
  ;; require evil in an attempt to solve the issue with `evil-visual-char' and
  ;; `evil-define-key' causing issues with their `declare-function' statements.
  (require 'evil)
  (evil-mode 1)) ;; enable globally



;;;----------------------------------------------------------------------------
;;; evil-snipe
;;;----------------------------------------------------------------------------
;; (setq evil-snipe-enable-highlight nil)
;; (setq evil-snipe-enable-incremental-highlight nil)
;; (setq evil-snipe-scope 'visible)
;; (setq evil-snipe-repeat-scope 'visible)

;; (require 'evil-snipe)
;; (global-evil-snipe-mode 1)


;;;----------------------------------------------------------------------------
;;; evil-god-state
;;;----------------------------------------------------------------------------
;; (evil-define-key 'normal global-map (kbd "\\") 'evil-execute-in-god-state)
;; (evil-define-key 'motion global-map (kbd "\\") 'evil-execute-in-god-state)
;; (evil-define-key 'god global-map [escape] 'evil-god-state-bail)
;; ;;(evil-leader/set-key "," 'evil-execute-in-god-state)

;; ;; (add-hook 'evil-god-start-hook (lambda ()
;; ;;                                  (diminish 'god-local-mode)))
;; ;; (add-hook 'evil-god-stop-hook (lambda ()
;; ;;                                 (diminish-undo 'god-local-mode)))

;;;----------------------------------------------------------------------------
;;; evil-surround
;;;----------------------------------------------------------------------------
;; (require 'evil-surround)
;; (global-evil-surround-mode 1)


;;;----------------------------------------------------------------------------
;;; font
;;;----------------------------------------------------------------------------
(autoload #'my-set-frame-font-ivy "my-font-stuff" nil t)
(global-set-key (kbd "<f5>") #'my-set-frame-font-ivy)


;;;----------------------------------------------------------------------------
;;; inc/dec font size
;;;----------------------------------------------------------------------------
(autoload #'my-change-font-size-bigger "my-font-stuff" nil t)
(autoload #'my-change-font-size-smaller "my-font-stuff" nil t)

(global-set-key (kbd "M-=") #'my-change-font-size-bigger)
(global-set-key (kbd "M--") #'my-change-font-size-smaller)

;;;----------------------------------------------------------------------------
;;; Color theme stuff.
;;;----------------------------------------------------------------------------
;; In Emacs 27+ make `custom-theme-set-faces' work immediately.
(setq custom--inhibit-theme-enable nil)

;;TODO: implement a way to undo color settings made outside the theme
;;      definition. Use custom-theme-set-faces to set the colors/styles so they
;;      are rolled back when switching/disabling themes.
(defadvice load-theme (before disable-before-load)
  "Disable any loaded themes before enabling a new theme.
This prevents overlapping themes; something I would rarely want."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

;; (defadvice load-theme (before capture-theme)
;;   "Capture the theme in a global var."
;;   (setq my-curr-theme theme))

(setq custom-safe-themes t) ;; Disable the confirmation to load themes.
;; (defadvice load-theme (around disable-security)
;;   "Disable the confirmation to load themes."
;;     (let ((no-confirm t))
;;       ad-do-it))

(ad-activate 'load-theme)


(defvar my-themes '(charcoal dark-bright cmd ultimate bluey)
  "Themes I created.")

;; vim charcoal: hi Normal guifg=#ADC299 guibg=#35352B "*
(defvar mayan-smoke "#F4F4E8" "Background color from the Vim theme.")
(defvar my-charcoal "#35352B" "Experimental dark background color.")
(defvar my-peach "#fff9F5"
  "Re-create what peachpuff looked like on an old monitor.")
(defvar my-ultimate "#fffeFa"
  "Re-create what my \"ultimate\" color looked like on an old monitor.")



;; programmatically call a function as if a prefix arg C-u was used.
;; (let ((current-prefix-arg '(4)))
;;   (call-interactively #'next-line))


(progn ; theme changing stuff.
  (autoload #'my-cycle-theme "my-load-theme" nil t)
  (autoload #'my-force-light-bg "my-load-theme" nil t)
  (autoload #'my-cycle-light-bg "my-load-theme" nil t)
  (autoload #'my-cycle-light-bg-forward "my-load-theme" nil t)
  (autoload #'my-cycle-light-bg-backward "my-load-theme" nil t)

  ;; (autoload #'my-handle-weird-theme-setups "my-load-theme" nil t)
  (autoload #'my-load-theme-wrapper "my-load-theme" nil t)
  (autoload #'my-counsel-load-theme "my-load-theme" nil t)
  (if my-use-ivy-p
      (global-set-key (kbd "<f9>") #'my-counsel-load-theme)
    (global-set-key (kbd "<f9>") #'my-load-theme-wrapper))

  ;; after calling `counsel-load-theme', call `ivy-occur' to see a UI
  ;; for theme selection. This is better than `my-cycle-theme' becuase it
  ;; does not redunantly call (custom-available-themes) and you can skip
  ;; around.
  (global-set-key (kbd "<f10>") #'my-counsel-load-theme)
  (global-set-key (kbd "<f12>") #'my-cycle-light-bg-forward)
  (global-set-key (kbd "S-<f12>") #'my-cycle-light-bg-backward))


(autoload #'my-load-theme-make-bold-like-zenburn "my-load-theme" nil t)

(autoload #'my-toggle-inverse-video "my-load-theme" nil t)
(autoload #'my-load-theme-inverse "my-load-theme" nil t)


(let ((file "my-color-theme-mods"))
  (autoload #'my-rainbow-parens-dark-bg file nil t)
  (autoload #'my-rainbow-parens-dark-bg-bold file nil t)
  (autoload #'my-rainbow-parens-light-bg file nil t)
  (autoload #'my-rainbow-parens-light-bg2 file nil t)
  (autoload #'my-rainbow-parens-light-bg3 file nil t)

  (autoload #'my-color-tao-yin file nil t)
  (autoload #'my-color-base16-decaf file nil t)
  (autoload #'my-color-grandshell file nil t)
  (autoload #'my-color-zenburn file nil t)
  (autoload #'my-color-github file nil t)
  (autoload #'my-color-badger file nil t)
  (autoload #'my-color-gruvbox file nil t)
  (autoload #'my-color-gruvbox-dark file nil t)
  (autoload #'my-color-monokai file nil t)
  (autoload #'my-color-tommyh file nil t)
  (autoload #'my-color-default file nil t)
  (autoload #'my-color-default-fancy file nil t)
  (autoload #'my-color-gandalf file nil t)
  (autoload #'my-color-leuven file nil t)
  (autoload #'my-color-dichromacy file nil t)
  (autoload #'my-color-firebelly file nil t)
  (autoload #'my-color-molokai file nil t)
  (autoload #'my-color-majapahit file nil t)
  (autoload #'my-color-deeper-blue file nil t)
  (autoload #'my-color-kosmos file nil t)
  (autoload #'my-color-niflheim file nil t)
  (autoload #'my-color-spacemacs-light file nil t)
  (autoload #'my-color-tango-dark file nil t)
  (autoload #'my-color-sunburn file nil t)
  (autoload #'my-color-overcast file nil t)
  (autoload #'my-color-warm-night file nil t)
  (autoload #'my-color-avk-daylight file nil t)
  (autoload #'my-color-moe-light file nil t)
  (autoload #'my-color-moe-dark file nil t))


(when my-graphic-p ;; transparency stuff
  (autoload #'my-change-alpha-more-solid "my-misc" nil t)
  (autoload #'my-change-alpha-less-solid "my-misc" nil t)

  (global-set-key (kbd "C-M-=") #'my-change-alpha-more-solid)
  (global-set-key (kbd "C-M--") #'my-change-alpha-less-solid))

;;;----------------------------------------------------------------------------
;;; theme of the week and corresponding settings. This may change often.
;;;----------------------------------------------------------------------------

;; Avoid resizing the GUI frame when font changes.
;; see https://old.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn
;; _fast/
(setq frame-inhibit-implied-resize t)

;; (when my-graphic-p ;; this isn't true for emacs daemon!
;;   (my-color-zenburn))
(cond
 ((eq my-curr-computer 'work-laptop-mac)
  (load-theme 'charcoal t)
  (push
   '(font . "-*-Menlo-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
   default-frame-alist))

 ((eq my-curr-computer 'mac-mini-m1-2021)
  (load-theme 'charcoal t)
  ;; (custom-theme-set-faces
  ;;  'ultimate
  ;;  `(default ((t :background "ivory3"))))
  (push
   ;; '(font . "-*-Menlo-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
   ;; '(font . "-*-JetBrains Mono NL-light-normal-normal-*-15-*-*-*-m-0-iso10646-1")
   ;; '(font . "-*-Ubuntu Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
   ;; '(font . "-*-Ubuntu Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
   ;; '(font . "-*-Iosevka-regular-normal-normal-*-17-*-*-*-m-0-iso10646-1")
   '(font . "-*-Iosevka-light-normal-normal-*-17-*-*-*-m-0-iso10646-1")
   ;; '(font . "-*-Unifont-normal-normal-normal-*-19-*-*-*-p-0-iso10646-1")
   ;; '(font . "-*-Unifont-bold-normal-normal-*-19-*-*-*-p-0-iso10646-1")
   default-frame-alist))

 ((eq my-curr-computer 'wild-dog)
  (load-theme 'charcoal t)

  ;; faster than `set-frame-font' for setting the font?
  ;; see https://old.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_da
  ;; mn_fast/
  (push
   '(font . "-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-9")
   default-frame-alist))

 ((eq my-curr-computer 'work-laptop-2019)
  (load-theme 'charcoal t)
  ;; (set-background-color "#E5E1C3")
  ;; (custom-theme-set-faces
  ;;  'ultimate
  ;;  `(default ((t :background "ivory3"))))
  (when my-graphic-p
    (push
     '(font
       .
       "-raster-Fixedsys-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
       ;; "-raster-Terminal-normal-normal-normal-mono-18-*-*-*-c-*-ms-oemlatin"
       ;; "-raster-Terminus-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
       ;; "-raster-Terminus-bold-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
       ;; "-raster-Terminus-bold-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
       ;; "-raster-Terminus-bold-normal-normal-mono-14-*-*-*-c-*-iso8859-1"
       ;; "-outline-Iosevka Medium-medium-normal-normal-mono-14-*-*-*-c-*-iso10646-1"
       ;; "-outline-Lucida Console-normal-normal-normal-mono-13-*-*-*-c-*-iso10646-1"
       ;; "-outline-Ubuntu Mono-bold-normal-normal-mono-16-*-*-*-c-*-iso10646-1"
       ;; "-outline-Ubuntu Mono-bold-normal-normal-mono-15-*-*-*-c-*-iso10646-1"
       ;; "-outline-Ubuntu Mono-normal-normal-normal-mono-14-*-*-*-c-*-iso10646-1"
       ;; "-outline-JetBrains Mono NL ExtraBold-extrabold-normal-normal-mono-13-*-*-*-c-*-iso10646-1"
       ;; "-outline-JetBrains Mono NL-normal-normal-normal-mono-13-*-*-*-c-*-iso10646-1"
       )
     default-frame-alist)
    ;; (set-frame-font
    ;;  "-raster-Dina-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1")
    ))

 ((eq my-curr-computer 'leyna-laptop)
  (load-theme 'charcoal t)
  (set-frame-font
   "-raster-Terminal-normal-normal-normal-mono-18-*-*-*-c-*-ms-oemlatin"))

 ((eq my-curr-computer 'a-laptop-old)
  (load-theme 'charcoal t)
  (set-frame-font
   "-raster-Fixedsys-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"))

 ((eq my-curr-computer 'hp-tower-2009)
  (load-theme 'charcoal t))

 ((eq my-curr-computer 'a-laptop-faster)
  (load-theme 'charcoal t)
  (set-frame-font
   "-unknown-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))

 ;; unknown windows computer.
 ((and (null my-curr-computer)
       (eq system-type 'windows-nt))
  (load-theme 'charcoal t)
  (set-frame-font
   "-raster-Fixedsys-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1")))


;;;----------------------------------------------------------------------------
;;; Font cycling stuff.
;;;----------------------------------------------------------------------------
(autoload #'my-cycle-font-forward "my-font-cycle" nil t)
(autoload #'my-cycle-font-backward "my-font-cycle" nil t)
(autoload #'my-select-font "my-font-cycle" nil t)

(global-set-key (kbd "<f8>") #'my-cycle-font-forward)
(global-set-key (kbd "S-<f8>") #'my-cycle-font-backward)


;;;----------------------------------------------------------------------------
;;; Recursively byte-compile every .el file
;;;----------------------------------------------------------------------------
;;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)


(defun my-delete-elc-files (dir-str)
  "Delete all elc files in folder DIR-STR."
  (let ((elc-files (directory-files-recursively
                    dir-str
                    "\.elc$")))
    ;; Optionally, on linux manually delete. First verify file list with:
    ;;   find . -name "*.elc" -type f
    ;; then run:
    ;;   find . -name "*.elc" -type f -delete
    (cl-loop for f in elc-files
             do
             (delete-file f))))

(defun my-byte-compile-curr-dir ()
  "Byte compile all elisp files in the current directory."
  (interactive)
  ;; delete all "elc" files first. Sometimes there are issues where an
  ;; elc is loaded which breaks compliation of an el file. Rare, but makes an
  ;; impossible to hunt down issue when it happens.
  (my-delete-elc-files default-directory) ; current directory

  (byte-recompile-directory
   default-directory   ; current directory
   0                   ; 0 means compile .el files if .elc is missing.
   t) ; t means force re-compile even if the .elc is up-to-date. May be
  )



;;;----------------------------------------------------------------------------
;;; sly
;;;----------------------------------------------------------------------------
(defvar my-use-sly nil
  "Whether I'm using sly or not. Used to avoid adding folders to the load-path
with duplicate bundled libs in Sly and SLIME.")

(when my-use-sly ; avoid duplicate bundled libraries also in SLIME.
  (push "~/.emacs.d/notElpa/sly" load-path)
  (push "~/.emacs.d/notElpa/sly/contrib" load-path)
  (push "~/.emacs.d/notElpa/sly/lib" load-path))


(when my-use-sly
  ;; (when (eq my-curr-computer 'work-laptop)
  ;;   (setq inferior-lisp-program
  ;;         "C:/Users/mtz/programs/ccl-1.10-windowsx86/ccl/wx86cl64"))

  (require 'sly-autoloads)
  (setq inferior-lisp-program "sbcl"))


;;;----------------------------------------------------------------------------
;;; pos-tip
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/pos-tip" load-path)
(autoload #'pos-tip-show "pos-tip" nil t)

;;;----------------------------------------------------------------------------
;;; macrostep
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/macrostep" load-path)
(autoload #'macrostep-c-mode-hook "macrostep-c" nil t)
(autoload #'macrostep-mode "macrostep" nil t)
(autoload #'macrostep-expand "macrostep" nil t)

;;;----------------------------------------------------------------------------
;;; SLIME
;;;----------------------------------------------------------------------------
(unless my-use-sly ; avoid conflicts with SLIME when testing out sly.
  (push "~/.emacs.d/notElpa/slime" load-path)
  (push "~/.emacs.d/notElpa/slime/contrib" load-path)
  ;; don't add /slime/lib/. it's added conditonally in slime.el if needed.
  ;; (push "~/.emacs.d/notElpa/slime/lib" load-path)

  (autoload #'slime "slime" nil t)
  (autoload #'slime-mode "slime" nil t)
  (autoload #'slime-connect "slime" nil t)
  (autoload #'slime-selector "slime" nil t)
  (autoload #'slime-selector "slime" nil t)
  (autoload #'slime-lisp-mode-hook "slime" nil t)
  ;; (autoload #'slime-sheme "slime-scheme" nil t)
  (autoload #'slime-setup "slime" nil t)

  ;; this line taken from slime-autoloads.el. Makes things work?
  (add-hook 'lisp-mode-hook 'slime-lisp-mode-hook))

;; (require 'slime-autoloads)
(with-eval-after-load 'slime

  ;; (when my-use-evil-p
  ;;   ;; use emacs bindings in the repl. It's the only way i can get C-n and
  ;;   ;; C-p to work for slimes built in autocompletion.
  ;;   (add-to-list 'evil-buffer-regexps '("\\*slime-repl" . emacs)))


  (let ((lst '(slime-fancy
               ;; slime-company
               slime-banner
               slime-indentation
               ;; slime-highlight-edits
               )))
    (when my-install-slime-company-p
      (push 'slime-company lst))
    (slime-setup lst))
  (setq slime-complete-symbol*-fancy t)
  ;; `slime-complete-symbol-function' is obsolete. comment out and use
  ;; `slime-completion-at-point-functions' instead.
  ;; (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (push 'slime-fuzzy-complete-symbol
        slime-completion-at-point-functions)

  (when my-use-evil-p
    ;; ;; override evil's binding of M-. when using slime
    ;; (define-key slime-mode-map (kbd "M-.") 'slime-edit-definition)

    ;; override evil's binding of M-. when using slime
    (evil-define-key 'normal slime-mode-map (kbd "M-.")
      #'slime-edit-definition)
    (evil-define-key 'normal slime-repl-mode-map (kbd "M-.")
      #'slime-edit-definition)

    ;; (progn
    ;;   ;; For evil, attempting to get C-n, C-p to move selection in slimes
    ;;   ;; fuzzy completions window.
    ;;   (evil-define-key 'insert slime-fuzzy-completions-mode-map
    ;;     (kbd "C-n") #'slime-fuzzy-next)
    ;;   (evil-define-key 'insert slime-target-buffer-fuzzy-completions-map
    ;;     (kbd "C-n") #'slime-fuzzy-next)
    ;;   (evil-define-key 'insert slime-fuzzy-completions-mode-map
    ;;     (kbd "C-p") #'slime-fuzzy-prev)
    ;;   (evil-define-key 'insert slime-target-buffer-fuzzy-completions-map
    ;;     (kbd "C-p") #'slime-fuzzy-prev)
    ;;   (define-key slime-fuzzy-completions-mode-map
    ;;     (kbd "C-n") #'slime-fuzzy-next)
    ;;   (define-key slime-target-buffer-fuzzy-completions-map
    ;;     (kbd "C-n") #'slime-fuzzy-next))
    )

  ;; disable the banner header line in repl.
  ;; TODO: get rid of the date string that replaces it too.
  (setq slime-header-line-p nil)
  (setq slime-startup-animation nil) ; disable text animation on repl start.

  ;; (require 's)
  ;; (setq slime-words-of-encouragement
  ;;       (let ((words '())) ;;hidden
  ;;         (dolist (w slime-words-of-encouragement)
  ;;           (when (s-contains? "REPL" w)
  ;;             (setq words (cons w words))))
  ;;         words))


  ;; redefine `slime-startup-message' to work how I want
  ;; (defun slime-startup-message ()
  ;;   (when slime-header-line-p
  ;;     (setq header-line-format
  ;;           (format "%s  Port: %s  Pid: %s"
  ;;                   (slime-lisp-implementation-type)
  ;;                   (slime-connection-port (slime-connection))
  ;;                   (slime-pid))))
  ;;   (when (zerop (buffer-size))
  ;;     (let* ((orig-welcome (concat "; SLIME " slime-version))
  ;;            (welcome (concat orig-welcome "   "
  ;;                             ;; (slime-random-words-of-encouragement)
  ;;                             (nth 5 slime-words-of-encouragement))))
  ;;       (if slime-startup-animation
  ;;           (animate-string welcome 0 0)
  ;;         (insert welcome)))))

  ;; restore deleted phrase. Also make it the only one.
  (setq slime-words-of-encouragement
        '("Take this REPL, brother, and may it serve you well."))

  (cond
   ((eq my-curr-computer 'mac-mini-m1-2021)
    (setq slime-default-lisp 'sbcl
          slime-lisp-implementations '((sbcl ("/usr/local/bin/sbcl")))))

   ((eq my-curr-computer 'wild-dog)
    (setq slime-default-lisp 'sbcl
          slime-lisp-implementations '((ccl ("~/proj/ccl/lx86cl64"))
                                       (sbcl ("sbcl")))))

   ((eq my-curr-computer 'work-laptop)
    (setq slime-default-lisp 'ccl)
    (setq slime-lisp-implementations
          '((ccl
             ("C:/Users/mtz/programs/ccl-1.11.5-windowsx86/ccl/wx86cl64"))
            (sbcl
             ("C:/Program Files/Steel Bank Common Lisp/1.2.15/sbcl.exe"))
            (ecl ("C:/Users/mtz/programs/ecl/ecl.exe"))
            ;; clisp is just a fake example for now.
            (clisp ("~/path/to/clisp-2.49/clisp" "-modern")))))

   ((eq my-curr-computer 'work-laptop-2019)
    (setq slime-default-lisp 'sbcl)
    (setq slime-lisp-implementations
          '((ccl
             ("c:/Users/mtz/programs/ccl-1.12.1-windowsx86/ccl/wx86cl64.exe"))
            (sbcl ("sbcl")))))


   ((eq my-curr-computer 'utilite)
    (setq slime-default-lisp 'ccl
          slime-lisp-implementations '((ccl ("armcl")))))

   ((eq my-curr-computer 'hp-tower-2009)
    (setq slime-default-lisp 'ccl
          slime-lisp-implementations '((ccl ("~/software/ccl/lx86cl64"))
                                       (sbcl ("/usr/bin/sbcl")))))

   ((eq my-curr-computer 'a-laptop-faster)
    (setq slime-default-lisp 'ccl
          slime-lisp-implementations '((ccl ("~/proj/ccl/lx86cl"))))))

  (when nil ;; don't turn on SLIME automatically for now.
    ;; when on a computer with SLIME set up
    (when (or (eq my-curr-computer 'work-laptop)
              (eq my-curr-computer 'utilite)
              (eq my-curr-computer 'a-laptop-faster))
      ;; Connect lisp buffers to SLIME automatically.
      ;; I think this works because in SLIME's MELPA package, file
      ;; slime-autoloads.el, it sneakily adds a slime hook to lisp-mode-hook.
      ;; Using the code: (add-hook 'lisp-mode-hook 'slime-lisp-mode-hook)
      (add-hook 'slime-mode-hook
                (lambda ()
                  (unless (slime-connected-p)
                    (save-excursion (slime)))))))

  ;; overlay using the 'eros package. TODO: wire-up to a keybind.
  (require 'dash)
  (require 's)
  (defun slime-eval-last-sexp-overlay ()
    (interactive)
    (eros--eval-overlay
     (let ((result (s-trim (slime-eval `(swank:pprint-eval
                                         ,(slime-last-expression))))))
       (cond
        ;; number or string, eval it
        ((-contains-p '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?\")
                      (string-to-char (substring result 0 1)))
         (eval (car (read-from-string result))))

        ;; eg #\\Newline
        ((s-starts-with-p "#\\" result) result)

        ;; cf http://www.lispworks.com/documentation/HyperSpec/Body/02_dh.htm
        ;; eg #p"arst" => "#p arst"
        ((s-starts-with-p "#" result)
         (format "%s %s"
                 (substring result 0 2)
                 (substring result 3 (- (length result) 1))))

        ;; maybe remove NIL from the end
        ((s-ends-with-p "\nNIL" result) result)

        ;; list
        ((and (s-starts-with-p "(" result)
              (s-ends-with-p ")" result))
         (eval (car (read-from-string (format "(quote %s)" result)))))

        ;; spaces, give up
        ((s-contains-p " " result) result)

        ;; symbol
        (t (eval (car (read-from-string (format "(quote %s)" result)))))))
     (point)))

  ;; (add-hook 'slime-mode-hook #'lispy-mode)
  ;; (add-hook 'slime-repl-mode-hook #'lispy-mode)
  (defun my-setup-slime-repl ()
    ;; Turn off line numbers in the repl
    ;; (linum-mode 0) ;; linum-mode obsolete and not loaded by default on
                      ;; emacs 29+

    ;; There's always a trailing space at repl prompt. Don't highlight it.
    (setq show-trailing-whitespace nil)
    ;; Aggressive-indent moves SLIME's comments in the REPL. Turn it off.
    (when (fboundp 'aggressive-indent-mode)
      (aggressive-indent-mode 0)))

  (add-hook 'slime-repl-mode-hook #'my-setup-slime-repl)

  ;; (define-key slime-mode-map (kbd "<tab>")
  ;;   #'slime-indent-and-complete-symbol)

  (if my-graphic-p
      (defun my-slime-eval-last-sexp-display ()
        (interactive)
        (save-excursion
          (evil-append 1)
          (let ((string (slime-last-expression)))
            (evil-normal-state)
            (slime-eval-async
             `(swank:eval-and-grab-output ,string)
             (lambda (result)
               (cl-destructuring-bind (_output value) result
                 (pos-tip-show value)
                 ;;(push-mark)
                 ;;(insert output value)
                 ))))))
    (defun my-slime-eval-last-sexp-display ()
      (interactive)
      (evil-append 1)
      (let ((string (slime-last-expression)))
        (evil-normal-state)
        (slime-eval-async
         `(swank:eval-and-grab-output ,string)
         (lambda (result)
           (cl-destructuring-bind (output value) result
             ;; (pos-tip-show value)
             (save-excursion
               (push-mark)
               (evil-append 1)
               (default-indent-new-line)
               (insert output value)
               (evil-normal-state))))))))

  (require 'eros)
  (defun slime-eval-last-expression-eros ()
    (interactive)
    (save-excursion
      (forward-char)
      (cl-destructuring-bind (output value)
          (slime-eval `(swank:eval-and-grab-output ,(slime-last-expression)))
        (eros--make-result-overlay (concat output value)
          :where (point)
          :duration eros-eval-result-duration))))

  (when my-use-evil-p
    ;; NOTE: `evil-leader/set-key-for-mode' doesn't work for minor modes
    ;;       like `slime-mode'. Binding for `lisp-mode' instead since I
    ;;       automatically turn on slime.
    ;; TODO: find alternative to fn `evil-leader/set-key-for-mode' or
    ;;       even an alternative to `evil-leader' itself.
    ;; (evil-leader/set-key-for-mode 'slime-mode "e" eval-fn)
    (evil-leader/set-key-for-mode 'lisp-mode "e"
      ;#'my-slime-eval-last-sexp-display
      #'slime-eval-last-expression-eros)
    (evil-leader/set-key-for-mode 'slime-repl-mode "e"
      ;#'my-slime-eval-last-sexp-display
      #'slime-eval-last-expression-eros))

  ;; set link to common lisp hyperspec.
  (setq common-lisp-hyperspec-root
        (concat "file://"
                (if (eq system-type 'windows-nt) "/" "") ; extra / on windows.
                (expand-file-name "~/")
                ".emacs.d/notElpa/hyperspec/HyperSpec/")
        ;; (cond
        ;;  ((eq my-curr-computer 'work-laptop)
        ;;   "file:///C:/users/mtz/CommonLispHyperSpec/HyperSpec/")
        ;;  ((eq my-curr-computer 'wild-dog)
        ;;   "file:///home/mike/books/lisp/hyperspec/HyperSpec/")
        ;;  ;; else use the online version.
        ;;  (t "http://www.lispworks.com/documentation/HyperSpec/"))
        )

  (defun my-view-hyperspec ()
    (interactive)
    (let ((browse-url-browser-function #'eww-browse-url))
      (slime-documentation-lookup)))
  (let ((key (kbd "C-c C-d h")))
    (define-key slime-mode-map key #'my-view-hyperspec)
    (define-key slime-repl-mode-map key #'my-view-hyperspec)
    (define-key slime-macroexpansion-minor-mode-map key #'my-view-hyperspec))
  (when my-use-ivy-p
    ;; (define-key slime-mode-map (kbd "C-M-i") #'counsel-cl)
    (define-key slime-mode-map (kbd "C-M-i") #'complete-symbol))
  (global-set-key (kbd "C-c b") #'slime-selector))


;;;----------------------------------------------------------------------------
;;; redshank
;;;----------------------------------------------------------------------------
;; (require 'redshank-loader)
;; (eval-after-load "redshank-loader"
;;   `(redshank-setup '(lisp-mode-hook
;;                      slime-repl-mode-hook) t))
;; ;;   (eval-after-load "redshank"
;; ;;     '(progn ...redefine keys, etc....))


;;;----------------------------------------------------------------------------
;;; company
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/company-mode" load-path)
(autoload #'company-mode "company" nil t)
(autoload #'global-company-mode "company" nil t)
(autoload #'company-manual-begin "company" nil t)
(autoload #'company-complete "company" nil t)
(autoload #'company-ispell "company-ispell" nil t)
(autoload #'company-abbrev "company-abbrev" nil t)
(autoload #'company-css "company-css" nil t)
(autoload #'company-dabbrev "company-dabbrev" nil t)
(autoload #'company-dabbrev-code "company-dabbrev-code" nil t)
(autoload #'company-elisp "company-elisp" nil t)

;; (require 'company)
;; (add-hook 'after-init-hook #'global-company-mode) ; all buffers

(when my-use-evil-p
  (evil-define-key 'insert global-map (kbd "C-SPC") #'company-complete)
  (evil-define-key 'insert global-map (kbd "C-o") #'company-complete)
  (evil-define-key 'insert global-map (kbd "C-w") #'company-ispell))


(with-eval-after-load 'company
  (global-company-mode 1) ; company functions require company mode to be "on".
  (when my-use-evil-p
    ;; C-Space like Visual Studio
    (evil-define-key 'insert company-mode-map (kbd "C-SPC") #'company-complete)
    ;; C-SPC doesn't work in some terminals, so bind an alternative key.
    (evil-define-key 'insert company-mode-map (kbd "C-o") #'company-complete)
    ;; (define-key company-mode-map (kbd "C-SPC") #'company-complete)
    (evil-define-key 'insert company-mode-map (kbd "C-w") #'company-ispell))

  (defun my-company-page-size ()
    "Return the page size of the actively displayed company popup."
    ;; page-size-pseudo gives the correct page size when a small window forces
    ;; a smaller company popup.
    (let* ((page-size-pseudo (abs (company--pseudo-tooltip-height)))
           (page-size-normal (if (< company-candidates-length
                                    company-tooltip-limit)
                                 company-candidates-length
                               company-tooltip-limit))
           (page-size        (if (< page-size-pseudo page-size-normal)
                                 page-size-pseudo
                               page-size-normal)))
      page-size))

  (defun my-company-next-page ()
    "Copy of `company-next-page'.  But with different page size calculation."
    (interactive)
    (when (company-manual-begin)
      (if (and company-selection-wrap-around
               (= company-selection (1- company-candidates-length)))
          (company-set-selection 0)
        (let (company-selection-wrap-around)
          (company-set-selection
           (+ company-selection
              (my-company-page-size) ; company-tooltip-limit
              ))))))

  (defun my-company-previous-page ()
    "Copy of `company-previous-page'.
But with different page size calculation."
    (interactive)
    (when (company-manual-begin)
      (if (and company-selection-wrap-around
               (zerop company-selection))
          (company-set-selection (1- company-candidates-length))
        (let (company-selection-wrap-around)
          (company-set-selection
           (- company-selection
              (my-company-page-size) ; company-tooltip-limit
              ))))))

  (defun my-company-complete-common ()
    "Like `company-complete-common', but insert a hyphen on repeated press."
    (interactive)
    (let ((repeatp (eq this-command last-command)))
      (if repeatp
          (insert "-")
        (company-complete-common))))
  (define-key company-active-map (kbd "SPC") #'my-company-complete-common)

  ;; unbind C-h. it interferes with C-h k to lookup what is bound.
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-SPC") #'company-complete-common)
  ;; (define-key company-active-map (kbd "SPC") #'company-complete-common)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "\C-d") #'company-show-doc-buffer)
  ;; expands till -. Completes after that.
  (define-key company-active-map (kbd "C-<tab>") #'company-complete-common)
  (define-key company-active-map (kbd "<tab>") #'company-complete)
  ;; would be default, but my other keymap killed this
  (define-key company-active-map (kbd "C-v") #'my-company-next-page)
  ;; default, but set just in case.
  (define-key company-active-map (kbd "M-v") #'my-company-previous-page)
  (defun my-company-jump-to-first ()
    (interactive)
    (let ((company-selection-wrap-around nil))
      (company-set-selection 0)))
  (defun my-company-jump-to-last ()
    (interactive)
    (let ((company-selection-wrap-around nil))
      (company-set-selection company-candidates-length)))
  (define-key company-active-map (kbd "M-<") #'my-company-jump-to-first)
  (define-key company-active-map (kbd "M->") #'my-company-jump-to-last)

  (let* ((positions '(mid bot top))
         (pos nil) ; cache values for repeated M-r presses.
         (page-size nil))
    (defun my-company-cycle-position ()
      "Jump to the mid/bot/top of the currently displayed company candidates.
Cycles between 3 locations mid/bot/top.
Similar to `move-to-window-line-top-bottom' (M-r) in normal buffers.
Closure over `pos', `page-size'."
      (interactive)
      (let ((repeatp (eq this-command last-command)))
        (unless repeatp
          (setq page-size (my-company-page-size)))
        (setq pos (car (or (cdr (memq (if repeatp pos nil) positions))
                                positions))))
      (let* ((row-num company-tooltip-offset) ; lines-above
             (move-cnt (cond ((eq pos 'top) 0)
                             ((eq pos 'mid) (/ page-size 2))
                             ((eq pos 'bot) (1- page-size))))
             (row-num-target (+ row-num move-cnt)))
        ;; the jump
        (company-set-selection row-num-target))))
  (define-key company-active-map (kbd "M-r") #'my-company-cycle-position)


  (defun my--company-set-min-width (&rest _ignored-hook-args)
    "Calculate width big enough for the largest candidate and set it.
This avoids changing pop-up width while scrolling through candidates."
    (setq company-tooltip-minimum-width
          (apply #'max
                 (mapcar #'length
                         company-candidates))))
  (add-hook 'company-completion-started-hook
            #'my--company-set-min-width)

  (setq company-idle-delay nil)          ; disable automatic completion
  (setq company-minimum-prefix-length 3) ; but if automatic is on, don't fire
                                         ; until 3 chars.
  (setq company-tooltip-limit 21))       ; popup more suggestions.


;;;----------------------------------------------------------------------------
;;; select numbered completions. from https://oremacs.com/
;;;----------------------------------------------------------------------------
(when nil ;; don't do this for now. It's messing up alignment of the
           ;; numbers on the right?
  (setq company-show-quick-access t)

  (with-eval-after-load 'company
    (defun ora-company-number ()
      "Forward to `company-complete-number'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
      (interactive)
      (let* ((k (this-command-keys))
             (re (concat "^" company-prefix k)))
        (if (cl-find-if (lambda (s) (string-match re s))
                        company-candidates)
            (self-insert-command 1)
          (company-complete-tooltip-row (string-to-number k)))))

    (let ((map company-active-map))
      (mapc
       (lambda (x)
         (define-key map (format "%d" x) 'ora-company-number))
       (number-sequence 0 9))
      (define-key map " " (lambda ()
                            (interactive)
                            (company-abort)
                            (self-insert-command 1)))
      (define-key map (kbd "<return>") nil))))


;;;----------------------------------------------------------------------------
;;; web-completion-data. dependency of company-web
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/web-completion-data" load-path)

;;;----------------------------------------------------------------------------
;;; company-web
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/company-web" load-path)
(autoload #'company-web-html "company-web-html" nil t)

(with-eval-after-load 'web-mode
  (require 'company) ; for company-backends
  (push 'company-web-html company-backends)
  (push 'company-web-jade company-backends)
  (push 'company-web-slim company-backends)

  (defun my-setup-company-web ()
    (set (make-local-variable 'company-backends)
         '(company-web-html company-files)))
  (add-hook 'web-mode-hook #'my-setup-company-web)

  (when my-use-evil-p
    (define-key web-mode-map (kbd "C-SPC") #'company-web-html)))


;;;----------------------------------------------------------------------------
;;; company-quickhelp. not using messes up keybinds
;;;----------------------------------------------------------------------------
;; (setq company-quickhelp-idle-delay 0.5)
;; (company-quickhelp-mode 1)

;;;----------------------------------------------------------------------------
;;; slime-company
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/slime-company" load-path)
;; this is set in the slime section

;;;----------------------------------------------------------------------------
;;; Auto-complete
;;;----------------------------------------------------------------------------
;; TODO: look into a way to use auto-complete for some modes and company for
;;       others.

;; ;;use auto-complete in emacs 24.3 and below
;; (when t
;;   ;; (and (<= emacs-major-version 24)
;;   ;;      (<= emacs-minor-version 3))
;;   (require 'auto-complete-config)
;;   (ac-config-default)

;;   ;; C-Space like Visual Studio
;;   (define-key ac-mode-map (kbd "C-SPC") 'auto-complete)
;;   ;; don't automatically pop up completions. Use C-Space
;;   (setq ac-auto-start nil)

;;   ;;navigate completion menu with c-n and c-p
;;   (setq ac-use-menu-map t)
;;   (define-key ac-menu-map "\C-n" 'ac-next)
;;   (define-key ac-menu-map "\C-p" 'ac-previous)

;;   (setq ac-menu-height 10) ;num items in the popup.
;;   (setq ac-use-quick-help t)
;;   (setq ac-quick-help-delay 0.6)

;;   ;;(set-face-background 'ac-candidate-face "lightgray")
;;   ;;(set-face-underline 'ac-candidate-face "darkgray")
;;   ;;(set-face-background 'ac-selection-face "steelblue")
;;   )

;;;----------------------------------------------------------------------------
;;; ac-slime. integrates auto-complete with slime.
;;;----------------------------------------------------------------------------
;; (when t
;;   (require 'ac-slime)
;;   ;;(add-hook 'slime-mode-hook 'set-up-slime-ac)
;;   ;;(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;;   (add-hook 'slime-mode-hook (lambda ()
;;                                (interactive)
;;                                (set-up-slime-ac t))) ;t for fuzzy matching
;;   (add-hook 'slime-repl-mode-hook
;;             (lambda ()
;;               (interactive)
;;               (set-up-slime-ac t))) ;t for fuzzy matching
;;   (eval-after-load "auto-complete"
;;     '(add-to-list 'ac-modes 'slime-repl-mode)))

;;;----------------------------------------------------------------------------
;;; turn on lisp-mode when editing file .stumpwmrc
;;;----------------------------------------------------------------------------
;; don't need this anymore. Using a mode hint comment in .stumpwmrc instead.
;; (add-to-list 'auto-mode-alist '("\\.stumpwmrc\\'" . lisp-mode))

;;;----------------------------------------------------------------------------
;;; Org mode
;;;----------------------------------------------------------------------------
(push '("\\.org\\'" . org-mode) auto-mode-alist)


(defvar my-main-todo (cond ((eq my-curr-computer 'wild-dog)
                            "~/todo/TODO.org")

                           ((memq my-curr-computer '(work-laptop-2019))
                            "C:/Users/mtz/TODO/TODO.org")

                           (t nil))
  "The main todo file on a particular computer.")

;; if the computer has a main todo file.
(when my-main-todo
  (defun my-open-main-todo ()
    (interactive)
    (message "Opening main todo...")
    (find-file-existing my-main-todo))
  (when my-use-evil-p
    (evil-leader/set-key "t" #'my-open-main-todo)
    (evil-leader/set-key "a" #'org-agenda-list)))

(with-eval-after-load 'org
  (when my-use-evil-p
    ;; `evil-define-key' isn't working?
    ;; (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
    (define-key org-mode-map (kbd "<tab>") #'org-cycle))

  (setq org-startup-indented t)
  ;; show the leading stars. But it's set to t again by `org-indent-mode'?
  ;; TODO: try setting in a hook.
  (setq org-hide-leading-stars nil)

  (setq org-log-done t) ; make time stamp when flagging something done
                        ; with C-c C-t
  (setq org-agenda-timegrid-use-ampm t)
  (setq org-src-preserve-indentation t)

  (progn ;;HOLD keyword stuff
    ;; new keyword for tasks put on hold
    (push '(sequence "HOLD") org-todo-keywords)

    ;; (defface org-hold ;; font-lock-warning-face
    ;;   '((t (:foreground "powder blue" :weight bold)))
    ;;   "Face for HOLD keywords."
    ;;   :group 'org-faces)

    ;; icy color to make HOLD items look frozen.
    (setq org-todo-keyword-faces '(("HOLD" . (:foreground "deep sky blue"
                                                          :weight bold)))))

  (defun my-org-export-html ()
    "Custom Emacs fthemes tend to produce unreadable colors in html docs.
Disable themes before explorting to html, then turn them back on."
    (interactive)
    (let ((themes custom-enabled-themes))
      ;; disable themes
      (dolist (thm themes)
        (disable-theme thm))

      ;; export
      (org-html-export-to-html) ; (org-html-export-as-html)

      ;; turn themes back on
      (dolist (thm themes)
        (load-theme thm t))))

  ;; TODO: fix `org-summary-todo'. Not working. Commented out for now.
  ;; (defun org-summary-todo (n-done n-not-done)
  ;;   "Switch entry to DONE when all sub-entries are done, to TODO otherwise."
  ;;   (let ((org-log-done org-log-states)) ; turn off logging
  ;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  ;; (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; on computers that have a main todo file
  (when my-main-todo
    ;; `org-agenda-files' is used by fn `org-agenda-list'
    (setq org-agenda-files `(,my-main-todo)))

  ;; org mode steals M-h keybind. reclaim it. TODO: rebind org fn to a key.
  (when my-use-evil-p
    (define-key org-mode-map (kbd "M-h") #'evil-window-left))

  (defun my-wrap-in-org-src-block (start end)
    "Wrap the region in an org source block.
Defaults to c for `c-mode'.
TODO: specify the mode in a place-holder with `yasnippet'.

START = start of region.
END = end of region.

Inserts a new line and the beginning and end with text values:
#+BEGIN_SRC c
#+END_SRC"
    (interactive "r") ; automatically wires up the current region's start/end
                      ; to the args start/end.
    (goto-char end)
    (insert "#+END_SRC\n")

    (goto-char start)
    (insert "#+BEGIN_SRC c\n"))

  (when my-use-evil-p
    ;; (define-key org-mode-map (kbd "M-h") #'evil-window-left)
    (evil-define-key
      'visual
      org-mode-map
      (kbd ";")
      #'my-wrap-in-org-src-block)))

;;;----------------------------------------------------------------------------
;;; worf. key shortcuts for org-mode
;;;----------------------------------------------------------------------------


;;;----------------------------------------------------------------------------
;;; csharp-mode. C#
;;;----------------------------------------------------------------------------
(unless (fboundp #'csharp-mode)
  ;; csharp-mode was added to core emacs in version 29. So avoid loading the
  ;; separate pacakge if on emacs 29+. NOTE: check for the fn itself rather
  ;; than the Emacs version becuase older Emacs 29's in package managers (brew)
  ;; still don't have csharp-mode yet!!!
  (push "~/.emacs.d/notElpa/csharp-mode" load-path))
(autoload #'csharp-mode "csharp-mode" nil t)

(push '("\\.cs$" . csharp-mode) auto-mode-alist)

(with-eval-after-load 'csharp-mode

  ;; keybinds
  (define-key csharp-mode-map (kbd "C-c C-c") #'compile)

  ;; hook
  (defun my-setup-csharp-mode ()
    ;; set compile-command. Assumes dotnet core
    (let* ((dotnet (cond ((eq system-type 'darwin)
                          "/usr/local/bin/dotnet")
                         ((eq system-type 'windows-nt)
                          "\"C:\\Program Files\\dotnet\\dotnet\"")))
           (cmd (concat dotnet " build")))
      (set (make-local-variable 'compile-command)
           cmd))

    (yas-minor-mode 1)
    (rainbow-delimiters-mode 1)

    (when (fboundp #'display-fill-column-indicator-mode)
      (setq display-fill-column-indicator-column 110) ; long lines in C#
      (display-fill-column-indicator-mode 1))

    (my-turn-on-electric-pair-local-mode))
  (add-hook #'csharp-mode-hook #'my-setup-csharp-mode))


;;;----------------------------------------------------------------------------
;;; align-let.el in ~/.emacs.d/notElpa
;;;----------------------------------------------------------------------------
;; (autoload 'align-let "align-let" nil t)
;; (let ((key (kbd "C-c C-a")))
;;   (define-key lisp-mode-map key #'align-let)
;;   (define-key emacs-lisp-mode-map key #'align-let)
;;   (define-key lisp-interaction-mode-map key #'align-let))

(with-eval-after-load 'lisp-mode
  (autoload 'align-let "align-let" nil t)
  (define-key lisp-mode-shared-map (kbd "C-c C-a") #'align-let))

;; (let ((abadf 333)
;;       (x     222)
;;       (yy    44)))

;; (setq aaaaaaaaaaaaa 2
;;       b             3
;;       cc            433
;;       d             "hello there")



;;;----------------------------------------------------------------------------
;;; javascript-mode
;;; js-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'js
  (define-key js-mode-map (kbd "C-c C-c") #'compile)

  (progn ;; Add parsing of jshint output in compilation mode
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(jshint "^\\(.*\\): line \\([0-9]+\\), col \\([0-9]+\\), "
                          1 2 3))
    (add-to-list 'compilation-error-regexp-alist 'jshint))

  (defvar my-js-linter-cmds '(:jslint "jslint --terse "
                                     :jshint "jshint "
                                     :eslint "eslint -f unix ")
    "Property list of commands to invoke linter.
With option flags so the output is friendly to the Emacs compile buffer.
For :jshint, the parsing of output is done via
`compilation-error-regexp-alist-alist' to make the output friendly to the
Emacs compile buffer.")

  (cl-defun my-js-set-compile-command (&optional linter-sym)
    "Set js compile command.
LINTER values: :jslint :jshint :eslint"
    (interactive)
    ;; for now require a file
    (unless (and buffer-file-name
                 (my-str-ends-with-p buffer-file-name ".js"))
      (message "No js file found for buffer.")
      (cl-return-from my-js-set-compile-command))

    (let* ((linter (or linter-sym
                       (intern (completing-read "linter: "
                                                '(:jslint :jshint :eslint)))))
           (cmd (cl-getf my-js-linter-cmds linter)))
      ;; wireup M-x compile
      (set (make-local-variable 'compile-command)
           (concat cmd (shell-quote-argument buffer-file-name)))))

  (defun my-setup-js ()
    ;; set explicitly because shorter width in json mode corrupts it.
    (setq js-indent-level my-indent-width)
    (when (fboundp #'display-fill-column-indicator-mode)
      (setq display-fill-column-indicator-column 79)
      (display-fill-column-indicator-mode 1))
    (my-turn-on-electric-pair-local-mode)
    (yas-minor-mode 1)
    (rainbow-delimiters-mode-enable)
    ;; (electric-spacing-mode 1)
    (my-js-set-compile-command :jshint))

  (add-hook 'js-mode-hook #'my-setup-js))


;;;----------------------------------------------------------------------------
;;; js2-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/js2-mode" load-path)
(autoload 'js2-imenu-extras-setup "js2-imenu-extras" nil nil nil)
(autoload 'js2-imenu-extras-mode "js2-imenu-extras" nil t nil)
(autoload 'js2-highlight-unused-variables-mode "js2-mode" nil t nil)
(autoload 'js2-minor-mode "js2-mode" nil t nil)
(autoload 'js2-mode "js2-mode" nil t nil)
(autoload 'js2-jsx-mode "js2-mode" nil t nil)

(push '("\\.js$" . js2-mode) auto-mode-alist)

(with-eval-after-load 'js2-mode
  (setq-default
   js2-global-externs
   '("$" "module" "require" "buster" "sinon" "assert" "refute"
     "__dirname" "console" "JSON" "ActiveXObject" "jQuery"
     ;; most copied from the auto-complete dict folder.
     "Anchor" "Area" "Array" "Boolean" "Button" "Checkbox" "Date"
     "Document" "Element" "FileUpload" "Form" "Frame" "Function"
     "Hidden" "History" "Image" "Infinity" "JavaArray" "JavaClass"
     "JavaObject" "JavaPackage" "Link" "Location" "Math" "MimeType"
     "NaN" "Navigator" "Number" "Object" "Option" "Packages"
     "Password" "Plugin" "Radio" "RegExp" "Reset" "Select" "String"
     "Submit" "Text" "Textarea" "Window" "alert" "arguments" "assign"
     "blur" "break" "callee" "caller" "captureEvents" "case"
     "clearInterval" "clearTimeout" "close" "closed" "comment"
     "confirm" "constructor" "continue" "default" "defaultStatus"
     "delete" "do" "document" "else" "escape" "eval" "export" "find"
     "focus" "for" "frames" "function" "getClass" "history" "home"
     "if" "import" "in" "innerHeight" "innerWidth" "isFinite" "isNan"
     "java" "label" "length" "location" "locationbar" "menubar"
     "moveBy" "moveTo" "name" "navigate" "navigator" "netscape" "new"
     "onBlur" "onError" "onFocus" "onLoad" "onUnload" "open" "opener"
     "outerHeight" "outerWidth" "pageXoffset" "pageYoffset" "parent"
     "parseFloat" "parseInt" "personalbar" "print" "prompt"
     "prototype" "ref" "releaseEvents" "resizeBy" "resizeTo" "return"
     "routeEvent" "scroll" "scrollBy" "scrollTo" "scrollbars" "self"
     "setInterval" "setTimeout" "status" "statusbar" "stop" "sun"
     "switch" "taint" "this" "toString" "toolbar" "top" "typeof"
     "unescape" "untaint" "unwatch" "valueOf" "var" "void" "watch"
     "while" "window" "with"))
  (setq js2-highlight-level 3) ;;maximum highlighting

  (setq js2-bounce-indent-p nil) ;; set t to have tab toggle indents

  ;; default is 4, but set explicilty anyway.
  (setq js2-basic-offset my-indent-width)

  (setq js2-mode-show-strict-warnings t)
  (setq js2-warn-about-unused-function-arguments t)

  ;; ;; recognize vars in the global comment for jslint. Doesn't work?
  ;; (setq js2-include-jslint-globals t)

  ;; ;; After js2 has parsed a js file, we look for jslint globals decl
  ;; ;; comment ("/* global Fred, _, Harry */") and add any symbols to a
  ;; ;; buffer-local var of acceptable global vars. Note that we also support
  ;; ;; the "symbol: true" way of specifying names via a hack (remove any
  ;; ;; ":true" to make it look like a plain decl, and any ':false' are left
  ;; ;; behind so they'll effectively be ignored as you can't have a symbol
  ;; ;; called "someName:false"
  ;; (add-hook 'js2-post-parse-callbacks
  ;;           (lambda ()
  ;;             (when (> (buffer-size) 0)
  ;;               (let ((btext (replace-regexp-in-string
  ;;                             ": *true" " "
  ;;                             (replace-regexp-in-string
  ;;                              "[\n\t ]+" " "
  ;;                              (buffer-substring-no-properties
  ;;                               1
  ;;                               (buffer-size))
  ;;                              t t))))
  ;;                 (mapc (apply-partially 'add-to-list
  ;;                                        'js2-additional-externs)
  ;;                       (split-string
  ;;                        (if (string-match "/\\* *global *\\(.*?\\) *\\*/"
  ;;                                          btext)
  ;;                            (match-string-no-properties 1 btext) "")
  ;;                        " *, *" t))))))

  (defun my-js2-prev-error ()
    (interactive)
    (js2-next-error -1))
  (when my-use-evil-p
    ;; js2 steals M-j keybinding by default. Reclaim it.
    (define-key js2-mode-map (kbd "M-j") #'evil-window-down)
    (evil-define-key 'normal js2-mode-map (kbd "M-n") #'js2-next-error)
    (evil-define-key 'normal js2-mode-map (kbd "M-p") #'my-js2-prev-error))
  (define-key js2-mode-map (kbd "C-c e") #'js2-display-error-list)

  (defun my-js2-indent-defun ()
    "Indent the function the cursor is inside."
    (interactive)
    (js2-mark-defun)
    (call-interactively #'indent-region))
  (define-key js2-mode-map (kbd "C-c <tab>") #'my-js2-indent-defun)

  ;; (defhydra hydra-js2-flycheck ()
  ;;   "js2 flycheck"
  ;;   ("n" flycheck-next-error)
  ;;   ("p" flycheck-previous-error)
  ;;   ("q" nil))
  ;; (evil-define-key 'normal js2-mode-map (kbd "C-c l")
  ;;   #'hydra-js2-flycheck/body)

  ;; TODO: this hydra isn't working? look into it later.
  ;; (defhydra my-hydra-js2-flymake (:color amaranth)
  ;;   "jslint:flymake: "
  ;;   ("n" flymake-goto-next-error)
  ;;   ("p" flymake-goto-prev-error)
  ;;   ("v" flymake-popup-current-error-menu)
  ;;   ("C-g" nil nil)
  ;;   ("q" nil))
  ;; (when my-use-evil-p
  ;;   (evil-define-key 'normal js2-mode-map (kbd "C-c l")
  ;;     #'my-hydra-js2-flymake/body))

  ;; (evil-define-key 'normal js2-mode-map (kbd "C-c h") #'my-hydra-hs/body)

  ;; Add parsing of jslint output in compilation mode
  ;; (add-to-list 'compilation-error-regexp-alist-alist
  ;;              '(jslint "^\\(.*\\): line \\([0-9]+\\), col \\([0-9]+\\), "
  ;;                       1 2 3))
  ;; (add-to-list 'compilation-error-regexp-alist 'jslint)

  (defun my-set-jslint-compile-command ()
    "Set the `compile-command' to jslint."
    (interactive)
    ;; TODO: look into a way to send the text to jslint without a file.
    ;; wireup M-x compile
    (when buffer-file-name
      (set (make-local-variable 'compile-command)
           (concat "jslint --terse "
                   (shell-quote-argument buffer-file-name)))))

  (defun my-js2-init ()
    ;; NOTE: if a tmp file is created after js2-mode loads, this won't set
    ;; the `compile-command'. But `my-set-jslint-compile-command' can be
    ;; called manually in that case.
    ;; (my-set-jslint-compile-command)

    (js2-highlight-unused-variables-mode t)
    ;; replace ambiguous name "Javascript-IDE" with "js2"
    (setq mode-name "js2")
    ;; (setq-default js2-global-externs "jQuery $")
    ;; (setq-default js2-indent-on-enter-key t)
    ;; (add-to-list 'js2-ecma-262-externs "setTimeout")

    ;; (when (featurep 'js2-highlight-vars)
    ;;   (js2-highlight-vars-mode))

    ;;(js2-imenu-extras-mode)

    ;; (my-turn-on-electric-pair-local-mode)
    ;; (smartparens-mode 1)


    ;; (yas-minor-mode 1)
    ;; (rainbow-delimiters-mode-enable)
    ;; (electric-spacing-mode 1)

    ;; use jslint, but only if editing a .js file on disk.
    ;; TODO: use with in-memory buffer, or narrowed region of html file.
    ;; TODO: use flycheck instead of flymake
    ;; (when (and buffer-file-name
    ;;            (my-str-ends-with-p buffer-file-name ".js"))
    ;;   ;; wireup M-x compile
    ;;   (set (make-local-variable 'compile-command)
    ;;        (concat "jslint --terse "
    ;;                (shell-quote-argument buffer-file-name)))
    ;;   ;; ;; and turn on flymake-jslint. (only works on saved files)
    ;;   ;; (flymake-jslint-load)
    ;;   ;; ;; bind M-n, M-p to use flymake functions instead of js2 functions
    ;;   ;; (evil-define-key 'normal js2-mode-map (kbd "M-n")
    ;;   ;;   #'flymake-goto-next-error)
    ;;   ;; (evil-define-key 'normal js2-mode-map (kbd "M-p")
    ;;   ;;   #'flymake-goto-prev-error)
    ;;   ;; (evil-define-key 'normal js2-mode-map (kbd "C-c m")
    ;;   ;;   #'flymake-popup-current-error-menu)
    ;;   )
    ;; show a Greek lambda for function
    (setq prettify-symbols-alist '(("function" . 955)))
    ;; collapse/show sections of code
    (hs-minor-mode 1))

  (add-hook 'js2-mode-hook #'my-js2-init))

;;;----------------------------------------------------------------------------
;;; json-snatcher. dependency of json-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/json-snatcher" load-path)
(autoload #'jsons-print-path "json-snatcher" nil t)

;;;----------------------------------------------------------------------------
;;; json-reformat. dependency of json-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/json-reformat" load-path)
(autoload #'json-reformat-region "json-reformat" nil t)

;;;----------------------------------------------------------------------------
;;; json-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/json-mode" load-path)
(autoload #'json-mode "json-mode" nil t nil)
(autoload #'json-mode-show-path "json-mode" nil t nil)
(autoload #'json-mode-kill-path "json-mode" nil t nil)
(autoload #'json-mode-beautify "json-mode" nil t nil)

(with-eval-after-load 'json-mode
  (defun my-setup-json-mode ()
    ;; for json, I'd like to use a more compact indentation. 2 chars, and
    ;; visualize tabs as 2 chars wide. But `js-indent-level' is shared with
    ;; js-mode and js2-mode; corrupting it. Can't be fixed with hooks alone as
    ;; hooks don't fire on already-open buffers. So for now just using indent
    ;; of 4 to avoid corruption.
    ;; TODO: modify json-mode to use it's own independent indent-level.
    (progn
      ;; buffer local. Safe to change.
      (setq tab-width my-indent-width)
      ;; not buffer local! Make sure `js-indent-level' set in hooks
      ;; for javascript-mode and/or js2-mode.
      (setq js-indent-level my-indent-width))
    (rainbow-delimiters-mode 1)
    (my-turn-on-electric-pair-local-mode))

  (add-hook 'json-mode-hook #'my-setup-json-mode))

;;;----------------------------------------------------------------------------
;;; web-beautify
;;;----------------------------------------------------------------------------
;; ;; Depends on external programs: nodejs, js-beatify
;; ;; So only use on computers with the dependencies set up.
;; (when (eq my-curr-computer 'work-laptop)
;;   ;; (with-eval-after-load "web-beautify"
;;   ;;   (add-to-list 'web-beautify-args "3")
;;   ;;   (add-to-list 'web-beautify-args "-m")
;;   ;;   )
;;   (with-eval-after-load 'js2-mode
;;     (define-key js2-mode-map (kbd "C-c b") #'web-beautify-js))
;;   ;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
;;   (with-eval-after-load 'js
;;     (define-key js-mode-map (kbd "C-c b") #'web-beautify-js))

;;   (with-eval-after-load 'json-mode
;;     (define-key json-mode-map (kbd "C-c b") #'web-beautify-js))

;;   ;; (with-eval-after-load "sgml-mode"
;;   ;;   (define-key html-mode-map (kbd "C-c b") #'web-beautify-html))

;;   ;; (with-eval-after-load "css-mode"
;;   ;;   (define-key css-mode-map (kbd "C-c b") #'web-beautify-css))
;;   )

;; put the following JSON text into a .jsbeautifyrc file to control formatting
;; of external program js-beatify.
;; program
;; {
    ;;     "indent_size": 4,
    ;;     "indent_char": " ",
    ;;     "eol": "\n",
    ;;     "indent_level": 0,
    ;;     "indent_with_tabs": false,
    ;;     "preserve_newlines": true,
    ;;     "max_preserve_newlines": 4,
    ;;     "jslint_happy": true,
    ;;     "space_after_anon_function": false,
    ;;     "brace_style": "collapse",
    ;;     "keep_array_indentation": false,
    ;;     "keep_function_indentation": false,
    ;;     "space_before_conditional": true,
    ;;     "break_chained_methods": false,
    ;;     "eval_code": false,
    ;;     "unescape_strings": false,
    ;;     "wrap_line_length": 0,
    ;;     "wrap_attributes": "auto",
    ;;     "wrap_attributes_indent_size": 4,
    ;;     "end_with_newline": false,
    ;;     "good-stuff" : true
    ;; }

;;;----------------------------------------------------------------------------
;;; ac-js2
;;;----------------------------------------------------------------------------
;; (when nil
;;   (add-hook 'js2-mode-hook 'ac-js2-mode)
;;   (setq ac-js2-evaluate-calls t) ; requires connection to browser with
;;                                  ; (run-skewer)
;;   ;; (add-to-list 'ac-js2-external-libraries
;;   ;;              "path/to/lib/library.js") ; external lib example
;;   )

;;;----------------------------------------------------------------------------
;;; nxml
;;;----------------------------------------------------------------------------
(with-eval-after-load 'nxml-mode
  (setq nxml-slash-auto-complete-flag t) ;auto-insert when typing </
  (when my-use-evil-p
    ;; reclaim key M-h which nxml stole for`nxml-mark-paragraph'
    (define-key nxml-mode-map (kbd "M-h") #'evil-window-left))

  ;; TODO: make a minor mode for elmah logs. Inherit from nxml-mode.
  (defun my-elmah-format-xml ()
    "Insert newlines at the escape codes in elmah's XML error message.
To make it human readable."
    (interactive)
    (save-excursion
      (goto-char (point-min)) ;; go to beginning of buffer
      (let ((pos 0))
        (while (not (null pos))
          (setq pos (search-forward "&#xD;&#xA;" nil t))
          (unless (null pos)
            (newline)))))))

;;;----------------------------------------------------------------------------
;;; Helm
;;;----------------------------------------------------------------------------
;;(add-to-list 'load-path "~/.emacs.d/helm")
(push "~/.emacs.d/notElpa/helm" load-path)
(autoload #'helm "helm" nil nil)
(autoload #'helm-mode "helm-mode" nil t)
(autoload #'helm-buffers-list "helm-buffers" nil t)
(autoload #'helm-M-x "helm-command" nil t)
(autoload #'helm-find-files "helm-find-files" nil t)
(autoload #'helm-show-kill-ring "helm-show-kill-ring" nil t)
(autoload #'helm-imenu "helm-imenu" nil t)
(autoload #'helm-occur "helm-occur" nil t)

(when (and my-use-helm-p
           ;; helm is a little slow on a raspberry pi.
           (not (eq my-curr-computer 'raspberry-pi))
           (not (eq my-curr-computer 'leyna-laptop)))

  (progn ;;functions in key maps are auto-loaded.
    (when my-use-evil-p
     (evil-leader/set-key "b" #'helm-buffers-list))
    (global-set-key (kbd "C-x b") #'helm-buffers-list)
    ;;(evil-leader/set-key "b" #'helm-mini) ;;use helm instead of bs-show
    ;;(global-set-key (kbd "C-x b")   #'helm-mini)
    ;;(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
    (global-set-key (kbd "M-x") #'helm-M-x)
    (global-set-key (kbd "C-x C-f") #'helm-find-files)
    ;; (global-set-key (kbd "C-x C-r") #'helm-recentf)
    ;; (global-set-key (kbd "C-x r l") #'helm-filtered-bookmarks)
    (global-set-key (kbd "M-y") #'helm-show-kill-ring)
    (when my-use-evil-p
      (evil-leader/set-key "i" #'helm-imenu))
    ;; TODO: use `helm-dabbrev', once i figure out what's preventing it from
    ;;       finding candidates. The standard emacs `dabbrev-expand' works
    ;;       fine. `hippie-expand' works too.
    ;; (global-set-key (kbd "M-/") #'hippie-expand)
    ;; (global-set-key (kbd "M-/") #'helm-dabbrev)
    )

  (when my-load-helm-on-init-p
    (helm-mode 1)) ;helm-selection everywhere like when using M-x.
  ;; list of functions helm should ignore and allow default completion.
  ;; NOTE: this breaks if put in eval-after-load. Strange, but it works if
  ;; I just put it after the call to (helm-mode 1)
  ;;(add-to-list 'helm-completing-read-handlers-alist '(my-load-theme . nil))
  )

(with-eval-after-load "helm"
  (setq helm-ff-transformer-show-only-basename nil
        ;;helm-adaptive-history-file             "~/.emacs.d/data/helm-history"
        ;;helm-yank-symbol-first                 t
        ;;helm-move-to-line-cycle-in-source      t
        helm-buffers-fuzzy-matching t
        ;;helm-ff-auto-update-initial-value      t
        )

  (setq helm-ff-lynx-style-map nil
        helm-input-idle-delay 0.1
        helm-idle-delay 0.1)

  ;; (autoload 'helm-descbinds      "helm-descbinds" t)
  ;; (autoload 'helm-eshell-history "helm-eshell"    t)
  ;; (autoload 'helm-esh-pcomplete  "helm-eshell"    t)

  ;; (global-set-key (kbd "C-h a")    #'helm-apropos)
  ;; (global-set-key (kbd "C-h i")    #'helm-info-emacs)
  ;; (global-set-key (kbd "C-h b")    #'helm-descbinds)

  ;; (add-hook 'eshell-mode-hook
  ;; (lambda ()
  ;;   (define-key eshell-mode-map (kbd "<tab>") #'helm-esh-pcomplete)
  ;;   (define-key eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history)))


  ;;(helm-adaptative-mode t)

  (progn ;;from tuhdo. Customizing helm window size/display.
    (setq helm-display-header-line nil) ; save 1 line for rarely used header.
    ;; don't make source separators bigger than needed
    (set-face-attribute 'helm-source-header nil :height 1.0)
    ;; (progn
    ;;   ;; helm-autoresize-mode hides other windows, and dynamically adjusts
    ;;   ;; the helm window size as you type.
    ;;   (helm-autoresize-mode 1)
    ;;   ;;disable the dynamic size adjustment.
    ;;   (setq helm-autoresize-max-height 35)
    ;;   (setq helm-autoresize-min-height 35))
    ;; ;; prevents the window hiding from `helm-autoresize-mode'. And when
    ;; ;; there are lots of split windows, keep the popup at the current
    ;; ;; window.
    ;; (setq helm-split-window-in-side-p t)
    )


  ;; (progn
  ;;   ;; Trick from tuhdo. Move helm input to top of helm buffer, hide
  ;;   ;; in echo area.

  ;;   (setq helm-echo-input-in-header-line t)
  ;;   ;; optionally put helm buffer inside current buffer.
  ;;   (setq helm-split-window-in-side-p t)

  ;;   (defun helm-hide-minibuffer-maybe ()
  ;;     (when (with-helm-buffer helm-echo-input-in-header-line)
  ;;       (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
  ;;         (overlay-put ov 'window (selected-window))
  ;;         (overlay-put ov 'face
  ;;                      (let ((bg-color (face-background 'default nil)))
  ;;                        `(:background ,bg-color :foreground ,bg-color)))
  ;;         (setq-local cursor-type nil))))

  ;;   (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe))


  ;; ;; helm-selection everywhere like when using M-x. Putting this in
  ;; ;; eval-after-load to decrease start up time a bit.
  ;; (helm-mode 1)

  ;;(global-set-key (kbd "C-x c!")   #'helm-calcul-expression)
  ;;(global-set-key (kbd "C-x c:")   #'helm-eval-expression-with-eldoc)
  ;;(define-key helm-map (kbd "M-o") #'helm-previous-source)

  ;;(global-set-key (kbd "M-s s")   #'helm-ag)

  ) ;;end helm eval-after-load


;;;----------------------------------------------------------------------------
;;; helm-descbinds
;;;----------------------------------------------------------------------------
;; (helm-descbinds-mode)

;; ;; Now, `describe-bindings' is replaced to `helm-descbinds'. Type
;; ;; `C-h b', `C-x C-h' these run `helm-descbinds'.
;; ;;
;; ;; In the Helm buffer, you can select key-binds with helm interface.
;; ;;
;; ;;  - When type RET, selected candidate command is executed.
;; ;;
;; ;;  - When type TAB, You can "Execute", "Describe Function" or "Find
;; ;;    Function" by the menu.
;; ;;
;; ;;  - When type C-z, selected command is described without quiting.


;;;----------------------------------------------------------------------------
;;; helm-company
;;;----------------------------------------------------------------------------
;; (eval-after-load 'company
;;   '(progn
;;      (define-key company-mode-map (kbd "C-SPC") #'helm-company)
;;      (define-key company-active-map (kbd "C-SPC") #'helm-company)))

;;;----------------------------------------------------------------------------
;;; evil-escape
;;;----------------------------------------------------------------------------
(autoload #'evil-escape-mode "evil-escape" nil t)

(with-eval-after-load 'evil-escape
  (setq evil-escape-key-sequence (kbd "fj"))
  (setq evil-escape-delay 0.2))

;; NOTE: Don't use evil-escape. Using `key-chord' instead becuase it doesn't
;; care about the order of the keys in the chord. ie "fj" and "jf" both work.
;; (evil-escape-mode 1)

;;;----------------------------------------------------------------------------
;;; helm-git-grep (makes emacs crash on windows)
;;;----------------------------------------------------------------------------
;; (when my-run-sys-specific
;;   (defadvice helm-git-grep (after turn-off-activeupdate)
;;     "Turn off active update in MS-windows.
;; It can't handle grep processes spawning on each keystroke."
;;     (helm-toggle-suspend-update))
;;   (ad-activate 'helm-git-grep))

;; (require 'helm-git-grep)
;; (define-key helm-git-grep-mode-map (kbd "C-u") #'helm-toggle-suspend-update)
;; (evil-leader/set-key "g" #'helm-git-grep)


;;;----------------------------------------------------------------------------
;;; vc-git-grep. This is better for ms-windows since it can't handle
;;; helm-git-grep's many processes. Also grepping is a pretty heavy weight
;;; operation so I prefer to set up the search inputs first, select the top
;;; folder, etc instead of searching in real-time for each key press.
;;;----------------------------------------------------------------------------
(autoload 'vc-git-grep "vc-git" nil t)
;; defined in ~/emacs.d/notElpa/mine/my-vc-git-grep.el
(autoload 'my-vc-git-grep "my-vc-git-grep" nil t)
(autoload #'my-grep-dwim "my-grep" nil t)
;; (when my-use-evil-p
;;   (evil-leader/set-key "g" #'my-grep-dwim))

;;;----------------------------------------------------------------------------
;;; helm-swoop. No longer using as the built-in `helm-occur' is faster.
;;;----------------------------------------------------------------------------
;; (autoload 'helm-swoop "helm-swoop" nil t)

;; invoke with M-x for now.
;; (when my-use-helm-p
;;   ;; helm needs to be initialized or else helm-swoop won't work.
;;   ;; (it doesn't `require' everything it needs)
;;   (when my-use-evil-p
;;     (define-key evil-normal-state-map (kbd "s") #'helm-swoop)))

;; (global-set-key (kbd "C-c s") #'helm-swoop)
;; (global-set-key (kbd "C-c C-s") #'helm-swoop)
;;(evil-leader/set-key "s" #'helm-multi-swoop-all)

(with-eval-after-load 'helm-swoop
  ;;Prevent swoop from grabbing the text under the cursor. I rarely want that.
  (setq helm-swoop-pre-input-function
        (lambda () ""))

  ;; Change keybinds to whatever you like :)
  ;; (global-set-key (kbd "M-i") #'helm-swoop)
  ;; (global-set-key (kbd "M-I") #'helm-swoop-back-to-last-point)
  ;; (global-set-key (kbd "C-c M-i") #'helm-multi-swoop)
  ;; (global-set-key (kbd "C-x M-i") #'helm-multi-swoop-all)

  ;; When doing isearch, hand the word over to helm-swoop
  ;; (define-key isearch-mode-map (kbd "M-i") #'helm-swoop-from-isearch)
  ;; (define-key helm-swoop-map (kbd "M-i")
  ;;   #'helm-multi-swoop-all-from-helm-swoop)

  ;; Save buffer when helm-multi-swoop-edit complete
  ;; (setq helm-multi-swoop-edit-save t)

  ;; If this value is t, split window inside the current window
  ;; (setq helm-swoop-split-with-multiple-windows nil)

  ;; Split direction. 'split-window-vertically or 'split-window-horizontally
  ;; (setq helm-swoop-split-direction 'split-window-vertically)

  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color nil) ;use color. Worth the small delay.
  ;; (setq helm-swoop-speed-or-color t) ;use color. Worth the small delay.
  )

;;;----------------------------------------------------------------------------
;;; sublimity
;;;----------------------------------------------------------------------------
;; (with-eval-after-load "sublimity"
;;   (setq sublimity-scroll-drift-length 4)
;;   (setq sublimity-scroll-weight 8)
;;   (require 'sublimity-scroll)
;;   ;; map is annoying;; (require 'sublimity-map)
;;   )

;;;----------------------------------------------------------------------------
;;; Clippy. pop-up help
;;;----------------------------------------------------------------------------
;; (when my-use-evil-p
;;   (evil-leader/set-key "c" #'clippy-describe-function)
;;   (evil-leader/set-key "v" #'clippy-describe-variable))

;; (evil-leader/set-key "n"
;;   (lambda ()
;;     (interactive)
;;     (clippy-say
;;      (concat "It looks like you want to know more about the [search "
;;              "function]. Please use the [search function] to find out more "
;;              "about the [search function]. The [search function] provides "
;;              "access to more a more detailed description than can be "
;;              "provided in tool-tip help.") t)))

;; (evil-leader/set-key "n"
;;   (lambda (variable)
;;     (interactive (list (variable-at-point)))
;;     (if (not (null variable))
;;         (clippy-say
;;          (format
;;           (concat "It looks like you want to know more about [%s]. Please "
;;                   "use the [search function] to find out more about [%s]. "
;;                   "The [search function] provides access to more a more "
;;                   "detailed description than can be provided in tool-tip "
;;                   "help.")
;;           variable
;;           variable)
;;          t))))


;;;----------------------------------------------------------------------------
;;; icomplete, fido
;;;----------------------------------------------------------------------------
(when (eq my-narrow-type 'icomplete)
  (icomplete-mode 1)
  (when my-use-evil-p
    (evil-leader/set-key "b" #'switch-to-buffer)))

;; Fido is just icomplete, but configured to work similar to ido.
(when (eq my-narrow-type 'fido)
  (fido-mode 1)
  (when my-use-evil-p
    (evil-leader/set-key "b" #'switch-to-buffer)))

(with-eval-after-load 'icomplete
  ;; redefine the set up fn to avoid flex matching
  (defun icomplete--fido-mode-setup ()
    "Setup `fido-mode''s minibuffer."
    (when (and icomplete-mode (icomplete-simple-completing-p))
      (use-local-map (make-composed-keymap icomplete-fido-mode-map
                                           (current-local-map)))
      (setq-local icomplete-tidy-shadowed-file-names t
                  icomplete-show-matches-on-no-input t
                  icomplete-hide-common-prefix nil
                  completion-styles '(basic
                                      partial-completion
                                      emacs22
                                      substring)
                  completion-flex-nospace nil
                  completion-category-defaults nil)))

  (setq icomplete-compute-delay 0))

;;;----------------------------------------------------------------------------
;;; smex. used by ido, ivy
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/smex" load-path)
(autoload #'smex "smex" nil t)
(autoload #'smex-major-mode-commands "smex" nil t)
(autoload #'smex-initialize "smex" nil t)

(with-eval-after-load 'smex
  ;; GUARD: smex is used for `counsel-M-x' too where this advice is not needed.
  (when (or my-use-fancy-ido-p
            my-use-grid-ido-p
            my-use-bare-ido-p)
    ;; insert a hyphen - on space like in normal M-x
    (defadvice smex (around space-inserts-hyphen activate compile)
      (let ((ido-cannot-complete-command
             `(lambda ()
                (interactive)
                (if (string= " " (this-command-keys))
                    (insert ?-)
                  (funcall ,ido-cannot-complete-command)))))
        ad-do-it))))

;;;----------------------------------------------------------------------------
;;; flx
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/flx" load-path)
(autoload #'flx-ido-mode "flx-ido" nil t)

;;;----------------------------------------------------------------------------
;;; ido
;;; ido-vertical-mode
;;; ido-ubiquitous
;;; flx-ido
;;;----------------------------------------------------------------------------
(when (or my-use-fancy-ido-p
          my-use-grid-ido-p
          my-use-bare-ido-p)
  ;; ;; icomplete's display is similar to ido. So use it for completions ido
  ;; ;; does not support. (ie `describe-function' `load-theme' etc)
  ;; (icomplete-mode 1)

  (setq ido-enable-flex-matching nil)
  (setq ido-everywhere t)
  (setq ido-show-dot-for-dired nil) ; prefer history not folder.
                                    ; use C-d for dired
  ;; TODO: figure out why it's still prompting for new buffer creation even
  ;; when `ido-create-new-buffer' is set to 'always.
  (setq ido-create-new-buffer 'always)
  (ido-mode 1) ;;autoloaded function. turn on ido.

  (when my-use-evil-p
    (evil-leader/set-key "b" #'ido-switch-buffer))

  ;; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
  ;;                   ; when Smex is auto-initialized on its first run.
  (global-set-key (kbd "M-x") #'smex)
  (global-set-key (kbd "M-X") #'smex-major-mode-commands)
  (global-set-key (kbd "C-c M-x")
                  #'execute-extended-command) ; rebind the original M-x command
  )

(with-eval-after-load 'ido

  ;; (setf (nth 0 ido-decorations) nil)
  ;; (setf (nth 1 ido-decorations) nil)
  ;; (setf (nth 2 ido-decorations) "\n") ; vertical style
  ;; (setf (nth 3 ido-decorations) " ...")

  (defun my-ido-find-file ()
    "Calls `ido-find-file'.
But shadows `ido-work-directory-list' to prevent ido from brining in
completions from folders other than the current one."
    (interactive)
    (let ((ido-work-directory-list '())
          (ido-grid-rows 0.8)) ; taller ido-grid completion window for files.
      (ido-find-file)))

  (when (or my-use-fancy-ido-p
            my-use-grid-ido-p
            my-use-bare-ido-p)
    (global-set-key (kbd "C-x C-f") #'my-ido-find-file))

  (when my-use-fancy-ido-p ;; GUARD against calling ido-ubiquitous-mode.

    (ido-ubiquitous-mode 1)
    ;; NOTE: i removed some un-wanted advice code from the autoloads file of
    ;; `ido-completing-read+' (a dependency of `ido-ubiquitous-mode'). Because
    ;; it forced a load of ido automatically at start up, even when I'm not
    ;; using ido!!!
    ;; ALWAYS-DO: periodically monitor package `ido-completing-read+' after
    ;; updates, and remove the un-wanted code in the autoload file.

    ;; TODO: figure out what is loading ido on start up. it's forcing me
    ;;       to include this block of code in the guard.
    (let ((my-ido-display 'vertical)) ;; Choices: 'grid 'vertical nil
      (cond ((eq my-ido-display 'vertical)
             ;; 3rd party extension to ido. Display vertically like swiper.
             ;; invokes with-eval-after-load "ido-vertical-mode"
             (ido-vertical-mode 1))
            ((eq my-ido-display 'grid)
             ;; TODO: prevent grid mode from messing up the "space-as-dash"
             ;;       advice.
             ;; TODO: make Tab behave the same for smex, general completion,
             ;;       etc.
             (ido-grid-mode)))))

  ;; (flx-ido-mode 1) ;; invokes (with-eval-after-load "flx-ido")

  (when my-use-evil-p
    ;; reclaim M-k keybind. TODO: rebind `ido-forget-work-directory'
    (define-key ido-file-dir-completion-map (kbd "M-k") #'evil-window-up))

  ;; insert a hyphen - on space like in normal M-x
  (defadvice ido-switch-buffer (around space-inserts-hyphen activate compile)
    (let ((ido-cannot-complete-command
           `(lambda ()
              (interactive)
              (if (string= " " (this-command-keys))
                  (insert ?-)
                (funcall ,ido-cannot-complete-command)))))
      ad-do-it)))

(with-eval-after-load 'flx-ido
  ;; disable ido faces to see flx highlights.
  (setq ido-use-faces nil))

(with-eval-after-load 'ido-vertical-mode
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-vertical-show-count t))

;;;----------------------------------------------------------------------------
;;; ido-grid. The successor to ido-grid-mode (same author?)
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/ido-grid.el" load-path)
(autoload #'ido-grid-enable "ido-grid" nil t)

(with-eval-after-load 'ido-grid
  (setq ido-grid-bind-keys nil)
  (setq ido-grid-rows 20) ; Float for %. Whole number for exact num.
  (setq ido-grid-max-columns nil) ; as many as can fit on screen
  ;; not showing the grid sort of defeats the purpose.
  (setq ido-grid-start-small nil)


  (defun my-ido-grid-page-down ()
    (interactive)
    (ido-grid--shift (* ido-grid--rows
                        ido-grid--cols)))
  (defun my-ido-grid-page-up ()
    (interactive)
    (ido-grid--shift (- (* ido-grid--rows
                           ido-grid--cols))))

  (defun my-ido-grid-keybinds-hook ()
    "Normally I would not set keybinds in a hook as it executes repeatedly.
But ido-grid is weird and only lets you set keybinds on the fly?"
    ;; default keybinds from the author
    ;; (setq ido-grid--prior-ccc ido-cannot-complete-command
    ;;       ido-cannot-complete-command #'ido-grid-down)
    (define-key ido-completion-map (kbd "<right>") #'ido-grid-right)
    (define-key ido-completion-map (kbd "<left>")  #'ido-grid-left)
    (define-key ido-completion-map (kbd "<up>")    #'ido-grid-up-or-expand)
    (define-key ido-completion-map (kbd "<down>")  #'ido-grid-down-or-expand)
    (define-key ido-completion-map (kbd "C-<up>")  #'ido-grid-display-more-rows)
    (define-key ido-completion-map (kbd "C-p")     #'ido-grid-up)
    (define-key ido-completion-map (kbd "C-n")     #'ido-grid-down)

    ;; vim-like keybinds
    (define-key ido-completion-map (kbd "C-l") #'ido-grid-right)
    (define-key ido-completion-map (kbd "C-h")  #'ido-grid-left)
    ;; classic ido keybinds
    (define-key ido-completion-map (kbd "C-s") #'ido-grid-right)
    (define-key ido-completion-map (kbd "C-r")  #'ido-grid-left)
    ;; paging. With the "wrap around" style UI this doesn't work quite as
    ;; wanted. But sort of works, so going with it.
    (define-key ido-completion-map (kbd "M->") #'my-ido-grid-page-down)
    (define-key ido-completion-map (kbd "M-<") #'my-ido-grid-page-up)
    )
  (add-hook 'ido-setup-hook #'my-ido-grid-keybinds-hook)
  ;; TODO: maybe remove this hook when ido-grid is disabled.
  ;;       (remove-hook 'ido-setup-hook #'my-ido-grid-keybinds-hook)


  ;; insert a hyphen - on space like in normal M-x
  (defadvice ido-grid--completions (around space-inserts-hyphen activate compile)
    (let ((ido-cannot-complete-command
           `(lambda ()
              (interactive)
              (if (string= " " (this-command-keys))
                  (insert ?-)
                (funcall ,ido-cannot-complete-command)))))
      ad-do-it))
  (ad-activate 'ido-grid--completions))

(when my-use-grid-ido-p
  (ido-grid-enable))


;;;----------------------------------------------------------------------------
;;; Yasnippet
;;;----------------------------------------------------------------------------
;; ;;(add-to-list 'load-path "~/.emacs.d/yasnippet")


(progn
  ;; an older copy of yasnippet before it caused unwanted indentation on
  ;; each keystroke.
  ;; (push "~/.emacs.d/yasnippet-20160416.831_correctIndent" load-path)

  ;; using a recent version of yasnippet again. Things seem to work now.
  (push "~/.emacs.d/notElpa/yasnippet" load-path)

  (progn
    ;; manually add the autoloads because I'm not using the package manager
    ;; for yasnippet anymore.
    (autoload 'yas-minor-mode "yasnippet" nil t nil)
    (defvar yas-global-mode nil)
    (custom-autoload 'yas-global-mode "yasnippet" nil)
    (autoload 'yas-global-mode "yasnippet" nil t nil)
    (autoload 'snippet-mode "yasnippet" nil t nil)))



;;(require 'yasnippet)
;;(yas-global-mode 0)
;; (autoload 'yasnippet "yasnippet" "yasnippet mode" t)
;; (autoload #'snippet-mode "yasnippet" "A mode for editing yasnippets" t)

(with-eval-after-load 'yasnippet
  ;; so custom snippets are not overwritten when updating from melpa.
  (yas-load-directory "~/.emacs.d/snippets" t)

  ;; (setq yas-snippet-dirs
  ;;       `("~/.emacs.d/snippets" ; personal snippets
  ;;         ;; "/path/to/some/collection/" ; foo-mode and bar-mode snippet
  ;;         ;;                             ; collection
  ;;         ;; "/path/to/yasnippet/yasmate/snippets" ; the yasmate collection
  ;;         ,yas-installed-snippets-dir ; the default collection
  ;;         ))

  ;; Enable/disable trigger of a sub-snippet while in a snippet.
  (setq yas-triggers-in-field nil)

  (defun my-yas-handle-param (param-str
                              sep-char
                              fn-deco
                              fn-fix-first
                              fn-fix-last)
    "Does something special for each parameter in a snippet."
    (let* ((split (split-string param-str sep-char))
           (decorated (mapcar fn-deco split)))
      (setcar decorated (funcall fn-fix-first (car decorated)))
      (setf (nthcdr (- (length decorated) 1) decorated)
            (cons (funcall fn-fix-last (car (last decorated)) ) nil))
      (apply #'concat decorated))))

;; (my-yas-handle-param "first, middle1, middle2, last"
;;                      ","
;;                      (lambda (x)
;;                        (upcase (concat "'" x "' - ")))
;;                      (lambda (f)
;;                        (downcase f))
;;                      (lambda (l)
;;                        (concat l "|")))
;; "'first' - ' MIDDLE1' - ' MIDDLE2' - ' LAST' - |"



;;;----------------------------------------------------------------------------
;;; my auto-newline functions. used by cc-mode and electric-spacing-mode.
;;;----------------------------------------------------------------------------
(defvar my-auto-newline-p nil
  "Use my custom auto newline functionality if t.  For cc mode.")

(defun my-search-line-backwards (str)
  (interactive)
  (let ((line-start (save-excursion
                      (beginning-of-line 1)
                      (point))))
    (save-excursion
      (search-backward str line-start t))))

(cl-defun my-next-char-}-p ()
  "Return t if the first non-whitespace char after point is }.
Also only return t if the } is relatively close to (point)."
  (interactive)
  ;; Search a max of 200 chars forward (or less if near end of buffer).
  (let* ((distance-until-end (- (buffer-size) (point)))
         (end (min 200 distance-until-end)))
    (cl-loop named loop for i from (point) to end
             do
             (let ((c (byte-to-string (char-after i))))
               ;; unless whitespace
               (unless (or (string-equal c " ")
                           (string-equal c "	")
                           (string-equal c "\n"))
                 ;; (return (string-equal c "}"))
                 (cl-return-from my-next-char-}-p (string-equal c "}")))))))

(defun my-add-newline-automatically-p ()
  (interactive)
  (if (or (my-search-line-backwards "return")
          (my-next-char-}-p))
      'stop
    'yes-add-newline))

;;;----------------------------------------------------------------------------
;;; cc-mode
;;;----------------------------------------------------------------------------
;; TODO: wire up static analyzer and ASAN, UBSAN, TSAN

;; This fn is useful for aligning trailing comments when using tabs for
;; indentation.  It won't work if different numbers of tabs are used within the
;; aligned set of comments. But that case (different tab level) should be rare
;; as a different tab level is a different block of logic, so you wouldn't have
;; a set of comments span across it.
(defun my-comment-dwim-align-with-spaces ()
  "Temporarily use spaces while making the comments.
It will still use tabs for left-side indentation.
Useful for aligning trailing comments when using tabs for indentation, spaces
for alignment.  Doesn't solve all comment alignment issues but helps in a few
cases."
  (interactive)
  (let ((indent-tabs-mode nil)) ; spaces
    (call-interactively #'comment-dwim)))


;; TODO: incorpate ref docs from:
;; https://en.cppreference.com/w/
(when (eq my-curr-computer 'work-laptop)
  (defun my-c-reference ()
    "Opens the function list of the GNU c docs."
    (interactive)
    (split-window-sensibly)
    (eww-open-file
     (concat
      "C:/Users/mtz/programs/libc-html_node/libc/"
      "Function-Index.html#Function-Index"))))

(with-eval-after-load 'cc-mode
  ;; (assoc "cc-mode" c-style-alist)
  ;; (assoc "user" c-style-alist)
  ;; (assoc "c#" c-style-alist)
  ;; (assoc "gnu" c-style-alist)
  ;; (assoc "k&r" c-style-alist)
  ;; (assoc "bsd" c-style-alist)
  ;; (assoc "stroustrup" c-style-alist)
  ;; (assoc "whitesmith" c-style-alist)
  ;; (assoc "ellemtel" c-style-alist)
  ;; (assoc "linux" c-style-alist)
  ;; (assoc "python" c-style-alist)
  ;; (assoc "java" c-style-alist)
  ;; (assoc "awk" c-style-alist)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "linux")))
  ;;(setq-default c-default-style "java")

  ;; NOTE: sometimes I use a different tab-width for c
  (defvar my-indent-width-c my-indent-width)
  (setq-default c-basic-offset my-indent-width-c) ;tab width
  (setq-default c-electric-flag t)
  (setq-default c-electric-pound-behavior '(alignleft))

  ;; custom function for trailing comment alignment. (useful when using tabs)
  (define-key c-mode-map (kbd "M-;") #'my-comment-dwim-align-with-spaces)
  (define-key c++-mode-map (kbd "M-;") #'my-comment-dwim-align-with-spaces)

  ;; switch between .c/.h file.
  (let ((key (kbd "C-c f")))
    (define-key c-mode-map key #'ff-find-other-file)
    (define-key c++-mode-map key #'ff-find-other-file))

  ;; compile
  (define-key c-mode-map (kbd "C-c C-c") #'compile)
  (define-key c++-mode-map (kbd "C-c C-c") #'compile)

  (defun my-linux-tabs-toggle ()
    "Choose a tabbing style.
The variables set are buffer local.
Call with a C-u prefix to modify the buffer via `tabify' or `untabify'
and indent."
    (interactive)
    (let ((mutate-buffer-p (equal current-prefix-arg '(4)))
          (tab-style (intern (completing-read "tabbing style: "
                                              '("mine" "linux")))))
      (cond ((eq tab-style 'mine)
             (setq c-basic-offset 4)
             (setq tab-width 4)
             (setq indent-tabs-mode nil)
             (when mutate-buffer-p
               ;; (mark-whole-buffer)
               ;; (call-interactively #'untabify)
               (untabify (point-min) (point-max))))
            ((eq tab-style 'linux)
             (setq c-basic-offset 8)
             (setq tab-width 8)
             (setq indent-tabs-mode t)
             (when mutate-buffer-p
               ;; (mark-whole-buffer)
               ;; (call-interactively #'tabify)
               (tabify (point-min) (point-max)))))
      (when mutate-buffer-p
        (indent-region (point-min) (point-max)))))

  ;; `which-function-mode' is OK, but it turns on the mode globally for all
  ;; buffers which is annoying. And if the function fits on screen then it's
  ;; just wasted modeline space. As an alternative use `beginning-of-defun'
  ;; C-M-a to jump to the function name. Then `evil-jump-backward' C-o to jump
  ;; back to where you were.
  ;;(eval-after-load 'cc-mode 'which-function-mode)

  (when my-auto-newline-p
    ;; TODO: look into a way to add to this list. For some reason it's a sybmol
    ;; by default, not a list. So doing a heavy handed overwrite for now.
    (setq c-hanging-semi&comma-criteria '(my-add-newline-automatically-p)))

  (defun my-setup-c-mode-common ()
    ;; highlight escape characters in strings
    ;; see https://emacs.stackexchange.com/questions/2508/highlight-n-and-s-ins
    ;; ide-strings
    (highlight-regexp "[%\\][[:alpha:]]" 'font-lock-builtin-face)

    (yas-minor-mode 1)
    ;; (which-function-mode) ; displays function at cursor in the
    ;;                       ; mode-line. But can be annoying.
    (my-turn-on-electric-pair-local-mode)

    ;; highlight escapes in the printf format string.
    ;; TODO: highlight placeholders %d differently than escapes \n
    ;; (highlight-regexp "%[[:alpha:]]\\|\\\\[[:alpha:]]")

    ;; set to 1 so comments on the same line are kept close to the
    ;; code by default.
    (setq comment-column 1)             ; buffer local

    (when nil ; don't turn on flycheck for now.
      (unless (eq system-type 'windows-nt)
        ;; sometime in early March 2016, flycheck became very slow on
        ;; Windows for C.
        ;; TODO: find the problem, fix it. Commit upstream if relevant.
        (flycheck-mode 1)))

    ;; NOTE: only enable `electric-spacing-mode' if ";" is removed from
    ;; `electric-spacing-operators'. Otherwise it duplicates newline
    ;; functionality of cc mode's auto-newline.
    ;; (electric-spacing-mode 1)
    (c-toggle-hungry-state 1)
    ;; (c-toggle-auto-newline 1)

    ;; (when (fboundp #'fci-mode) ; NOTE: using the new native alternative.
    ;;   (fci-mode 1))
    (when (fboundp #'display-fill-column-indicator-mode)
      (setq display-fill-column-indicator-column 79)
      (display-fill-column-indicator-mode 1)))
  (add-hook 'c-mode-common-hook #'my-setup-c-mode-common)

  (defun my-setup-c-mode ()
    ;; (when my-graphic-p
    ;;   (highlight-indent-guides-mode 1))

    (progn ;; use linux style tabbing/indentation for C
      ;; these values should be buffer local.
      (setq c-basic-offset my-indent-width-c)
      (setq tab-width my-indent-width-c)
      ;; TODO: solve issue of snippets using spaces while I'm using
      ;;       tabs for C.
      (setq indent-tabs-mode t))

    (progn ;; smart-tabs-mode
      (smart-tabs-advice c-indent-line c-basic-offset)
      (smart-tabs-advice c-indent-region c-basic-offset)
      (smart-tabs-mode-enable)))
  (add-hook 'c-mode-hook #'my-setup-c-mode)

  (defun my-setup-c++-mode ()
    (progn ;; use linux style tabbing/indentation.
      (c-set-offset 'innamespace [0]) ;; disable namesapce indentation.
      ;; these values should be buffer local.
      (setq c-basic-offset my-indent-width-c)
      (setq tab-width my-indent-width-c)
      (setq indent-tabs-mode t))

    (progn ;; smart-tabs-mode
      (smart-tabs-advice c-indent-line c-basic-offset)
      (smart-tabs-advice c-indent-region c-basic-offset)
      (smart-tabs-mode-enable)))
  (add-hook 'c++-mode-hook #'my-setup-c++-mode)

  (defvar my-use-tabs-java-p nil)
  (defun my-setup-java-mode ()
    (progn ;; tab/indent stuff
      (setq indent-tabs-mode my-use-tabs-java-p)
      ;; NOTE: `tab-width' and `c-basic-offset' must be the same.
      (setq c-basic-offset my-indent-width-c
            tab-width      my-indent-width-c)
      (when my-use-tabs-java-p ;; smart-tabs-mode
        (smart-tabs-advice c-indent-line c-basic-offset)
        (smart-tabs-advice c-indent-region c-basic-offset)
        (smart-tabs-mode-enable)))

    (when (fboundp #'display-fill-column-indicator-mode)
      (setq display-fill-column-indicator-column 110) ; long lines in java.
      (display-fill-column-indicator-mode 1)))
  (add-hook 'java-mode-hook #'my-setup-java-mode)


  ;; (add-hook 'c-initialization-hook
  ;;           (lambda ()
  ;;             ;;TODO: fill this up
  ;;             ;; hook that runs 1 time.
  ;;             ;; equivalent to using eval-after-load???
  ;;             ))

  ;; (defun my-make-CR-do-indent ()
  ;;   (define-key c-mode-base-map "\C-m" 'c-context-line-break))
  ;; (add-hook 'c-initialization-hook 'my-make-CR-do-indent)
  )

;;;----------------------------------------------------------------------------
;;; c-ts-mode
;;;----------------------------------------------------------------------------
;; prefer c-ts-mode if it's available.
(when (treesit-language-available-p 'c)
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode)))

(with-eval-after-load 'c-ts-mode
  (setq c-ts-mode-indent-offset 4)
  (setq c-ts-mode-indent-style 'linux)

  ;; key binds
  (define-key c-ts-mode-map (kbd "C-c C-c") #'compile)
  (define-key c-ts-mode-map (kbd "C-c c") #'compile)
  ;; switch between .c/.h file.
  (define-key c-ts-mode-map (kbd "C-c f") #'ff-find-other-file)

  (defun my-setup-c-ts-mode ()
    (yas-minor-mode 1)
    (my-turn-on-electric-pair-local-mode)
    (setq comment-column 1) ;; buffer local
    (setq tab-width 4) ;; buffer local
    (indent-tabs-mode 1)
    (rainbow-delimiters-mode-enable))
  (add-hook 'c-ts-mode-hook #'my-setup-c-ts-mode))

;;;----------------------------------------------------------------------------
;;; Dired
;;;----------------------------------------------------------------------------
(with-eval-after-load 'dired ; dired -> dired.el in `load-path'
  (setq-default dired-isearch-filenames t) ;search file names only in Dired.
  (defun my-setup-dired ()
    (when (fboundp #'dired-hide-details-mode) ;; avoid break on older emacs
      (dired-hide-details-mode 1)))
  (add-hook 'dired-mode-hook #'my-setup-dired)

  (defun my-dired-create-directory ()
    "Same as `dired-create-directory' but prevent ido from giving junk."
    (interactive)
    ;; shadow `ido-work-directory-list' so ido doesn't pull in suggestions
    ;; from irrelevant folders.
    (let ((ido-work-directory-list '()))
      (call-interactively #'dired-create-directory)))

  (define-key dired-mode-map (kbd "+") #'my-dired-create-directory)

  (define-key dired-mode-map (kbd "C-o") #'dired-up-directory)
  (when my-use-evil-p
    ;; vimify the keybinds.
    (define-key dired-mode-map (kbd "j") #'dired-next-line)
    (define-key dired-mode-map (kbd "k") #'dired-previous-line)
    (define-key dired-mode-map (kbd "w") #'evil-forward-word-begin)
    (define-key dired-mode-map (kbd "e") #'evil-forward-word-end)
    (define-key dired-mode-map (kbd "n") #'evil-search-next)
    (define-key dired-mode-map (kbd "N") #'evil-search-previous)
    (define-key dired-mode-map (kbd "H") #'evil-window-top)
    (define-key dired-mode-map (kbd "M") #'evil-window-middle)
    (define-key dired-mode-map (kbd "L") #'evil-window-bottom)
    (evil-define-key 'normal dired-mode-map (kbd "s") my-swoop-fn)
    (define-key dired-mode-map (kbd "SPC") #'avy-goto-word-1)
    (define-key dired-mode-map (kbd "G") #'evil-goto-line)

    ;; re-bind the default bindings we clobbered.
    (define-key dired-mode-map (kbd "C-c w") #'dired-copy-filename-as-kill)
    (define-key dired-mode-map (kbd "C-c e") #'dired-find-file)
    (define-key dired-mode-map (kbd "C-c N") #'dired-man)
    (define-key dired-mode-map (kbd "C-c H") #'dired-do-hardlink)
    (define-key dired-mode-map (kbd "C-c M") #'dired-do-chmod)
    (define-key dired-mode-map (kbd "C-c L") #'dired-do-load)
    (define-key dired-mode-map (kbd "C-c s") #'dired-sort-toggle-or-edit)
    (define-key dired-mode-map (kbd "C-c SPC") #'dired-next-line)
    (define-key dired-mode-map (kbd "C-c G") #'dired-do-chgrp)))

;;(define-key dired-mode-map "c" 'find-file) ;create file

;; ;; disable the disable status of function `dired-find-alternate-file'
;; (put 'dired-find-alternate-file 'disabled nil)

;; (progn
;;   ;; use the same buffer when navigating folders in dired. And use the same
;;   ;; buffer when opening a file in dired.
;;   (define-key dired-mode-map (kbd "RET")
;;     'dired-find-alternate-file)       ; was dired-advertised-find-file
;;   (define-key dired-mode-map (kbd "^")
;;     (lambda ()
;;       (interactive)
;;       (find-alternate-file "..")))) ; was dired-up-directory

;;;----------------------------------------------------------------------------
;;; dired-details
;;;----------------------------------------------------------------------------
;; allows collapsing the file details with "(" and ")" in emacs <= 24.3

;; only use dired-details on older Emacs versions. If available use the built
;; in function `dired-hide-details-mode' instead.
(with-eval-after-load 'dired
  ;; NOTE: `unless' guard does not work when wrapping `with-eval-after-load'.
  ;; So putting guard inside `with-eval-after-load' instead.
  ;; Maybe something to do with the code running at macro expansion time?
  (unless (fboundp #'dired-hide-details-mode)
    (require 'dired-details)
    (dired-details-install)))


;;;----------------------------------------------------------------------------
;;; sql-indent
;;;----------------------------------------------------------------------------
(with-eval-after-load 'sql-indent
  (setq-default sqlind-basic-offset 4)
  (setq-default sqlind-indentation-offsets-alist
                `((select-clause 0)
                  (insert-clause 0)
                  (delete-clause 0)
                  (update-clause 0)
                  (select-table-continuation 0)
                  (in-select-clause 0)
                  ,@sqlind-default-indentation-offsets-alist)))

;;;----------------------------------------------------------------------------
;;; sql-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'sql
  (autoload #'s-trim "my-misc" nil nil) ; used by snippet "ins"

  (setq sql-product 'ms) ; using sql server at the moment.

  (progn
    ;; some SqlServer related keyboard macros to help prepare a generated insert
    ;; statement. Just textual manipulations. Ad hoc.
    (fset 'my-kbd-sql-delete-brackets
          (kmacro-lambda-form [?/ ?i ?n ?s ?e ?r ?t return ?v ?/ ?\) return ?\M-x ?m ?y ?- ?d ?e ?l ?e ?t ?e ?- ?b ?r ?e backspace ?a ?c ?k ?e ?t ?s return] 0 "%d"))

    (fset 'my-kbd-sql-move-stuff-left
          (kmacro-lambda-form [?/ ?v ?a ?l ?u ?e ?s return ?0 ?D ?i ?s ?e ?l ?e ?c ?t escape ?j ?w ?v ?% ?3 ?< ?% ?x ?? ?s ?e ?l ?e ?c ?t return ?j ?x] 0 "%d"))

    (fset 'my-kbd-sql-fix-col-1
          (kmacro-lambda-form [?x ?i ?\C-q ?\[ escape ?/ ?, return ?i ?\C-q ?\] escape ?l ?l ?D ?j ?0] 0 "%d"))

    (fset 'my-kbd-sql-fix-col-n
          (kmacro-lambda-form [?x ?x ?i ?\C-q ?\[ escape ?/ ?, return ?i ?\C-q ?\] escape ?l ?l ?D ?j ?0] 0 "%d"))


    (defun my-prepare-sql-insert ()
      (interactive)
      "Run several pre-recorded keyboard macros to prepare a sql insert
statement generated my SqlServer."
      (my-kbd-sql-delete-brackets)
      (my-kbd-sql-move-stuff-left)
      (my-kbd-sql-fix-col-1)
      (cl-loop repeat 3 do
               (my-kbd-sql-fix-col-n)))

    (defun my-sql-fix-n ()
      (interactive)
      (let ((reps (if (null current-prefix-arg)
                      1
                    current-prefix-arg)))
        (cl-loop repeat reps do
                 (my-kbd-sql-fix-col-n)))))



  (defun my-setup-sql ()
    (my-turn-on-electric-pair-local-mode)
    (yas-minor-mode 1)
    ;; electric-indent doesn't work very well with T-sql.
    ;; use C-j for newline and indent.
    (when (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1))
    ;; turn off indent when you press "o" in evil. Buffer local
    (when my-use-evil-p
      (setq evil-auto-indent nil))
    ;; (sqlind-minor-mode 1)
    (when (fboundp #'display-fill-column-indicator-mode)
      (setq display-fill-column-indicator-column 100)
      (display-fill-column-indicator-mode 1)))

  (add-hook 'sql-mode-hook #'my-setup-sql)

  ;; ;;experiment to handle annoying indents.
  ;; (when nil
  ;;   (defun my-delete-region (start end)
  ;;     (interactive "r")
  ;;     (delete-region)
  ;;     (deactivate-mark))
  ;;   ;; augment the backspace to handle the annoying indentation sql-mode
  ;;   ;; gives.
  ;;   (evil-define-key 'insert sql-mode-map (kbd "<backspace>")
  ;;     (lambda ()
  ;;       (interactive)
  ;;       (set-mark-command nil)
  ;;       (evil-backward-word-begin)
  ;;       (evil-forward-word-end)
  ;;       (evil-forward-char)
  ;;       (call-interactively #'my-delete-region))))
  )


;;;----------------------------------------------------------------------------
;;; rainbow-delimiters
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/rainbow-delimiters" load-path)
(autoload #'rainbow-delimiters-mode "rainbow-delimiters" nil t)
(autoload #'rainbow-delimiters-mode-enable "rainbow-delimiters" nil t)
(autoload #'rainbow-delimiters-mode-disable "rainbow-delimiters" nil t)

(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'slime-repl-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'ielm-mode-hook #'rainbow-delimiters-mode-enable)

;; this doesn't work for mini buffer? TODO: look into it.
;; (add-hook 'eval-expression-minibuffer-setup-hook
;;           #'rainbow-delimiters-mode-enable)

;; (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'sql-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'c-mode-common-hook #'rainbow-delimiters-mode-enable)
;; (add-hook 'sly-mrepl-mode-hook #'rainbow-delimiters-mode-enable)
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable)
;;(global-rainbow-delimiters-mode)

;;;----------------------------------------------------------------------------
;;; rainbow-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/rainbow-mode" load-path)
(autoload #'rainbow-mode "rainbow-mode" nil t)

;;;----------------------------------------------------------------------------
;;; expand-region
;;; https://github.com/magnars/expand-region.el
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/expand-region.el" load-path)
(autoload #'er/expand-region "expand-region" nil t)
(autoload #'er/contract-region "expand-region-core" nil t)
(global-set-key (kbd "C-=") #'er/expand-region)
(global-set-key (kbd "C--") #'er/contract-region)

(autoload #'hydra-expand-region/body "my-hydras" nil t)
(global-set-key (kbd "C-c k") #'hydra-expand-region/body)

;;;----------------------------------------------------------------------------
;;; multiple-cursors
;;; https://github.com/magnars/multiple-cursors.el
;;;----------------------------------------------------------------------------
;;(require 'multiple-cursors)
;;(global-set-key (kbd "C--") 'mc/edit-lines)

;;;----------------------------------------------------------------------------
;;; Paredit
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/paredit" load-path)
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook #'enable-paredit-mode)
;;(add-hook 'sly-mrepl-mode-hook (lambda () (paredit-mode +1)))
;;(add-hook 'sql-mode-hook #'enable-paredit-mode)


(with-eval-after-load 'paredit
  ;; remove the new "RET" key behavior added to paredit. Seems to break indent and
  ;; in the minibuffer it eats the return key so I can't press RET to eval.
  ;; TODO: look into the "proper" way to fix this.
  (define-key paredit-mode-map (kbd "RET") nil)
  (define-key paredit-mode-map (kbd "C-j") nil)

  (defun my-setup-paredit ()
    ;; Don't disalbe electric-indent for now
    ;; ;; Paredit and electric-indent are incompatible. So turn off electric-ident
    ;; ;; when turning on paredit.
    ;; (when (and (boundp 'electric-indent-mode)
    ;;            (fboundp #'electric-indent-local-mode)
    ;;            electric-indent-mode)
    ;;   (electric-indent-local-mode -1))
    )
  (add-hook 'paredit-mode-hook #'my-setup-paredit)

  ;; Stop SLIME's REPL from grabbing DEL,
  ;; which is annoying when backspacing over a '('
  (defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
                (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

  ;; barf/slurp keybinds
  (define-key paredit-mode-map (kbd "C-9") #'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-0") #'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-M-9") #'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-M-0") #'paredit-backward-barf-sexp)
  ;; don't allow paredit to steal the M-r keybind.
  (define-key paredit-mode-map (kbd "M-r") #'my-cycle-line-position)
  ;; rebind `paredit-raise-sexp' to C-M-r
  (define-key paredit-mode-map (kbd "C-M-r") #'paredit-raise-sexp)

  (when (eq my-ui-type 'emacs)
    ;; reclaim the M-r binding to move cursor to middle, high, low
    (define-key paredit-mode-map (kbd "M-r") #'move-to-window-line-top-bottom)
    ;; rebind paredits raise function
    (define-key paredit-mode-map (kbd "C-c M-r") #'paredit-raise-sexp))

  (defun my-paredit-view-docs ()
    "View paredit examples in a new buffer."
    (interactive)
    (let ((paredit-buff "*paredit examples*"))
      (when (get-buffer paredit-buff)
        (kill-buffer paredit-buff))
      (switch-to-buffer paredit-buff)
      (with-current-buffer paredit-buff
        (cl-loop
         for vars in paredit-commands
         with header-fmt =
(concat
 ";;;--------------------------------------------------------------------\n"
 ";;; %s\n"
 ";;;--------------------------------------------------------------------\n\n")
         do
         (cond
          ;; string headers
          ((stringp vars)
           (insert (format header-fmt vars)))
          ;; examples
          ((listp vars)
           (let ((keybinds (cl-first vars))
                 (cmd-name (cl-second vars))
                 (examples (cddr vars)))
             ;; cmd-name
             (insert (format ";;----------------------\n;; %s\n"
                             cmd-name))
             ;; key-binds
             (insert ";; keybinds: ")
             (if (stringp keybinds)
                 (insert (format "%s" keybinds))
               (cl-loop for k in keybinds
                        with thresh = (1- (length keybinds))
                        with i = 0
                        do
                        (insert k)
                        (when (< i thresh) ; avoid sep on last
                          (insert "     "))
                        (cl-incf i)))
             (insert "\n")
             ;; examples
             (cl-loop for e in examples ; example is a list of strings.
                      with i = 1 ; example index
                      do
                      (insert (format ";; example %d:\n" i))
                      (dolist (str e) ; before / after strings
                        (insert (format "%s\n\n" str)))
                      (insert "\n")
                      (cl-incf i))
             (insert "\n")))
          ;; something else?
          (t
           (insert
            "Unknown. Please review `my-paredit-view-docs'.\n"))))
        ;; turn on the relevant modes
        (emacs-lisp-mode)
        (enable-paredit-mode)
        ;; warp up to the top.
        (goto-char 0)))))

;; ;;key maps
;; (global-set-key (kbd "C-9") 'paredit-backward-slurp-sexp)
;; (global-set-key (kbd "C-0") 'paredit-forward-slurp-sexp)
;; (global-set-key (kbd "C-M-9") 'paredit-backward-barf-sexp)
;; (global-set-key (kbd "C-M-0") 'paredit-forward-barf-sexp)


;;;----------------------------------------------------------------------------
;;; smartparens
;;;----------------------------------------------------------------------------
;;(require 'smartparens-config)


;;;----------------------------------------------------------------------------
;;; Omnisharp
;;;----------------------------------------------------------------------------
(when (and t                ;nil ;; turn off omnisharp for the moment.
           (eq my-curr-computer 'work-laptop))

  ;; (add-hook 'csharp-mode-hook
  ;;           'omnisharp-mode) ; turn on automatically for C# files.

  (with-eval-after-load 'omnisharp

    ;; TODO; figure out why the M-. keybind is not setting. M-, seems to work.
    (define-key omnisharp-mode-map (kbd "M-.") #'omnisharp-go-to-definition)
    (define-key omnisharp-mode-map (kbd "M-,") #'pop-tag-mark)

    (when my-use-evil-p
      ;; Example evil-mode config

      ;; (evil-define-key 'insert omnisharp-mode-map
      ;;   (kbd "C-SPC") 'omnisharp-auto-complete);C-Space like Visual Studio
      ;; (define-key omnisharp-mode-map
      ;;   (kbd "C-SPC") ; C-Space like Visual Studio
      ;;   'omnisharp-auto-complete)

      (evil-define-key 'normal omnisharp-mode-map
        (kbd "g u") #'omnisharp-find-usages)
      (evil-define-key 'normal omnisharp-mode-map
        (kbd "g o") #'omnisharp-go-to-definition)
      (evil-define-key 'normal omnisharp-mode-map
        (kbd "g i") #'omnisharp-find-implementations)
      (evil-define-key 'normal omnisharp-mode-map
        (kbd "g r") #'omnisharp-run-code-action-refactoring)
      (evil-define-key 'normal omnisharp-mode-map
        (kbd "g f") #'omnisharp-fix-code-issue-at-point)
      (evil-define-key 'normal omnisharp-mode-map
        (kbd "g R") #'omnisharp-rename)

      ;; (evil-define-key 'normal omnisharp-mode-map
      ;;   (kbd ", i") 'omnisharp-current-type-information)
      ;; (evil-define-key 'insert omnisharp-mode-map
      ;;   (kbd ".") 'omnisharp-add-dot-and-auto-complete)
      ;; (evil-define-key 'normal omnisharp-mode-map
      ;;   (kbd ", n t") 'omnisharp-navigate-to-current-file-member)
      ;; (evil-define-key 'normal omnisharp-mode-map
      ;;   (kbd ", n s") 'omnisharp-navigate-to-solution-member)
      ;; (evil-define-key 'normal omnisharp-mode-map
      ;;   (kbd ", n f") 'omnisharp-navigate-to-solution-file-then-file-member)
      ;; (evil-define-key 'normal omnisharp-mode-map
      ;;   (kbd ", n F") 'omnisharp-navigate-to-solution-file)
      ;; (evil-define-key 'normal omnisharp-mode-map
      ;;   (kbd ", n r") 'omnisharp-navigate-to-region)
      ;; (evil-define-key 'normal omnisharp-mode-map
      ;;   (kbd "<f12>") 'omnisharp-show-last-auto-complete-result)
      ;; (evil-define-key 'insert omnisharp-mode-map
      ;;   (kbd "<f12>") 'omnisharp-show-last-auto-complete-result)
      ;; (evil-define-key 'normal omnisharp-mode-map
      ;;   (kbd ",.") 'omnisharp-show-overloads-at-point)
      )


    ;; Speed up auto-complete on mono drastically. This comes with the
    ;; downside that documentation is impossible to fetch.
    (setq omnisharp-auto-complete-want-documentation nil)

    (setq omnisharp--curl-executable-path
          "C:/Users/mtz/programs/curl-7.37.0-win64/bin/curl.exe")
    (setq
     omnisharp-server-executable-path
     "C:/Users/mtz/programs/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe")
    ;; windows doesn't like the C:\ root folder
    (setq omnisharp--windows-curl-tmp-file-path
          "C:/Users/mtz/omnisharp-curl-tmp.cs")
    (setq omnisharp-host "http://localhost:2000/")
    ;; (setq omnisharp-curl "curl.exe")
    ;; `(:command ,omnisharp--curl-executable-path)

    (let ((i-am-using-omnisharp t))
      (when i-am-using-omnisharp
        (with-eval-after-load 'company
          (push 'company-omnisharp company-backends))))

    ;; tab completion of parameters. acts weird
    (setq omnisharp-company-do-template-completion nil)
    (setq omnisharp-company-ignore-case t)

    (defun my-start-omnisharp-server (sln)
      "Starts omnisharp server with the correct cmd line string."
      (interactive)
      (start-process-shell-command
       "Omni-Server"
       (get-buffer-create "*Omni-Server*")
       (concat omnisharp-server-executable-path " -p 2000 -s " sln)))))


;;;----------------------------------------------------------------------------
;;; nyan-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/nyan-mode" load-path)
(autoload #'nyan-mode "nyan-mode" nil t)
;;(nyan-mode)
;;(setq nyan-wavy-trail nil)
;;(nyan-start-animation)


;;;----------------------------------------------------------------------------
;;; nyan-prompt
;;;----------------------------------------------------------------------------
;;(add-hook 'eshell-load-hook 'nyan-prompt-enable)

;;;----------------------------------------------------------------------------
;;; powerline  NOTE: powerline has an error on start up in emacs 24.4.50.1,
;;; even when all code is commented out. Deleting the elpa folder for now.
;;;----------------------------------------------------------------------------
;;(powerline-default-theme)
;;(powerline-center-theme)


;;;----------------------------------------------------------------------------
;;; Avy
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/avy" load-path)
(autoload #'avy-goto-line "avy" nil t)
(autoload #'avy-isearch "avy" nil t)
(autoload #'avy-goto-word-1 "avy" nil t)
(autoload #'avy-goto-char-timer "avy" nil t)
;; TODO: Add more autoloads. Only making autoloads for what i'm currently using
;;       at the moment.

(global-set-key (kbd "M-g g") #'avy-goto-line)
(global-set-key (kbd "M-g M-g") #'avy-goto-line)
;; TODO: fix issue (maybe upstream too?) where `avy-isearch' doesn't
;; work with evil "/" command. But it does work with evil's "?".
(define-key isearch-mode-map (kbd "C-SPC") #'avy-isearch)
(define-key isearch-mode-map (kbd "C-'") #'avy-isearch) ; swiper convention
;; (define-key evil-normal-state-map (kbd "s") ; like vim sneak.
;;   #'avy-goto-char-2)
;; (define-key evil-motion-state-map (kbd "s") #'avy-goto-char-2)
(when my-use-evil-p
  (define-key evil-normal-state-map (kbd "SPC") #'avy-goto-word-1)
  (define-key evil-motion-state-map (kbd "SPC") #'avy-goto-word-1)
  (evil-leader/set-key "s" #'avy-goto-char-timer))

(with-eval-after-load 'avy
  ;; make keys like ace-jump. Lots of letters means more likely to need only 1
  ;; overlay char.
  (setq avy-keys (nconc (cl-loop for i from ?a to ?z collect i)
                        (cl-loop for i from ?A to ?Z collect i)))
  (setq avy-style 'at-full) ;; options (pre at at-full post)
  (setq avy-background nil) ;; eye is already focused on the jump point so no
                            ;; need to gray-out the background.
  (setq avy-all-windows t)       ;; allow jumps between windows.
  (setq avy-case-fold-search t)  ;; case insensitive
  (setq avy-timeout-seconds 0.5) ;; delay for `avy-goto-char-timer'

  ;; (defun my-avy-goto-line ()
  ;;   (interactive)
  ;;   ;; use the default keys for line jumps
  ;;   (let ((avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
  ;;     (avy-goto-line)))
  ;; (global-set-key (kbd "M-g g") #'my-avy-goto-line)
  ;; (global-set-key (kbd "M-g M-g") #'my-avy-goto-line)

  ;; commenting out. was avy-goto-char-3 deleted?
  ;; (defun my-avy-goto-char-3 (char1 char2 char3 &optional arg)
  ;;   "Copied `avy-goto-char-2' but reading 3 chars. Feels like too many."
  ;;   (interactive (list (read-char "char 1: ")
  ;;                      (read-char "char 2: ")
  ;;                      (read-char "char 3: ")
  ;;                      current-prefix-arg))
  ;;   (avy--with-avy-keys avy-goto-char-3
  ;;                       (avy--generic-jump
  ;;                        (regexp-quote (string char1 char2 char3))
  ;;                        arg
  ;;                        avy-style)))
  )


;;;----------------------------------------------------------------------------
;;; ace-link
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/ace-link" load-path)
(autoload #'ace-link "ace-link" nil t)
(autoload #'ace-link-info "ace-link" nil t)
(autoload #'ace-link-help "ace-link" nil t)
(autoload #'ace-link-man "ace-link" nil t)
(autoload #'ace-link-woman "ace-link" nil t)
(autoload #'ace-link-eww "ace-link" nil t)
(autoload #'ace-link-w3m "ace-link" nil t)
(autoload #'ace-link-compilation "ace-link" nil t)
(autoload #'ace-link-gnus "ace-link" nil t)
(autoload #'ace-link-mu4e "ace-link" nil t)
(autoload #'ace-link-org "ace-link" nil t)
(autoload #'ace-link-org-agenda "ace-link" nil t)
(autoload #'ace-link-xref "ace-link" nil t)
(autoload #'ace-link-custom "ace-link" nil t)
(autoload #'ace-link-addr "ace-link" nil t)
(autoload #'xace-link-sldbxx "ace-link" nil t)
(autoload #'ace-link-slime-xref "ace-link" nil t)
(autoload #'ace-link-slime-inspector "ace-link" nil t)
(autoload #'ace-link-indium-inspector "ace-link" nil t)
(autoload #'ace-link-indium-debugger-frames "ace-link" nil t)
(autoload #'ace-link-cider-inspector "ace-link" nil t)
(autoload #'ace-link-setup-default "ace-link" nil t)

;; NOTE: this code is (mostly) copy/pasted from `ace-link-setup-default'
;;       because calling that autoloaded function caused a premature
;;       load of the ace-link feature!
;;       Discovered by `profile-emacs.el'
(with-eval-after-load 'info
  (define-key Info-mode-map (kbd "o") #'ace-link-info))
(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "o") #'ace-link-compilation))
(with-eval-after-load 'help-mode
  (define-key help-mode-map (kbd "o") #'ace-link-help))
(with-eval-after-load 'woman
  ;; TODO: test this binding
  (define-key woman-mode-map (kbd "o") #'ace-link-woman))

(unless my-use-evil-p
  (with-eval-after-load 'eww
    (define-key eww-link-keymap (kbd "o") #'ace-link-eww)
    (define-key eww-mode-map (kbd "o") #'ace-link-eww))
  (with-eval-after-load 'cus-edit
    (define-key custom-mode-map (kbd "o") #'ace-link-custom)))

(when my-use-evil-p
  (with-eval-after-load 'eww
    (evil-define-key 'normal eww-link-keymap (kbd "o") #'ace-link-eww)
    (evil-define-key 'normal eww-mode-map (kbd "o") #'ace-link-eww))
  (with-eval-after-load 'cus-edit
    ;; TODO: test this binding
    (evil-define-key 'normal custom-mode-map (kbd "o") #'ace-link-custom)))

;;;--------------------
;;; Ace jump mode
;;;--------------------
;; ;; (add-to-list 'load-path "/full/path/where/ace-jump-mode.el/in/")

;; ;; eye is already focused on the jump point so no need to gray background.
;; (setq ace-jump-mode-gray-background nil)
;; (autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
;; (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

;;;----------------------------------------------------------------------------
;;; ace-window
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/ace-window" load-path)
(autoload #'ace-select-window "ace-window" nil t)
(autoload #'ace-delete-window "ace-window" nil t)
(autoload #'ace-swap-window "ace-window" nil t)
(autoload #'ace-delete-other-windows "ace-window" nil t)
(autoload #'ace-display-buffer "ace-window" nil t)
(autoload #'ace-window "ace-window" nil t)
(autoload #'ace-window-display-mode "ace-window" nil t)

(unless (eq my-ui-type 'emacs)
  ;; This is the emacs "copy" keybind. Don't clobber it.
  (global-set-key (kbd "M-w") #'ace-window))

(with-eval-after-load 'ace-window
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ;; home row
  (setq aw-background nil) ;; don't dim the background
  )

;;;----------------------------------------------------------------------------
;;; ace-jump-zap
;;;----------------------------------------------------------------------------
;; (global-set-key (kbd "M-z") #'ace-jump-zap-to-char)

;;;----------------------------------------------------------------------------
;;; clang-format
;;;----------------------------------------------------------------------------
;; rarely use `clang-format', so commenting it out for now.
;; (when (eq my-curr-computer 'work-laptop)
;;   (load "C:/Users/mtz/programs/LLVM/share/clang/clang-format.el")
;;   ;;(global-set-key [C-M-tab] 'clang-format-region)
;;   (global-set-key (kbd "C-c f") 'clang-format-region)
;;   (global-set-key (kbd "C-c b") 'clang-format-buffer))

;;;----------------------------------------------------------------------------
;;; irony
;;;----------------------------------------------------------------------------
(when my-has-clang-p
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's asynchronous function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (when (eq my-curr-computer 'work-laptop)
    ;;directory to libclang.dll
    (push "C:/Users/mtz/programs/LLVM/bin" exec-path))

  ;; (irony-cdb-autosetup-compile-options) ;should be in the hook
  )



;; Only needed on Windows
(when (and (eq system-type 'windows-nt)
           (boundp 'w32-pipe-read-delay))
  (setq w32-pipe-read-delay 0))

;;See also:
;; - https://github.com/Sarcasm/company-irony
;; - https://github.com/Sarcasm/ac-irony

;;;----------------------------------------------------------------------------
;;; company-irony
;;;----------------------------------------------------------------------------
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))

;; ;; (optional) adds CC special commands to `company-begin-commands' in order
;; ;; to trigger completion at interesting places, such as after scope operator
;; ;;     std::|
;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;;----------------------------------------------------------------------------
;;; Load projects
;;;----------------------------------------------------------------------------
;; TODO: move `my-proj-pcl-fancy-modes' to another file and autoload it.
;; TODO: limit fn visibility to buffer were pcl is being viewed.
(defun my-proj-pcl-fancy-modes ()
  "Call this while in the eww buffer."
  (interactive)
  (let ((curr-pos (point))) ; remember position in eww buffer.
    (eww-reload) ; causes images to load that may have failed previously.
    ;; dynamically override default mor settings
    (let ((mor-readonly-for-extra-protection-p nil) ; eww is already readonly.
          (mor-format-automatically-p nil) ; non-lisp text is present.
          (mor-switch-buff-fn #'switch-to-buffer) ; use same window.
          (mor-mode-fn #'common-lisp-mode))
      (mor-mode-on-region (point-min) (point-max)))
    (whitespace-cleanup)
    ;; try to warp to generally the same area you were in the eww buffer.
    (goto-char curr-pos)))

(when (eq my-curr-computer 'mac-mini-m1-2021)
  (let ((lisp-file "my-proj-mac-mini-m1-2021"))
    (autoload #'my-proj-pcl lisp-file nil t)))

(when (eq my-curr-computer 'wild-dog)
  (let ((lisp-file "my-proj-wild-dog"))
    (autoload #'my-proj-dive-python lisp-file nil t)
    (autoload #'my-proj-pcl lisp-file nil t)
    (autoload #'my-proj-progit2 lisp-file nil t)
    (autoload #'my-proj-progit2-dired lisp-file nil t)
    (autoload #'my-proj-ydnjs lisp-file nil t)
    (autoload #'my-proj-paip lisp-file nil t)))

(when (memq my-curr-computer '(work-laptop-2019 work-laptop))
  (let ((lisp-file "my-proj-work-laptop" ))
    (autoload #'my-proj-safetyweb lisp-file nil t)
    (autoload #'my-proj-safetyweb-ects lisp-file nil t)
    (autoload #'my-proj-rsims lisp-file nil t)
    (autoload #'my-proj-daily-diff lisp-file nil t)
    (autoload #'my-proj-db-safety lisp-file nil t)
    (autoload #'my-proj-trighist lisp-file nil t)
    (autoload #'my-proj-emacs lisp-file nil t)
    (autoload #'my-proj-cl lisp-file nil t)
    (autoload #'my-proj-imgtag lisp-file nil t)
    (autoload #'my-proj-cpp lisp-file nil t)
    (autoload #'my-proj-pcl lisp-file nil t)
    (autoload #'my-proj-tcpl lisp-file nil t)
    (autoload #'my-proj-dive-python lisp-file nil t)
    (autoload #'my-proj-progit2 lisp-file nil t)
    (autoload #'my-proj-paip lisp-file nil t)
    (autoload #'my-proj-emacs-manual lisp-file nil t)
    (autoload #'my-proj-sicp lisp-file nil t)
    (autoload #'my-proj-ticpp lisp-file nil t)
    (defun my-bookmarks-web ()
      "My browser bookmarks from IE saved to a text file."
      (interactive)
      (find-file "c:/users/mtz/bookmarks.txt")))

  ;;quick load of c:\users\mtz
  (when my-use-evil-p
    (defun my-open-user-folder ()
      (interactive)
      (dired "C:/Users/mtz"))
    (evil-leader/set-key "1" #'my-open-user-folder)

    ;;quick load of c:\users\mtz\proj\ecp\dev\db
    ;; (evil-leader/set-key "2" (lambda ()
    ;;                            (interactive)
    ;;                            (dired "c:/users/mtz/proj/ecp/dev/db")))

    ;;quick load of TFS \Main\SqlScripts
    ;; (evil-leader/set-key "3"
    ;;   (lambda ()
    ;;     (interactive)
    ;;     (dired "C:/Users/mtz/proj/TFS/SafetyWebsite/OSHE/Main/DbScripts")))

    ;; quick load of SafeteWebysite TFS folder \Development
    (defun my-open-dev-folder ()
      (interactive)
      ;; (dired "C:/Users/mtz/proj/TFS/SafetyWebsite/OSHE/Development")
      (dired "c:/Users/mtz/proj/safety/SafetyWebsite"))
    (evil-leader/set-key "4" #'my-open-dev-folder)))


(when (or (eq system-type 'gnu/linux)
          (eq system-type 'darwin))
  (when my-use-evil-p
    (defun my-open-user-folder ()
      (interactive)
      (dired "~"))
    (evil-leader/set-key "1" #'my-open-user-folder)))


;;; quick open of the .emacs (or init.el) file.
(defun my-open-init ()
  "Open my Emacs init file."
  (interactive)
  (find-file-existing "~/.emacs.d/init.el"))

(when my-use-evil-p
  (evil-leader/set-key "`" #'my-open-init)
  ;; the above key is hard to type on a 60% poker so making an alternative.
  (evil-leader/set-key "8" #'my-open-init))


;;;----------------------------------------------------------------------------
;;; VC version control
;;;----------------------------------------------------------------------------
;; (defadvice vc-dir (before ensure-excluded-dirs)
;;   "add to the excluded dir list. It's not working if I add in init.el"
;;   (add-to-list 'vc-directory-exclusion-list "bin")
;;   (add-to-list 'vc-directory-exclusion-list "obj"))
;; (ad-activate 'vc-dir)

;; speed up opening files. see https://www.reddit.com/r/emacs/comments/4c0mi3/t
;; he_biggest_performance_improvement_to_emacs_ive/
;; TODO: revisit this later. The performance problems may be fixed soon.
;;       see: https://lists.gnu.org/archive/html/emacs-devel/2016-02/msg00440.h
;;       tml
(let ((hook (if (boundp 'find-file-hook)
                'find-file-hook      ; use new hook var if available
              'find-file-hooks))     ; else older emacs-version < 22.1
      (fn (if (fboundp 'vc-refresh-state)
              'vc-refresh-state      ; use new hook fn if available.
            'vc-find-file-hook)))    ; else older emacs-version < 25.1
  ;; remove a vc source control hook that greatly slows down opening files.
  (remove-hook hook fn))

(with-eval-after-load 'vc
  (push "bin" vc-directory-exclusion-list)
  (push "obj" vc-directory-exclusion-list)
  (when my-use-evil-p
    ;; use emacs bindings (not evil).
    (push '("\\*vc" . emacs) evil-buffer-regexps)))

;; (add-hook 'vc-- (lambda () (linum-mode 0)))


;;;----------------------------------------------------------------------------
;;; log-edit-mode. used by vc for the commit msg buffer.
;;;----------------------------------------------------------------------------
(with-eval-after-load 'log-edit
  ;; set up auto-fill mode
  (defun my-setup-log-edit-mode ()
    (setq fill-column 72) ; max line length. buffer-local so set in hook.
    (turn-on-auto-fill)
    ;; prevent indentation 1
    (when (fboundp #'electric-indent-local-mode)
      (electric-indent-local-mode -1))
    ;; prevent indentation 2
    (paragraph-indent-minor-mode 1))
  (add-hook 'log-edit-hook #'my-setup-log-edit-mode))

;;;----------------------------------------------------------------------------
;;; Projectile
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/projectile" load-path)
(autoload 'projectile-version "projectile" nil t nil)
(autoload 'projectile-invalidate-cache "projectile" nil t nil)
(autoload 'projectile-purge-file-from-cache "projectile" nil t nil)
(autoload 'projectile-purge-dir-from-cache "projectile" nil t nil)
(autoload 'projectile-cache-current-file "projectile" nil t nil)
(autoload 'projectile-discover-projects-in-directory "projectile" nil t nil)
(autoload 'projectile-discover-projects-in-search-path "projectile" nil t nil)
(autoload 'projectile-switch-to-buffer "projectile" nil t nil)
(autoload 'projectile-switch-to-buffer-other-window "projectile" nil t nil)
(autoload 'projectile-switch-to-buffer-other-frame "projectile" nil t nil)
(autoload 'projectile-display-buffer "projectile" nil t nil)
(autoload 'projectile-project-buffers-other-buffer "projectile" nil t nil)
(autoload 'projectile-multi-occur "projectile" nil t nil)
(autoload 'projectile-find-other-file "projectile" nil t nil)
(autoload 'projectile-find-other-file-other-window "projectile" nil t nil)
(autoload 'projectile-find-other-file-other-frame "projectile" nil t nil)
(autoload 'projectile-find-file-dwim "projectile" nil t nil)
(autoload 'projectile-find-file-dwim-other-window "projectile" nil t nil)
(autoload 'projectile-find-file-dwim-other-frame "projectile" nil t nil)
(autoload 'projectile-find-file "projectile" nil t nil)
(autoload 'projectile-find-file-other-window "projectile" nil t nil)
(autoload 'projectile-find-file-other-frame "projectile" nil t nil)
(autoload 'projectile-toggle-project-read-only "projectile" nil t nil)
(autoload 'projectile-find-dir "projectile" nil t nil)
(autoload 'projectile-find-dir-other-window "projectile" nil t nil)
(autoload 'projectile-find-dir-other-frame "projectile" nil t nil)
(autoload 'projectile-find-test-file "projectile" nil t nil)
(autoload 'projectile-find-related-file-other-window "projectile" nil t nil)
(autoload 'projectile-find-related-file-other-frame "projectile" nil t nil)
(autoload 'projectile-find-related-file "projectile" nil t nil)
(autoload 'projectile-related-files-fn-groups "projectile" nil nil nil)
(autoload 'projectile-related-files-fn-extensions "projectile" nil nil nil)
(autoload 'projectile-related-files-fn-test-with-prefix "projectile" nil nil
  nil)
(autoload 'projectile-related-files-fn-test-with-suffix "projectile" nil nil nil)
(autoload 'projectile-project-info "projectile" nil t nil)
(autoload 'projectile-find-implementation-or-test-other-window "projectile" nil
  t nil)
(autoload 'projectile-find-implementation-or-test-other-frame "projectile" nil
  t nil)
(autoload 'projectile-toggle-between-implementation-and-test "projectile" nil t
  nil)
(autoload 'projectile-grep "projectile" nil t nil)
(autoload 'projectile-ag "projectile" nil t nil)
(autoload 'projectile-ripgrep "projectile" nil t nil)
(autoload 'projectile-regenerate-tags "projectile" nil t nil)
(autoload 'projectile-find-tag "projectile" nil t nil)
(autoload 'projectile-run-command-in-root "projectile" nil t nil)
(autoload 'projectile-run-shell-command-in-root "projectile" nil t nil)
(autoload 'projectile-run-async-shell-command-in-root "projectile" nil t nil)
(autoload 'projectile-run-gdb "projectile" nil t nil)
(autoload 'projectile-run-shell "projectile" nil t nil)
(autoload 'projectile-run-eshell "projectile" nil t nil)
(autoload 'projectile-run-ielm "projectile" nil t nil)
(autoload 'projectile-run-term "projectile" nil t nil)
(autoload 'projectile-run-vterm "projectile" nil t nil)
(autoload 'projectile-replace "projectile" nil t nil)
(autoload 'projectile-replace-regexp "projectile" nil t nil)
(autoload 'projectile-kill-buffers "projectile" nil t nil)
(autoload 'projectile-save-project-buffers "projectile" nil t nil)
(autoload 'projectile-dired "projectile" nil t nil)
(autoload 'projectile-dired-other-window "projectile" nil t nil)
(autoload 'projectile-dired-other-frame "projectile" nil t nil)
(autoload 'projectile-vc "projectile" nil t nil)
(autoload 'projectile-recentf "projectile" nil t nil)
(autoload 'projectile-configure-project "projectile" nil t nil)
(autoload 'projectile-compile-project "projectile" nil t nil)
(autoload 'projectile-test-project "projectile" nil t nil)
(autoload 'projectile-run-project "projectile" nil t nil)
(autoload 'projectile-repeat-last-command "projectile" nil t nil)
(autoload 'projectile-switch-project "projectile" nil t nil)
(autoload 'projectile-switch-open-project "projectile" nil t nil)
(autoload 'projectile-find-file-in-directory "projectile" nil t nil)
(autoload 'projectile-find-file-in-known-projects "projectile" nil t nil)
(autoload 'projectile-cleanup-known-projects "projectile" nil t nil)
(autoload 'projectile-clear-known-projects "projectile" nil t nil)
(autoload 'projectile-remove-known-project "projectile" nil t nil)
(autoload 'projectile-remove-current-project-from-known-projects "projectile"
  nil t nil)
(autoload 'projectile-add-known-project "projectile" nil t nil)
(autoload 'projectile-ibuffer "projectile" nil t nil)
(autoload 'projectile-commander "projectile" nil t nil)
(autoload 'projectile-browse-dirty-projects "projectile" nil t nil)
(autoload 'projectile-edit-dir-locals "projectile" nil t nil)
(defvar projectile-mode nil)
;; (custom-autoload 'projectile-mode "projectile" nil)
(autoload 'projectile-mode "projectile" nil t nil)

;; (projectile-global-mode)
;; (setq projectile-indexing-method 'native)
;; (setq projectile-enable-caching t)
;; (define-key projectile-mode-map (kbd "C-x C-b") 'projectile-ibuffer)

;;;----------------------------------------------------------------------------
;;; icicles
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/icicles" load-path)
(when (eq my-narrow-type 'icicles)
  (require 'icicles)
  (icicle-mode 1))

;;;----------------------------------------------------------------------------
;;; web-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/web-mode" load-path)
(autoload #'web-mode "web-mode" nil t)
(push '("\\.phtml\\'" . web-mode) auto-mode-alist)
(push '("\\.tpl\\.php\\'" . web-mode) auto-mode-alist)
(push '("\\.[gj]sp\\'" . web-mode) auto-mode-alist)
(push '("\\.as[cp]x\\'" . web-mode) auto-mode-alist)
(push '("\\.erb\\'" . web-mode) auto-mode-alist)
(push '("\\.mustache\\'" . web-mode) auto-mode-alist)
(push '("\\.djhtml\\'" . web-mode) auto-mode-alist)
(push '("\\.html?\\'" . web-mode) auto-mode-alist)
(push '("\\.cshtml\\'" . web-mode) auto-mode-alist)

(with-eval-after-load 'web-mode
  ;; Auto-close on > and </
  (setq web-mode-auto-close-style 2)

  ;; highlights for start/closing tags and vertical line between them.
  (setq web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight  nil)

  ;; indent widths of different code sections
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset    4
        web-mode-code-indent-offset   4
        web-mode-sql-indent-offset    4)

  (defun my-setup-web-mode ()
    ;; useful for embedded javascript or css
    (rainbow-delimiters-mode 1)
    (my-turn-on-electric-pair-local-mode))
  (add-hook 'web-mode-hook #'my-setup-web-mode))


;;;----------------------------------------------------------------------------
;;; vimrc-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/vimrc-mode" load-path)
(autoload #'vimrc-mode "vimrc-mode" nil t)
(push '(".vim\\(rc\\)?$" . vimrc-mode) auto-mode-alist)

;;;----------------------------------------------------------------------------
;;; Make dired appear in a side window
;;;----------------------------------------------------------------------------
(autoload #'my-current-file-path "my-folder-nav" nil t)
(autoload #'my-current-folder-path "my-folder-nav" nil t)
(autoload #'my-folder-nav "my-folder-nav" nil t)
;; (global-set-key (kbd "<f8>") #'my-folder-nav)


;;;----------------------------------------------------------------------------
;;; skewer-mode
;;;----------------------------------------------------------------------------
;; ;;(skewer-setup)
;; (add-hook 'js2-mode-hook 'skewer-mode)
;; (add-hook 'css-mode-hook 'skewer-css-mode)
;; (add-hook 'html-mode-hook 'skewer-html-mode)
;; (add-hook 'web-mode-hook 'skewer-html-mode)

(with-eval-after-load 'skewer-mode
;;   (defun my-skewer-repl-clear-buffer ()
;;     "Deletes the contents of the skewer-repl buffer.
;; Depends on evil mode."
;;     (interactive)
;;     (evil-goto-line) ;bottom
;;     (evil-previous-visual-line) ;up 1
;;     (evil-end-of-line)
;;     (delete-region 1 (+ (point) 2))
;;     (evil-end-of-line))
;;   (define-key skewer-repl-mode-map (kbd "C-c M-o")
;;     #'my-skewer-repl-clear-buffer)

  (defun my-setup-skewer-mode ()
    ;;turn off line numbers in the repl
    ;; (linum-mode 0) ;; linum-mode obsolete and not loaded by default on
    ;;                ;; emacs 29+

    ;;there's always a trailing space at repl prompt. Don't highlight it.
    (setq show-trailing-whitespace nil))
  (add-hook 'skewer-repl-mode-hook #'my-setup-skewer-mode)

  ;; (require 'simple-httpd)
  ;; (defun my-skewer-html ()
  ;;   "Wire up the html file you're editing with skewer."
  ;;   (interactive)
  ;;   ;; (skewer-html-mode) ; this is set in a hook, don't need it here.
  ;;   ;; (setq httpd-root "c:/users/mtz/scratch/testwebsite")
  ;;   (setq httpd-root (my-current-folder-path))
  ;;   (httpd-start)
  ;;   (browse-url-of-file (concat "http://localhost:8080/"
  ;;                               (file-name-nondirectory buffer-file-name)))
  ;;   (run-skewer)
  ;;   (message
  ;;    (concat "put this in the <head>: <script src=\"http://localhost:"
  ;;            "8080/skewer\"></script> --- switch to tab http://localhost:"
  ;;            "8080/FileOrRouteName, then start evaling html")))
  )

;;;----------------------------------------------------------------------------
;;; eshell
;;;----------------------------------------------------------------------------
(with-eval-after-load 'esh-mode

  (when (fboundp #'eshell/clear-scrollback) ;; emacs 25+
    (defun my-eshell-clear ()
      (interactive)
      (eshell/clear t))
    (add-hook 'eshell-mode-hook ; `eshell-mode-map' not recognized unless set
                                ; in the hook. Eval-after-load doesn't work.
                                ; Must be buffer local.
              (lambda ()
                ;; emulate some SLIME keybind
                (define-key eshell-mode-map
                  (kbd "C-c M-o")
                  #'my-eshell-clear)))))

;; (with-eval-after-load "eshell-mode"
;;   (defun my-eshell-clear-buffer ()
;;     "Deletes the contents of eshell buffer, except the last prompt"
;;     (interactive)
;;     (save-excursion
;;       (goto-char eshell-last-output-end)
;;       (let ((lines (count-lines 1 (point)))
;;             (inhibit-read-only t))
;;         (beginning-of-line)
;;         (let ((pos (point)))
;;           (if (bobp)
;;               (if (interactive-p)
;;                   (error "Buffer too short to truncate"))
;;             (delete-region (point-min) (point))
;;             (if (interactive-p)
;;                 (message "Buffer cleared")))))))

;;   (defun my-eshell-clear-line ()
;;     (interactive)
;;     ;;(message "") ;delete multiple lines of junk in the mini buffer.
;;     (eshell-bol)
;;     (evil-delete-line)
;;     ;;(message "") ;delete multiple lines of junk in the mini buffer.
;;     )

;;   ;;set up custom key bindings when the mode loads.
;;   (add-hook 'eshell-mode-hook ; `eshell-mode-map' not recognized unless set
;;                               ; in the hook. Eval-after-load doesn't work.
;;             (lambda ()
;;               ;;Use the same keybinding to clear eshell as the SLIME repl
;;               (define-key eshell-mode-map (kbd "C-c M-o")
;;                 'my-eshell-clear-buffer)
;;               ;;make evil's dd compatible with the read-only prompt of the
;;               ;; current line.
;;               ;; (define-key evil-normal-state-map
;;               ;;   (kbd "<remap> <evil-delete-whole-line>")
;;               ;;   'my-eshell-clear-line)
;;               ;; (evil-define-key 'normal eshell-mode-map (kbd "d d")
;;               ;;   'my-eshell-clear-line)
;;               )))

;;;----------------------------------------------------------------------------
;;; highlight-tail
;;;----------------------------------------------------------------------------
(autoload #'highlight-tail-mode "highlight-tail" nil t)

(with-eval-after-load 'highlight-tail

  (defun my-tail-colors-for-bg ()
    (if (eq 'light (frame-parameter nil 'background-mode))
        '(("green yellow" . 0)
          ("lemon chiffon". 40))
      '(("dark cyan" . 0)
        ("black" . 40))))

  ;; no hooks found, so this will be called manually when the theme changes.
  (defun my-highlight-tail-reload ()
    "Reload highlight tail with the colors based on the current background."
    (interactive)
    (setq highlight-tail-colors (my-tail-colors-for-bg))
    (highlight-tail-reload))

  (setq highlight-tail-colors (my-tail-colors-for-bg)
        highlight-tail-steps 40           ;; 80
        highlight-tail-timer 0.04         ;; 0.04
        highlight-tail-posterior-type t)) ;;'const

;; (highlight-tail-mode)
;; (highlight-tail-reload)

;;;----------------------------------------------------------------------------
;;; eww web-browser
;;;----------------------------------------------------------------------------
;; (setq browse-url-browser-function ; default for opening links.
;;       'eww-browse-url)


;; avoid proportional fonts. Could be used elsewhere besides eww, so set
;; at top level.
(when (>= emacs-major-version 25)
  (setq shr-use-fonts nil))

(with-eval-after-load 'eww
  (when (eq my-curr-computer 'work-laptop)
    (setq eww-download-directory "C:/Users/mtz/Downloads"))

  (define-key eww-mode-map (kbd "C-c h") #'eww-back-url)
  (define-key eww-mode-map (kbd "C-c l") #'eww-forward-url)
  (define-key eww-mode-map (kbd "C-c r") #'eww-reload)
  (define-key eww-mode-map (kbd "C-c g") #'eww)
  (define-key eww-mode-map (kbd "C-c o") #'ace-link-eww)
  (define-key eww-mode-map (kbd "<tab>") #'shr-next-link)
  ;; conkeror bindings. TODO: doesn't seem to work when in evil mode, fix it.
  (define-key eww-mode-map (kbd "B") #'eww-back-url)
  (define-key eww-mode-map (kbd "F") #'eww-forward-url)

  (when my-use-evil-p
    (evil-define-key 'normal eww-link-keymap (kbd "C-o") #'eww-back-url)
    (evil-define-key 'normal eww-mode-map (kbd "C-o") #'eww-back-url))

  (progn ;; clearing the ^M
    (defun my-clear-weird-m ()
      "Clear the weird ^M character that shows in some eww buffers."
      (interactive
       ;; (let ((args (query-replace-read-args "Replace" t)))
       ;;   (setcdr (cdr args) nil) ; remove third value returned from
       ;;                           ; query---args
       ;;   args)
       )
      ;; TODO: clear in the header line too.
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "" nil t)
          (replace-match ""))))

    (defadvice eww-render (after ad-clear-weird-m)
      "Clear the weird ^M character that shows in some eww buffers."
      (my-clear-weird-m))
    (ad-activate 'eww-render))

  (defun my-setup-eww ()
    (setq show-trailing-whitespace nil))
  (add-hook 'eww-mode-hook #'my-setup-eww))

;;;----------------------------------------------------------------------------
;;; w3
;;;----------------------------------------------------------------------------
(with-eval-after-load 'w3
  (defun my-w3-goto (url)
    (let ((w3--args nil) ; prevents errors when loading CL hyperspec pages on
                         ; local disk
          (w3-default-homepage url))
      (w3))))

;;;----------------------------------------------------------------------------
;;; cedet
;;;----------------------------------------------------------------------------
;; (progn
;;   (global-ede-mode 1)
;;   (semantic-load-enable-code-helpers))

;; (progn ;from tuhdo's github website demo.
;;   (require 'cc-mode)
;;   (require 'semantic)

;;   (global-semanticdb-minor-mode 1)
;;   (global-semantic-idle-scheduler-mode 1)
;;   (global-semantic-stickyfunc-mode 1)

;;   (semantic-mode 1)

;;   (defun alexott/cedet-hook ()
;;     (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
;;     (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

;;   (add-hook 'c-mode-common-hook 'alexott/cedet-hook)
;;   (add-hook 'c-mode-hook 'alexott/cedet-hook)
;;   (add-hook 'c++-mode-hook 'alexott/cedet-hook)

;;   ;; Enable EDE only in C/C++
;;   (require 'ede)
;;   (global-ede-mode)

;;   (provide 'setup-cedet))


;;;----------------------------------------------------------------------------
;;; aggressive-indent. Turning off for now since lispy makes it easy to keep
;;; things indented and aggressive-indent causes a noticeable lag when barfing/
;;; slurping in larger, deeply nested expressions.
;;;----------------------------------------------------------------------------
;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'css-mode-hook #'aggressive-indent-mode)
;; (add-hook 'js2-mode-hook #'aggressive-indent-mode)

;;(add-hook 'slime-repl-mode-hook #'aggressive-indent-mode)
;;(global-aggressive-indent-mode 1)
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
;; (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)

;;;----------------------------------------------------------------------------
;;; with-editor. dependency of magit
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/with-editor/lisp" load-path)
(autoload #'with-editor-export-editor "with-editor" nil t)
(autoload #'with-editor-export-git-editor "with-editor" nil t)
(autoload #'with-editor-export-hg-editor "with-editor" nil t)
(autoload #'shell-command-with-editor-mode "with-editor" nil t)
(autoload #'with-editor-async-shell-command "with-editor" nil t)
(autoload #'with-editor-shell-command "with-editor" nil t)
(autoload #'with-editor-export-editor "with-editor" nil t)
(autoload #'with-editor-export-git-editor "with-editor" nil t)
(autoload #'with-editor-export-hg-editor "with-editor" nil t)
(autoload #'shell-command-with-editor-mode "with-editor" nil t)
(autoload #'with-editor-async-shell-command "with-editor" nil t)
(autoload #'with-editor-shell-command "with-editor" nil t)

;;;----------------------------------------------------------------------------
;;; transient. dependency of magit
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/transient/lisp" load-path)
(autoload #'transient-define-prefix "transient" nil nil 'macro)
(autoload #'transient-insert-suffix "transient" nil nil nil)
(autoload #'transient-append-suffix "transient" nil nil nil)
(autoload #'transient-replace-suffix "transient" nil nil nil)
(autoload #'transient-remove-suffix "transient" nil nil nil)
(autoload #'transient-remove-suffix "transient" nil nil nil)

;;;----------------------------------------------------------------------------
;;; libgit. dependency of magit
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/libegit2" load-path)

;;;----------------------------------------------------------------------------
;;; magit
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/magit/lisp" load-path)
(autoload #'magit-status "magit-status" nil t)
(autoload #'magit-init "magit-status" nil t)
(autoload 'magit-show-refs "magit-refs" nil t)

;; NOTE: autoloads for git-commit. It's a separate melpa package, but in the
;; same git repo as magit itself.
(custom-autoload 'global-git-commit-mode "git-commit" nil)
(autoload 'global-git-commit-mode "git-commit" nil t nil)
(autoload 'git-commit-setup-check-buffer "git-commit" nil t nil)
(autoload 'git-commit-setup "git-commit" nil nil nil)
;; TODO: add more autoloads




;; prevent warning message.
;; Doesn't work when set in eval-after-load ???
;; (setq magit-last-seen-setup-instructions "1.4.0")

(when my-use-evil-p
  (evil-leader/set-key "m" #'magit-status)) ; autoloaded

(with-eval-after-load 'magit
  ;; Magit stole my M-h binding, take it back.
  ;; TODO: rebind magit-show-only-files, which was on M-h
  (when my-use-evil-p
    (define-key magit-mode-map (kbd "M-h") #'evil-window-left)

    ;; reclaim keybinds stolen by magit-log
    (define-key magit-mode-map (kbd "M-w") #'ace-window)
    ;; re-bind the keys we clobbered
    (define-key magit-mode-map (kbd "C-c w") #'magit-copy-buffer-revision)

    ;; use emacs bindings (not evil). the new v2.1.0 magit uses evil for some
    ;; buffers.
    (push '("\\*magit" . emacs) evil-buffer-regexps))

  (when my-use-ivy-p
    (setq magit-completing-read-function #'ivy-completing-read))

  ;; Special highlight on the changed words in a line. Makes it easier to see
  ;; what changed. If 'all becomes a performance problem then set it to t so
  ;; it only affects the currently highlighted diff.
  (setq magit-diff-refine-hunk 'all)

  ;; NOTE: var `magit-log-arguments' was removed. Only set it if using an
  ;; older magit version. TODO: look into replacement for the newer magit.
  (when (boundp 'magit-log-arguments)
    ;; use colored graph lines. Could be a performance issue.
    (push "--color" magit-log-arguments))

  ;; don't show recent commits when calling `magit-status'.
  ;; TODO: modify magit code so recent commits are not even queried if the
  ;;       count is 0.
  (setq magit-log-section-commit-count 0)

  ;; function to remove hooks magit adds to `find-file-hook'.
  ;; It slows down opening new files on ms-windows by a few seconds.
  (defun my-remove-slow-magit-hooks ()
    "TODO: find the root cause of the slowness. Removing hooks on
`find-file-hook' does not fix the performance issue.
TODO: call this function when it works."
    (interactive)
    (let ((file-hook (if (version< emacs-version "22.1")
                         'find-file-hooks
                       'find-file-hook)))
      (remove-hook file-hook 'magit-edit-branch*description-check-buffers)
      (remove-hook file-hook 'global-magit-file-mode-check-buffers)
      (remove-hook file-hook 'magit-auto-revert-mode-check-buffers)
      (remove-hook file-hook 'auto-revert-find-file-function)
      (remove-hook file-hook 'git-commit-setup-check-buffer)
      (remove-hook file-hook 'which-func-ff-hook))))


;;;----------------------------------------------------------------------------
;;; git. Just git stuff external to emacs.
;;;----------------------------------------------------------------------------
(when (eq my-curr-computer 'work-laptop) ; TODO: move this into separate file.
  (defun my-git-docs ()
    (interactive)
    (eww-open-file (concat "C:/Users/mtz/AppData/Local/Programs/Git/"
                           "mingw64/share/doc/git-doc/git.html"))))


;;;----------------------------------------------------------------------------
;;; ediff
;;;----------------------------------------------------------------------------
(with-eval-after-load 'ediff
  (setq ediff-split-window-function #'split-window-horizontally)
  ;; don't use the popup window
  (setq ediff-window-setup-function
        #'ediff-setup-windows-plain) ; 'ediff-setup-windows-multiframe

  (when (memq my-curr-computer '(work-laptop-2019))
    (setq ediff-diff-program "C:/Program Files/KDiff3/bin/diff.exe")
    (setq ediff-diff3-program "C:/Program Files/KDiff3/bin/diff3.exe")))


;;;----------------------------------------------------------------------------
;;; helm-w32-launcher. Microsoft Windows only?
;;;----------------------------------------------------------------------------
(when (fboundp #'helm-w32-launcher) ;;(eq system-type 'windows-nt)
  (global-set-key (kbd "C-c w") #'helm-w32-launcher))

;;;----------------------------------------------------------------------------
;;; leerzeichen. Displays symbols for tab, space, and newline.
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/leerzeichen.el" load-path)
(autoload #'leerzeichen-mode "leerzeichen" nil t)
;;(leerzeichen-mode)
;; (custom-set-faces
;;  '(leerzeichen ((t (:foreground "black";"#A8A800"
;;                                 :background "white";"#D4D4C8"
;;                                 :italic nil
;;                                 :bold nil
;;                                 ;;:box t
;;                                 )))))


;;;----------------------------------------------------------------------------
;;; darkroom
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/darkroom" load-path)
(autoload 'darkroom-mode "darkroom" nil t)
(autoload 'darkroom-tentative-mode "darkroom" nil t)

(with-eval-after-load 'darkroom
  (setq darkroom-margins 0.15)
  ;; nil keeps margins close to the centered text.
  (setq darkroom-fringes-outside-margins nil)
  ;; 0 to keep text the same size. Usually what i wnat when coding. But if
  ;; presenting my screen to others it would make sense to increase font size.
  (setq darkroom-text-scale-increase 0))

;;;----------------------------------------------------------------------------
;;; vim-empty-lines-mode
;;;----------------------------------------------------------------------------
;; ;; messes up recenter-top-bottom so not using for now.
;; (global-vim-empty-lines-mode)

;;;----------------------------------------------------------------------------
;;; fill-column-indicator, fci-mode
;;; NOTE: I have replaced this package with the native Emacs package
;;;       `display-fill-column-indicator'.
;;;----------------------------------------------------------------------------
;; (defvar-local company-fci-mode-on-p nil) ; put this in top level.
;;                                          ; flycheck warning.
;; (with-eval-after-load 'fill-column-indicator
;;   (setq fci-rule-column 79)
;;   (setq fci-rule-width 1)
;;   ;; use text, not bitmap to avoid increasing line spacing with fixedsys font.
;;   (setq fci-always-use-textual-rule t)
;;   (progn
;;     (setq fci-dash-pattern 0.5)   ;; length of the dash 0 to 1
;;     (setq fci-rule-use-dashes t))
;;   ;; (setq fci-rule-color "#4d4d4d") ;; tailored for zenburn ATM.

;;   (defun my-fci-refresh ()
;;     (interactive)
;;     (fci-mode 0)
;;     (fci-mode 1))

;;   (progn ;work-around issue where `fill-column-indicator' moves company
;;     ;; suggestion box.
;;     ;; TODO: handle for auto-complete too. It's on emacs.stackexchange.
;;     ;; (defvar-local company-fci-mode-on-p nil) ; moved to top level

;;     (defun company-turn-off-fci (&rest _ignore)
;;       (when (boundp 'fci-mode)
;;         (setq company-fci-mode-on-p fci-mode)
;;         (when fci-mode (fci-mode -1))))

;;     (defun company-maybe-turn-on-fci (&rest _ignore)
;;       (when company-fci-mode-on-p (fci-mode 1)))

;;     (add-hook 'company-completion-started-hook #'company-turn-off-fci)
;;     (add-hook 'company-completion-finished-hook #'company-maybe-turn-on-fci)
;;     (add-hook 'company-completion-cancelled-hook #'company-maybe-turn-on-fci)))



;;;----------------------------------------------------------------------------
;;; epl. dependency of pkg-info.
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/epl" load-path)

;;;----------------------------------------------------------------------------
;;; pkg-info. dependency of flycheck.
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/pkg-info" load-path)
(autoload #'pkg-info-library-original-version "pkg-info" nil t)
(autoload #'pkg-info-library-version "pkg-info" nil t)
(autoload #'pkg-info-defining-library-original-version "pkg-info" nil t)
(autoload #'pkg-info-defining-library-version "pkg-info" nil t)
(autoload #'pkg-info-package-version "pkg-info" nil t)
(autoload #'pkg-info-version-info "pkg-info" nil t)

;;;----------------------------------------------------------------------------
;;; flycheck
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/flycheck" load-path)
(autoload #'flycheck-manual "flycheck" nil t)
(autoload #'flycheck-mode "flycheck" nil t)
(autoload #'global-flycheck-mode "flycheck" nil t)
(autoload #'flycheck-define-error-level "flycheck" nil t)
(autoload #'flycheck-define-command-checker "flycheck" nil t)
(autoload #'flycheck-def-config-file-var "flycheck" nil t)
(autoload #'flycheck-def-option-var "flycheck" nil t)
(autoload #'flycheck-define-checker "flycheck" nil t)

(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "M-n") #'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") #'flycheck-previous-error)
  ;;(evil-define-key 'flycheck-mode-map (kbd "M-n") #'flycheck-next-error)
  (setq flycheck-emacs-lisp-load-path 'inherit)

;;;----------------------------------
;;; helm-flycheck
;;;----------------------------------
  ;; (when my-use-helm-p
  ;;   (defun my-helm-flycheck ()
  ;;     (interactive)
  ;;     ;; nil for no candidate limit. To scroll through all the warnings.
  ;;     (let ((helm-candidate-number-limit nil))
  ;;       (call-interactively #'helm-flycheck)))
  ;;   (define-key flycheck-mode-map (kbd "C-c f") #'my-helm-flycheck))
  )

;;;----------------------------------------------------------------------------
;;; flymake
;;;----------------------------------------------------------------------------
(with-eval-after-load 'flymake
  ;; key binds
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

  (when my-use-evil-p
    ;; use emacs key binds (not evil)
    (push '("\\*flymake diagnostics" . emacs) evil-buffer-regexps)))

;;;----------------------------------------------------------------------------
;;; hydra
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/hydra" load-path)
(autoload #'defhydra "hydra" nil t)

(autoload #'my-choose-hydra "my-hydras" nil t)
(autoload #'hydra-easyscroll/body "my-hydras" nil t)
(autoload #'my-hydra-smerge/body "my-hydras" nil t)

(when my-use-evil-p
  (define-key evil-normal-state-map (kbd "z s") #'hydra-easyscroll/body)
  ;; (define-key evil-normal-state-map (kbd "\\") #'my-choose-hydra)
  ;; (define-key evil-motion-state-map (kbd "\\") #'my-choose-hydra)
  )

(with-eval-after-load 'hydra
  (setq hydra-is-helpful t)
  ;; don't use window for hints. It seems to lock things up.
  ;; And window switcher mode really gets messed up.
  (setq hydra-hint-display-type 'message) ;; (setq hydra-lv nil)
  )

;;;----------------------------------------------------------------------------
;;; smerge-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'smerge-mode
  ;; taken from https://github.com/alphapapa/unpackaged.el#hydra
  (defhydra hydra-smerge
      (:color pink :hint nil :post (smerge-auto-leave))
    "
      ^Move^       ^Keep^               ^Diff^                 ^Other^
      ^^-----------^^-------------------^^---------------------^^-------
      _n_ext       _b_ase               _<_: upper/base        _C_ombine
      _p_rev       _u_pper              _=_: upper/lower       _r_esolve
      ^^           _l_ower              _>_: base/lower        _k_ill current
      ^^           _a_ll                _R_efine
      ^^           _RET_: current       _E_diff
        "
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
          "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))

  (define-key smerge-mode-map (kbd "C-c h") #'hydra-smerge/body)

  ;; keybind to turn on my custom hydra for smerge-mode.
  ;; (define-key smerge-mode-map (kbd "C-c h") #'my-hydra-smerge/body)
  )

;;;----------------------------------------------------------------------------
;;; lv. This is a file in hydra that lsp-mode depends on.
;;;----------------------------------------------------------------------------
(defvar lv-force-update)
(autoload #'lv-window "lv" nil t)
(autoload #'lv-message "lv" nil t)
(autoload #'lv-delete-window "lv" nil t)

;;;----------------------------------------------------------------------------
;;; erc
;;;----------------------------------------------------------------------------
(with-eval-after-load 'erc
  (setq erc-default-server "irc.libera.chat") ; formerly "irc.freenode.net"

  (let ((format "%-I:%M%#p")) ;; use 12 hour format. am/pm
    (setq erc-timestamp-format       format
          erc-timestamp-format-right format))

  (setq erc-autojoin-channels-alist
        '(("libera.chat" "#emacs")
          ;; ("freenode.net" "#emacs")
          ))

  (progn
    ;;from finster on irc #emacs. switch erc buffers
    ;; TODO; make it neutral to ido so i can use helm, swiper, default, etc.
    (defvar x/chatbuffer-types '(erc-mode
                                 circe-channel-mode
                                 circe-query-mode))

    ;; TODO: rename function since it's no longer ido specific.
    (defun x/ido-chat-buffer ()
      "Switch to erc/circe buffer, completed by ido."
      (interactive)
      ;; TODO: use ivy, ido, helm specific buffer switch fn.
      (switch-to-buffer
       (completing-read ;; ido-completing-read
        "Channel: "
        (save-excursion
          (delq
           nil
           (mapcar (lambda (buf)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (and (memq major-mode x/chatbuffer-types)
                              (buffer-name buf)))))
                   (buffer-list)))))))
    (define-key erc-mode-map (kbd "C-c b") #'x/ido-chat-buffer))

  (if nil ;;(eq my-curr-computer 'wild-dog)
      (my-erc-set-data))

  (setq erc-header-line-format nil) ; hide header for more screen space.

  (defun my-setup-erc ()
    (setq show-trailing-whitespace nil))
  (add-hook 'erc-mode-hook #'my-setup-erc))

;;;----------------------------------------------------------------------------
;;; erc-hl-nicks
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/erc-hl-nicks" load-path)
(autoload #'erc-hl-nicks-force-nick-face "erc-hl-nicks" nil nil)
(autoload #'erc-hl-nicks-alias-nick "erc-hl-nicks" nil nil)
(autoload #'erc-hl-nicks "erc-hl-nicks" nil nil)

(with-eval-after-load 'erc
  (require 'erc-hl-nicks)
  (add-to-list 'erc-modules 'hl-nicks t))

;;;----------------------------------------------------------------------------
;;; linum-relative
;;;----------------------------------------------------------------------------
;; ;; (require 'linum-relative) ; `linum-mode's behavior is changed by the
;; ;;                           ; linum-relative package.
;; (autoload 'linum-relative-toggle "linum-relative" "linum-relative" t)

;; ;; don't turn on by default. Makes the screen blink when line # changes.
;; ;;(linum-relative-toggle) ;;toggle between relative and straight.
;; (with-eval-after-load "linum-relative"
;;   ;; rel nums should never exceed 2 digits.
;;   (setq linum-relative-format "%2s")
;;   (setq linum-relative-current-symbol "0"))


;;;----------------------------------------------------------------------------
;;; guide-key
;;;----------------------------------------------------------------------------
;; (require 'guide-key)

(with-eval-after-load 'guide-key
  ;; (setq guide-key/guide-key-sequence
  ;;       '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
  (setq guide-key/idle-delay 1.0)       ;default
  (setq guide-key/guide-key-sequence '("C-x" "C-c"))
  (setq guide-key/recursive-key-sequence-flag t)
  ;;(setq guide-key/popup-window-position 'bottom)
  )
;; (guide-key-mode 1)



;;;----------------------------------------------------------------------------
;;; unkillable-scratch
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/unkillable-scratch" load-path)
(autoload #'unkillable-scratch "unkillable-scratch" nil t)

(with-eval-after-load 'unkillable-scratch
  (setq unkillable-scratch-behavior 'do-nothing)
  (setq unkillable-scratch-do-not-reset-scratch-buffer t))
(unkillable-scratch 1) ; autoloaded


;;;----------------------------------------------------------------------------
;;; website bookmarks
;;;----------------------------------------------------------------------------
;; (setq my-bookmarks
;;       '((google "www.google.com" (search email maps))
;;         (yahoo "www.yahoo.com" (search email news video))
;;         (msdn "www.msdn.com" (prog c-sharp visual-studio))))

;; (defun my-member-of (vals lst)
;;   (let ((has-a-tag-p nil))
;;     (cl-block check
;;       (dolist (v vals)
;;         (when (memq v lst)
;;           (setq has-a-tag-p t)
;;           (cl-return-from check))))
;;     has-a-tag-p))


;; (defun my-search-bookmarks (bookmarks tags)
;;   (cond
;;    ((null bookmarks) nil)
;;    ((my-member-of tags (third (first bookmarks)))
;;     (cons (first bookmarks)
;;           (my-search-bookmarks (rest bookmarks) tags)))
;;    (t (my-search-bookmarks (rest bookmarks) tags))))


;;;----------------------------------------------------------------------------
;;; ivy-explorer. grid style display of ivy candidates
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/ivy-explorer" load-path)
(autoload #'ivy-explorer-mode "ivy-explorer" nil t)

(defvar my-use-ivy-explorer nil)
;; NOTE: will turn on this mode later in (with-eval-after-load 'ivy) below.

;;;----------------------------------------------------------------------------
;;; swiper. ivy is (or at least was) bundled with swiper. git submodule
;;; ivy
;;; counsel -> provides extra features for completing some things.
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/swiper" load-path)
(autoload #'swiper-isearch "swiper" nil t)
(autoload #'swiper-avy "swiper" nil t)
(autoload #'ivy-switch-buffer "ivy" nil t)
(autoload #'ivy-completing-read "ivy" nil nil)
(autoload #'counsel-find-file "counsel" nil t)
(autoload #'counsel-switch-buffer "counsel" nil t)
(autoload #'counsel-load-theme "counsel" nil t)
(autoload #'counsel-M-x "counsel" nil t)
(autoload #'counsel-tmm "counsel" nil t)
(autoload #'counsel-describe-function "counsel" nil t)
(autoload #'counsel-describe-variable "counsel" nil t)
(autoload #'counsel-yank-pop "counsel" nil t)
(autoload #'counsel-git "counsel" nil t)
(autoload #'counsel-file-jump "counsel" nil t)
(autoload #'counsel-file-register "counsel" nil t)

;; TODO: set up more autoloads

(when my-use-ivy-p
  (when my-use-evil-p
    (evil-leader/set-key "b" #'ivy-switch-buffer))

  (when (eq my-ui-type 'emacs)
    (global-set-key (kbd "C-c C-s") #'swiper-isearch)
    (global-set-key (kbd "C-c C-b") #'ivy-switch-buffer))

  (progn ;; counsel completion augmentation

    (autoload #'counsel-tmm "counsel" nil t) ;; not autoloaded by default.
    (defun my-counsel-tmm ()
      "Same as `counsel-tmm' but with a taller window."
      (interactive)
      (let ((ivy-height 1000))
        (call-interactively #'counsel-tmm)))
    ;; (global-set-key (kbd "C-c m") #'my-counsel-tmm)

    (global-set-key (kbd "M-x") #'counsel-M-x)
    ;; ivy-explorer doesn't seemt to work with `counsel-find-file'.
    (unless my-use-ivy-explorer
      (global-set-key (kbd "C-x C-f") #'counsel-find-file))

    (global-set-key (kbd "C-h v") #'counsel-describe-variable)
    (global-set-key (kbd "C-h f") #'counsel-describe-function)
    ;; ;; replace keybind for `bookmark-bmenu-list'
    ;; (global-set-key (kbd "C-x r l") #'counsel-bookmark)
    (when my-use-evil-p
      (evil-leader/set-key "w" #'counsel-yank-pop)
      ;; (evil-leader/set-key "h" #'counsel-git) ; safe on ms-windows
      )))

(with-eval-after-load 'swiper
  ;; performance tweak to avoid visual-line-mode
  (setq swiper-use-visual-line-p #'ignore)

  (setq swiper-isearch-highlight-delay '(2 0.8))
  (define-key swiper-map (kbd "C-SPC") #'swiper-avy)
  (define-key swiper-all-map (kbd "C-SPC") #'swiper-avy))

(with-eval-after-load 'ivy
  (setq ivy-height 35)

  (when my-use-ivy-explorer
    ;; turn on grid-style display for find-file
    (ivy-explorer-mode 1))

  (when nil
    ;; an optional 3rd party sorting/filtering for ivy.
    ;; will remember remember past selections during find-file, etc.
    (ivy-prescient-mode 1)
    (prescient-persist-mode))

  (when my-use-evil-p
    ;; use emacs bindings (not evil) in ivy-occur buffer
    (push '("^*ivy-occur" . emacs) evil-buffer-regexps))

  (define-key ivy-occur-mode-map (kbd "n") #'ivy-occur-next-line)
  (define-key ivy-occur-mode-map (kbd "p") #'ivy-occur-previous-line)

  ;; redefine `ivy-help' to use outline mode. To avoid a long wait loading org.
  (defun ivy-help ()
    "Help for `ivy'."
    (interactive)
    (let ((buf (get-buffer "*Ivy Help*")))
      (unless buf
        (setq buf (get-buffer-create "*Ivy Help*"))
        (with-current-buffer buf
          (insert-file-contents ivy-help-file)
          (if (memq 'org features)
              (org-mode)
            (outline-mode))
          (view-mode)
          (goto-char (point-min))))
      (if (eq this-command 'ivy-help)
          (switch-to-buffer buf)
        (with-ivy-window
          (pop-to-buffer buf)))
      (view-mode)
      (goto-char (point-min))))

  ;; TODO: fix keybind to `ivy-avy'. It seems to be triggering outside of
  ;;       ivy-mode-map. Commenting keybind for now.
  ;; (define-key ivy-mode-map (kbd "C-SPC") #'ivy-avy)

  ;; remove the default ^ prefix used by `counsel-M-x' and a few others.
  (cl-loop for pair in ivy-initial-inputs-alist
           do
           (setcdr pair ""))
  ;; NOTE: no longer using `set-alist' to disable the ^ prefix. Because it
  ;;       pulled in a new package dependency `apel'. And maybe `flim'?
  ;; (set-alist 'ivy-initial-inputs-alist 'counsel-M-x "")

  ;; turn on ivy completion. turned on when an autoloaded fn is used with a
  ;; keybind to slightly improve emacs init time. (discovered with
  ;; profile-dotemacs.el)
  (when my-use-ivy-p ;; GUARD. I use swiper even when using ido, so guard
    (ivy-mode 1))    ;; against ivy-mode turning on.

  ;; ;; allow out of order matching.
  ;; (setq ivy-re-builders-alist
  ;;       '((t . ivy--regex-ignore-order)))

  ;; helper function to cycle the ivy matching styles.
  ;; NOTE: periodically look for new supported styles in ivy and add them to
  ;;       the local styles to cycle.
  (let ((styles '(ivy--regex-plus
                  ivy--regex-ignore-order
                  ivy--regex-fuzzy
                  ivy--regex
                  regexp-quote))
        (curr 'ivy--regex-plus))
    (defun my-cycle-ivy-match-style ()
      (interactive)
      (setq curr (car (or (cdr (memq curr styles))
                          styles)))
      (setq ivy-re-builders-alist `((t . ,curr)))
      (message "swiper match style: %s" (symbol-name curr))))

  ;; (global-set-key (kbd "<f7>") #'my-cycle-ivy-match-style)

  ;; use fancy highlights in the popup window
  (setq ivy-display-style 'fancy))

;; (with-eval-after-load "swiper"
;;   ;; overwrite `swiper--re-builder' to use out-of-order matching.
;;   ;; TODO: periodically sync this up with the latest package code.
;;   ;; TODO: make it highlight each match, not just the first.
;;   (defun swiper--re-builder (str)
;;     "Transform STR into a swiper regex.
;; This is the regex used in the minibuffer where candidates have
;; line numbers. For the buffer, use `ivy--regex' instead."
;;     (let ((re (cond
;;                ((equal str "")
;;                 "")
;;                ((equal str "^")
;;                 (setq ivy--subexps 0)
;;                 ".")
;;                ((string-match "^\\^" str)
;;                 (setq ivy--old-re "")
;;                 (let ((re (ivy--regex-ignore-order (substring str 1))) ;;###
;;                       ;; (re (ivy--regex-plus (substring str 1)))
;;                       )
;;                   (if (zerop ivy--subexps)
;;                       (prog1 (format "^ ?\\(%s\\)" re)
;;                         (setq ivy--subexps 1))
;;                     (format "^ %s" re))))
;;                (t
;;                 (ivy--regex-ignore-order str) ;;### I changed this line
;;                 ;; (ivy--regex-plus str)
;;                 ))))
;;       (cond ((stringp re)
;;              (replace-regexp-in-string "\t" "    " re))
;;             ((and (consp re)
;;                   (consp (car re)))
;;              (setf (caar re)
;;                    (replace-regexp-in-string "\t" "    " (caar re)))
;;              re)
;;             (t
;;              (error "Unexpected"))))))

(with-eval-after-load 'counsel
  ;; (setq counsel-grep-base-command
  ;;       "rg -i -M 120 --no-heading --line-number --color never %s %s")

  ;; redefine `counsel--load-theme-action' to not require confirmation
  ;; TODO: find an alternative to redefine so I don't have to manually
  ;;       merge with the latest version of `counsel--load-theme-action'
  ;;       on package updates.
  (defun counsel--load-theme-action (x)
    "Disable current themes and load theme X."
    (condition-case nil
        (progn
          (mapc #'disable-theme custom-enabled-themes)
          (load-theme (intern x) t)
          (when (fboundp 'powerline-reset)
            (powerline-reset)))
      (error "Problem loading theme %s" x))))

;;;----------------------------------------------------------------------------
;;; mish-mash. Keybinds for using several packages for narrowing.
;;;----------------------------------------------------------------------------
(when my-use-mish-mash-p
  ;; set up ivy/swiper/counsel keybinds
  ;; Avoid turning on `ivy-mode' because it replaces the `completing-read' fn
  ;; which i want sometimes for the column-style display.
  (when my-use-evil-p
    (evil-leader/set-key "b" #'switch-to-buffer)
    (evil-leader/set-key "w" #'counsel-yank-pop)
    ;; (evil-leader/set-key "h" #'counsel-git) ; safe on ms-windows
    )
  (global-set-key (kbd "C-h v") #'counsel-describe-variable)
  (global-set-key (kbd "C-h f") #'counsel-describe-function)
  ;; (global-set-key (kbd "M-x") #'counsel-M-x)
  ;; (global-set-key (kbd "C-x C-f") #'counsel-find-file)

  ;; ;; set up ido and smex with flx matching
  ;; (setq ido-enable-flex-matching t)
  ;; (setq ido-use-faces nil) ;; disable ido faces to see flx highlights.
  ;; (global-set-key (kbd "M-x") #'smex)
  ;; (global-set-key (kbd "M-X") #'smex-major-mode-commands)
  ;; ;; (global-set-key (kbd "C-x C-f") #'ido-find-file)
  ;; ;; (ido-mode 1) ;; seems ido fn's don't work unless ido mode is turned on.
  ;; (flx-ido-mode 1)
  ;; (ido-vertical-mode 1)
  )

;;;----------------------------------------------------------------------------
;;; color-identifiers-mode
;;;----------------------------------------------------------------------------
;; (when nil ;sample to test variable colors
;;   (let ((a 0) (b 1) (c 2)
;;         (d 2) (e 4) (f 4) (g 4))
;;     (+ a b c d e f g)))

;; (define-key lisp-mode-shared-map (kbd "C-c r") #'color-identifiers:refresh)
;; (add-hook 'emacs-lisp-mode-hook (lambda () (color-identifiers-mode 1)))

;; (when nil ;insert text of the supported modes into the buffer.
;;   (dolist (m color-identifiers:modes-alist)
;;     (insert (format "%s" m))
;;     (insert "\n")))


;;;----------------------------------------------------------------------------
;;; Integrate narrow-to-region with indirect buffers. To allow multiple
;;; major modes operating on 1 file.
;;;----------------------------------------------------------------------------
(defun my-narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly.
Region defined by START and END is automatically detected by
\(interactive \"r\"\)."
  (interactive "r")
  (deactivate-mark)
  (let ((buf-clone (clone-indirect-buffer nil nil))
        (mode      (intern (completing-read
                            "Mode: "
                            (mapcar (lambda (e)
                                      (list (symbol-name e)))
                                    (apropos-internal "-mode$" #'commandp))
                            nil t))))
    (with-current-buffer buf-clone
      (narrow-to-region start end)
      (funcall mode))
    (switch-to-buffer buf-clone)
    ;;refresh syntax highlighting by toggling `font-lock-mode'
    (cl-loop repeat 2 do
             (font-lock-mode))))

(defun my-narrow-to-region (start end)
  "Narrow to region and invoke an Emacs mode.
START of region.
END of region."
  (interactive "r")
  (deactivate-mark)
  (let ((mode (intern (completing-read
                       "Mode: "
                       (mapcar (lambda (e)
                                 (list (symbol-name e)))
                               (apropos-internal "-mode$" #'commandp))
                       nil t))))
    (narrow-to-region start end)
    (funcall mode)))

;; (defvar indirect-mode-name nil
;;   "Mode to set for indirect buffers.")
;; (make-variable-buffer-local 'indirect-mode-name)
;; (defun indirect-region (start end)
;;   "Edit the current region in another buffer.
;;     If the buffer-local variable `indirect-mode-name' is not set, prompt
;;     for mode name to choose for the indirect buffer interactively.
;;     Otherwise, use the value of said variable as argument to a funcall."
;;   (interactive "r")
;;   (let ((buffer-name (generate-new-buffer-name "*indirect*"))
;;         (mode
;;          (if (not indirect-mode-name)
;;              (setq indirect-mode-name
;;                    (intern
;;                     (completing-read
;;                      "Mode: "
;;                      (mapcar (lambda (e)
;;                                (list (symbol-name e)))
;;                              (apropos-internal "-mode$" 'commandp))
;;                      nil t)))
;;            indirect-mode-name)))
;;     (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
;;     (funcall mode)
;;     (narrow-to-region start end)
;;     (goto-char (point-min))
;;     (shrink-window-if-larger-than-buffer)))


;; ;; `narrow-to-region-indirect' from Zane's blog:
;; ;;    http://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
;; ;; NOTE: toggle `font-lock-mode' twice to clear highlighting.
;; (defun narrow-to-region-indirect (start end)
;;   "Restrict editing in this buffer to the current region, indirectly."
;;   (interactive "r")
;;   (deactivate-mark)
;;   (let ((buf (clone-indirect-buffer nil nil)))
;;     (with-current-buffer buf
;;       (narrow-to-region start end))
;;     (switch-to-buffer buf)))

;;;----------------------------------------------------------------------------
;;; mode-on-region.el in ~/.emacs.d/notElpa/mine/mor/
;;; Create a new buffer, stuff text in it, turn on mode.
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/mine/mor" load-path)
(autoload #'mor-mode-on-region "mode-on-region" nil t)
(autoload #'mor-prev-mode-on-region "mode-on-region" nil t)
(autoload #'mor-curr-mode-on-region "mode-on-region" nil t)
;; For faster init speed, avoid require. However when byte compiling the init
;; file, it can be useful to require the lib before compilation to avoid free
;; variable assignment warnings.
;; (cl-eval-when 'compile (require 'mode-on-region))
;; Config vars
(setq mor-format-automatically-p t)
(setq mor-fix-whitespace-p t)
(setq mor-readonly-for-extra-protection-p t)
(setq mor-prev-mode-fn #'emacs-lisp-mode) ; default so "." works immediately.
(setq mor-allow-tmp-files-p         t
      mor-modes-to-create-tmp-files '(js2-mode js-mode)
      mor-auto-delete-tmp-files-p   t)
;; Face for the selected region when readonly mode is enabled.
(custom-set-faces `(mor-readonly-face ((t :background "black"
                                          :foreground "red"
                                          :strike-through t))))
;; Recommended key binds for vanilla Emacs.  Press "C-c m" with text
;; highlighted.
(global-set-key (kbd "C-c m") #'mor-mode-on-region)
(global-set-key (kbd "C-c .") #'mor-prev-mode-on-region)
(global-set-key (kbd "C-c r") #'mor-curr-mode-on-region)
(with-eval-after-load 'evil
  ;; Recommended key binds for evil users.  Press "m" in visual mode.
  (define-key evil-visual-state-map (kbd "m") #'mor-mode-on-region)
  (define-key evil-visual-state-map (kbd ".") #'mor-prev-mode-on-region)
  (define-key evil-visual-state-map (kbd "r") #'mor-curr-mode-on-region))
(with-eval-after-load 'mode-on-region
  ;; Recommended key binds for the tmp buffer.  Both Vanilla and Evil.
  (define-key mor-tmp-buffer-mode-map (kbd "C-c b") #'mor-copy-back)
  (define-key mor-tmp-buffer-mode-map (kbd "C-c c") #'mor-close-tmp-buffer))

;;;----------------------------------------------------------------------------
;;; Focus javascript
;;;----------------------------------------------------------------------------
;; delay execution of this code until `web-mode' is turned on.
(with-eval-after-load 'web-mode
  ;;needed to bind a key for `js2-mode-map'.
  ;;(require 'cl-lib) is also needed but occurs higher up in this file.
  (require 'js2-mode)

  (defun my-js2-mode-on-region (start end)
    "Narrow on the active region, then turn on js2-mode."
    (interactive "r")
    (deactivate-mark)
    (narrow-to-region start end)
    (js2-mode))

  (cl-defun my-focus-javascript () ;using `cl-defun' to allow `cl-return-from'
    "Automatically narrow between <script> tags, then turn on js2-mode."
    (interactive)
    (save-excursion ;; don't allow tag searches to mess with cursor position.
      (let ((start-tag-name "<script")
            (end-tag-name   "</script")
            (start          nil)
            (end            nil))
        ;; Find start tag. Search backwards first to give priority to tag pairs
        ;; the cursor is currently inside.
        (setq start (search-backward start-tag-name nil t))
        (when (null start)
          ;; if start tag not found backwards, then try forwards.
          (setq start (search-forward start-tag-name nil t)))
        (when (null start)
          (message "start tag not found")
          (cl-return-from my-focus-javascript nil))
        ;;start is found, move to the closing bracket >
        (let ((end-of-start (search-forward ">" nil t)))
          (when (null end-of-start)
            (message "start tag not found")
            (cl-return-from my-focus-javascript nil)))
        ;; start highlighting
        ;; (next-line)
        ;; (move-beginning-of-line nil)
        (set-mark-command nil)           ;(evil-visual-line)
        ;; jump to end tag. always search forward
        (setq end (search-forward end-tag-name nil t))
        (when (null end)
          (deactivate-mark)
          (message "end tag not found")
          (cl-return-from my-focus-javascript nil))
        (let ((start-of-end (search-backward "<" nil t)))
          (when (null start-of-end)
            (message "end tag not found")
            (cl-return-from my-focus-javascript nil)))
        ;;end tag is found.
        ;; (previous-line)
        ;; (move-end-of-line nil)
        ;; turn on js2-mode for this region. (and narrow)
        (call-interactively #'my-js2-mode-on-region))))

  (cl-defun my-focus-javascript2 ()
    "Same as `my-focus-javascript2' but use my mor package to open in a new
buffer instead of narrowing."
    (interactive)
    (save-excursion ;; don't allow tag searches to mess with cursor position.
      (let ((start-tag-name "<script")
            (end-tag-name   "</script")
            (start          nil)
            (end            nil))
        ;; Find start tag. Search backwards first to give priority to tag pairs
        ;; the cursor is currently inside.
        (setq start (search-backward start-tag-name nil t))
        (when (null start)
          ;; if start tag not found backwards, then try forwards.
          (setq start (search-forward start-tag-name nil t)))
        (when (null start)
          (message "start tag not found")
          (cl-return-from my-focus-javascript nil))
        ;;start is found, move to the closing bracket >
        (let ((end-of-start (search-forward ">" nil t)))
          (when (null end-of-start)
            (message "start tag not found")
            (cl-return-from my-focus-javascript nil)))
        ;; start highlighting
        ;; (next-line)
        ;; (move-beginning-of-line nil)
        (set-mark-command nil)           ;(evil-visual-line)
        ;; jump to end tag. always search forward
        (setq end (search-forward end-tag-name nil t))
        (when (null end)
          (deactivate-mark)
          (message "end tag not found")
          (cl-return-from my-focus-javascript nil))
        (let ((start-of-end (search-backward "<" nil t)))
          (when (null start-of-end)
            (message "end tag not found")
            (cl-return-from my-focus-javascript nil)))
        ;;end tag is found.
        ;; (previous-line)
        ;; (move-end-of-line nil)
        ;; turn on js2-mode for this region.
        (let ((mor-mode-fn #'js2-mode))
          (call-interactively #'mor-mode-on-region)))))

  (defun my-unfocus-javascript ()
    "Undo the effects of `my-focus-javascript'."
    (interactive)
    (widen)
    (web-mode))

  ;; key bindings
  (define-key web-mode-map (kbd "C-c j") #'my-focus-javascript2)
  ;; TODO: Use a different technique for this keybind. If we didn't enter
  ;; `js2-mode' from `web-mode' then we don't want `my-unfocus-javascript' to
  ;; turn on web-mode.
  (define-key js2-mode-map (kbd "C-c C-c j") #'my-unfocus-javascript))



;;;----------------------------------------------------------------------------
;;; svg-mode-line-themes
;;;----------------------------------------------------------------------------
;; (when nil ;;don't use for now
;;   (when (eq system-type 'gnu/linux)
;;     (require 'svg-mode-line-themes)      ;from melpa
;;     ;;fix fonts?
;;     (let (( theme-archetype (cdr (assoc 'archetype smt/themes)))
;;           ( row-archetype (cdr (assoc 'archetype smt/rows))))
;;       (setf (getf theme-archetype :style)
;;             (list :font-family "Source Code Pro"
;;                   :font-size "40pt"))
;;       (setf (getf row-archetype :baseline) 12))

;;     ;; not from melpa. Example modeline using svg-mode-line-themes
;;     (require 'ocodo-svg-mode-line)
;;     ;;(require 'ocodo-slim-svg-mode-line)
;;     ))

;;;----------------------------------------------------------------------------
;;; isearch
;;;----------------------------------------------------------------------------
(with-eval-after-load "isearch"
  ;; start highlighting a little faster than the default 0.25
  (setq lazy-highlight-initial-delay 0.2)

  (defun my-isearch-forward-symbol-at-point ()
    "Similar to * key in Vim.
Useful for highlighting variables.  Unlike Vim * (which jumps to next match
after point) this remains on the variable your cursor started at. Enters
isearch-mode so you can <C-s> or <C-r> to navigate through the matches."
    (interactive)
    (let ((case-fold-search nil)) ;; case sensitive to match Vim * behavior.
      (isearch-forward-symbol-at-point)))
  (global-set-key (kbd "C-c v") #'my-isearch-forward-symbol-at-point)
  (global-set-key (kbd "C-c C-v") #'my-isearch-forward-symbol-at-point))

;;;----------------------------------------------------------------------------
;;; my-window-search.  Limit isearch to the visible buffer.
;;;----------------------------------------------------------------------------
(autoload #'my-window-search "my-window-search" nil t)
(global-set-key (kbd "C-c s") #'my-window-search)
(global-set-key (kbd "C-c C-s") #'my-window-search)

;; using this binding for swiper when `my-ui-type' is 'emacs
;; (global-set-key (kbd "C-c C-s") #'my-window-search)

;;;----------------------------------------------------------------------------
;;; zoutline. depency of lispy.
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/zoutline" load-path)

;;;----------------------------------------------------------------------------
;;; lispy. git submodle
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/lispy" load-path)
(autoload #'lispy-mode "lispy" nil t)

(when my-use-lispy-p
  (add-hook 'lisp-mode-hook #'lispy-mode) ; for common lisp.
  ;; now that I default the *scratch* buffer to fundamental-mode, the slow
  ;; loading of lispy will not affect startup time.
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
  (add-hook 'lisp-interaction-mode-hook #'lispy-mode)
  (add-hook 'ielm-mode-hook             #'lispy-mode)
  (add-hook 'slime-repl-mode-hook       #'lispy-mode))

(with-eval-after-load 'lispy
  (require 'zoutline)
  ;; To improve start up speed move hooks to eval-after-load. Otherwise the
  ;; scratch buffer which uses `lisp-interaction-mode' will load lispy. The
  ;; first lispy start will be manual, then hooks be set up. On work-laptop
  ;; this speeds start up by 3-4 seconds.
  ;; (add-hook 'emacs-lisp-mode-hook       #'lispy-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'lispy-mode)
  ;; (add-hook 'ielm-mode-hook             #'lispy-mode)
  ;; (add-hook 'lisp-interaction-mode-hook #'lispy-mode)
  ;; (add-hook 'scheme-mode-hook           #'lispy-mode)
  ;; (add-hook 'slime-repl-mode-hook       #'lispy-mode)

  (lispy-set-key-theme '(special)) ;; helps when using paredit with lispy.
  ;; (lispy-set-key-theme '(special paredit c-digits)) ;; emulation of paredit.

  (setq lispy-avy-style-char 'pre)
  (setq lispy-avy-style-paren 'at) ;not at-full because parents are 1 char
  (setq lispy-avy-style-symbol 'at-full)

  ;; re-implementing `lispy-eval-and-insert' to always save excursion
  ;; whether it's on the left or right.
  (defun lispy-eval-and-insert (&optional arg)
    "Eval last sexp and insert the result.

When ARG isn't nil, try to pretty print the sexp."
    (interactive "P")
    (let ((lispy-do-pprint arg))
      (cl-labels
          ((doit ()
                 (unless (or (lispy-right-p) (region-active-p))
                   (lispy-forward 1))
                 (let ((str (lispy--eval (lispy--string-dwim))))
                   (newline-and-indent)
                   (insert str)
                   (when (lispy-right-p)
                     (lispy-alt-multiline t)))))
        (save-excursion
          (doit)))))

  ;; make functions so "<" will always go left. ">" will always go right.
  ;; whether that's achieved via a barf or slurp.
  ;; TODO: make it handle number inputs (instead of defaulting to 1).
  (defun my-lispy-go-left-barf-or-slurp ()
    (interactive)
    (if (lispy-left-p)
        (lispy-slurp 1)
      (lispy-barf 1)))
  (defun my-lispy-go-right-barf-or-slurp ()
    (interactive)
    (if (lispy-left-p)
        (lispy-barf 1)
      (lispy-slurp 1)))

  ;; special means the cursor is at a paren (and in evil-insert).
  (lispy-define-key lispy-mode-map-special
      (kbd "<") #'my-lispy-go-left-barf-or-slurp)
  (lispy-define-key lispy-mode-map-special
      (kbd ">") #'my-lispy-go-right-barf-or-slurp)

  ;; don't evaluate/insert on C-j. Use the plain way like paredit.
  (define-key lispy-mode-map (kbd "C-j") #'lispy-newline-and-indent-plain)
  ;; fn `kill-line' was bound to evil-insert C-k earlier. Override for lispy.
  (when my-use-evil-p
    (evil-define-key 'insert lispy-mode-map (kbd "C-k") #'lispy-kill)))




;;;----------------------------------------------------------------------------
;;; Info-mode
;;;----------------------------------------------------------------------------

;; This turns on info mode with the user-friendly GUI.
;; see https://stackoverflow.com/questions/1921049/how-to-open-info-file-in-ema
;; cs-in-info-mode
(defun my-info-mode ()
  "Turn on info mode with the user-friendly GUI."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (info file-name)))
(push '("\\.info\\'" . my-info-mode) auto-mode-alist)

(with-eval-after-load 'info
  ;; rebind keys for vim friendliness.
  ;; original bindings. TODO: bind them to something else. Or just use M-x
  '((n Info-next)
    (p Info-prev)
    (f Info-follow-reference)
    (H describe-mode)
    (L Info-history)
    (w Info-copy-current-node-name)
    (e end-of-buffer)
    (b beginning-of-buffer)
    (g Info-goto-node)
    (s Info-search)
    (SPC Info-scroll-up))
  ;; 3 ways to unbind keys:
  ;; global-unset-key
  ;; local-unset-key
  ;; (define-key KEYMAP KEY nil)
  (when my-use-evil-p
    (define-key Info-mode-map (kbd "n") #'evil-search-next)
    (define-key Info-mode-map (kbd "p") nil)
    (define-key Info-mode-map (kbd "f") #'evil-find-char)
    (define-key Info-mode-map (kbd "H") #'evil-window-top)
    (define-key Info-mode-map (kbd "L") #'evil-window-bottom)
    (define-key Info-mode-map (kbd "w") #'evil-forward-word-begin)
    (define-key Info-mode-map (kbd "e") #'evil-forward-word-end)
    (define-key Info-mode-map (kbd "b") #'evil-backward-word-begin)
    (define-key Info-mode-map (kbd "g") #'evil-goto-first-line)
    (define-key Info-mode-map (kbd "s") my-swoop-fn)
    ;; With the latest version of evil, return no longer follows the nodes.
    ;; So map it explicitly.
    (define-key Info-mode-map (kbd "<return>") #'Info-follow-nearest-node)
    ;; can't get SPC keybind to work so using remap on Info-scroll-up.
    (define-key Info-mode-map [remap Info-scroll-up] #'avy-goto-word-1))
  ;;TODO: figure out how to bind gg for top.

  (unless my-use-evil-p
    ;; evil has this keybind by default. Use it in emacs style too.
    (define-key Info-mode-map (kbd "C-o") #'Info-history-back)))

;;;----------------------------------------------------------------------------
;;; help mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'help-mode
  (when my-use-evil-p
    (define-key help-mode-map (kbd "s") my-swoop-fn)))

;;;----------------------------------------------------------------------------
;;; elisp emacs lisp
;;;----------------------------------------------------------------------------
;; Increase number of list items printed from evaling a lisp expression.  Very
;; annoying when it cuts off with "...".  But still have a cap in case of
;; extreme sizes.
(setq eval-expression-print-length 12000
      eval-expression-print-level   4000)

(defun my-eval-region (start end)
  "Call `eval-region' with t flag to display the result in the echo area.
START and END define the region."
  (interactive "r")
  (eval-region start end t))
(define-key lisp-mode-shared-map (kbd "C-c C-r") #'my-eval-region)

(when my-use-ivy-p
  ;; two different modes (and maps) for elisp:
  (define-key emacs-lisp-mode-map (kbd "C-M-i") #'complete-symbol)
  (define-key lisp-interaction-mode-map (kbd "C-M-i") #'complete-symbol)
  ;; (define-key emacs-lisp-mode-map (kbd "C-M-i") #'counsel-el)
  ;; (define-key lisp-interaction-mode-map (kbd "C-M-i") #'counsel-el)
  )

;; (with-eval-after-load "lisp-mode"
;;   (add-hook 'emacs-lisp-mode-hook
;;             (lambda ()
;;               (push '("lambda" . ?f) prettify-symbols-alist))))

;;;----------------------------------------------------------------------------
;;; cider-style-overlays. file in /notElpa/
;;;----------------------------------------------------------------------------
(with-eval-after-load 'cider-style-overlays
  (setq cider-eval-result-prefix ""))

(defvar my-fancy-overlay-p (not (memq my-curr-computer '(a-laptop-faster)))
  "Whether to use the cider-style overlays to display evaluation results.")

(when my-fancy-overlay-p
  ;; overlays to display eval results
  (require 'cider-style-overlays))

;;;----------------------------------------------------------------------------
;;; popup eval result for emacs lisp.  For leader-E key.
;;;----------------------------------------------------------------------------
(let ((preceding-sexp-fn (if (< emacs-major-version 25)
                             ;; don't hash-quote `preceding-sexp' to trick
                             ;; flycheck and suppress warning.
                             'preceding-sexp
                           #'elisp--preceding-sexp)))
  (defun my-eval-last-sexp ()
    "Eval the last symbolic expression.  Return the value as a string.
Closure over `preceding-sexp-fn'."
    (interactive)
    (let ((val (eval (eval-sexp-add-defvars (funcall preceding-sexp-fn))
                     lexical-binding)))
      (prin1-to-string val))))

(cond
 ;; cider overlays
 (my-fancy-overlay-p
  (defun my-eval-last-sexp-display ()
    (interactive)
    (call-interactively #'eval-last-sexp)))
 ;; popup
 (my-graphic-p
  (defun my-eval-last-sexp-display ()
    (interactive)
    ;; (clippy-say (my-eval-last-sexp))
    (pos-tip-show (my-eval-last-sexp))))
 (t ; else in a terminal, no fancy
  (defun my-eval-last-sexp-display ()
    (interactive)
    (save-excursion
      (evil-append 1)
      (default-indent-new-line)
      (eval-last-sexp t)    ; t to insert result in buffer.
      (evil-normal-state)))))

(when my-use-evil-p
  ;; evaluate lisp expression. Insert result on a new line.
  ;;(evil-leader/set-key "l" "a\C-j\C-u\C-x\C-e")
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "e"
    #'my-eval-last-sexp-display)
  (evil-leader/set-key-for-mode 'lisp-interaction-mode "e"
    #'my-eval-last-sexp-display))

;;;----------------------------------------------------------------------------
;;; elisp-slime-nav
;;; TODO: look into lispy's navigation. Maybe remove this section.
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/elisp-slime-nav" load-path)
;; `elisp-slime-nav-mode' replaces `turn-on-elisp-slime-nav-mode'
(autoload #'elisp-slime-nav-mode "elisp-slime-nav" nil t)
(autoload #'elisp-slime-nav-find-elisp-thing-at-point "elisp-slime-nav" nil t)
(autoload #'elisp-slime-nav-describe-elisp-thing-at-point "elisp-slime-nav" nil
  t)

(dolist (hook '(emacs-lisp-mode-hook
                ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(with-eval-after-load 'elisp-slime-nav

  (defun my-elisp-slime-nav-colored ()
    (interactive)
    (call-interactively #'elisp-slime-nav-describe-elisp-thing-at-point)
    (with-current-buffer (help-buffer)
      (rainbow-delimiters-mode-enable)))

  ;; evil-mode stole the keybinds! take them back.
  (when my-use-evil-p
    (evil-define-key 'normal elisp-slime-nav-mode-map
      (kbd "M-.") #'elisp-slime-nav-find-elisp-thing-at-point)
    (evil-define-key 'normal elisp-slime-nav-mode-map
      (kbd "M-,") #'pop-tag-mark)
    (evil-define-key 'normal elisp-slime-nav-mode-map
      (kbd "C-c C-d d") #'my-elisp-slime-nav-colored)
    (evil-define-key 'normal elisp-slime-nav-mode-map
      (kbd "C-c C-d C-d") #'my-elisp-slime-nav-colored)))

;;;----------------------------------------------------------------------------
;;; maximize screen real-estate. TODO: complete this.
;;;----------------------------------------------------------------------------
(autoload #'my-real-estate-max "my-screen-real-estate" nil t)
;; see corresponding function `my-real-estate-restore'
(autoload #'my-real-estate-hide-mode-line "my-screen-real-estate" nil t)
(autoload #'my-real-estate-hide-fringe "my-screen-real-estate" nil t)

;;;----------------------------------------------------------------------------
;;; electric-spacing
;;;----------------------------------------------------------------------------
;; originally called smart-operator-mode.
;; `electric-spacing-mode' is autoloaded.


(with-eval-after-load 'electric-spacing
  ;; remove semi-colon ";". It adds newlines after ; which duplicates
  ;; the auto-newline feature in cc-mode. Coudl be useful for non-cc modes
  ;; but for now just remove it entirely.
  (cl-delete ?\; electric-spacing-operators)

  (setq electric-spacing-control-statement-parens t)
  (setq electric-spacing-double-space-docs t)

  ;; redefine package fn to not always insert newline.
  ;; Hamfisted approach. Occasionally look at package code to see if this
  ;; function is still relevant.
  (defun electric-spacing-\; ()
    "See `electric-spacing-insert'."
    (insert ";")
    (indent-according-to-mode)

    (unless (or (my-search-line-backwards "return")
                (my-next-char-}-p))
      (newline)
      (indent-according-to-mode))))


;;;----------------------------------------------------------------------------
;;; flymake-jslint
;;;----------------------------------------------------------------------------
;; (with-eval-after-load "flymake-jslint"
;;   (setq flymake-jslint-command "jslint")
;;   (setq flymake-jslint-args nil))

;;;----------------------------------------------------------------------------
;;; nlinum
;;;----------------------------------------------------------------------------
;; (with-eval-after-load "nlinum"
;;   )

;;;----------------------------------------------------------------------------
;;; nlinum-relative
;;;----------------------------------------------------------------------------
(with-eval-after-load 'nlinum-relative
  (setq nlinum-relative-current-symbol "0"))

;;;----------------------------------------------------------------------------
;;; sx
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/sx.el" load-path)
(autoload #'sx-tab-all-questions "sx-tab" nil t)
(autoload #'sx-tab-unanswered "sx-tab" nil t)
(autoload #'sx-tab-unanswered-my-tags "sx-tab" nil t)
(autoload #'sx-tab-featured "sx-tab" nil t)
(autoload #'sx-tab-starred "sx-tab" nil t)
(autoload #'sx-tab-frontpage "sx-tab" nil t)
(autoload #'sx-tab-newest "sx-tab" nil t)
(autoload #'sx-tab-topvoted "sx-tab" nil t)
(autoload #'sx-tab-hot "sx-tab" nil t)
(autoload #'sx-tab-week "sx-tab" nil t)
(autoload #'sx-tab-month "sx-tab" nil t)
(autoload #'sx-search "sx-search" nil t)
(autoload #'sx-search-tag-at-point "sx-search" nil t)
(autoload #'sx-open-link "sx-interaction" nil t)
(autoload #'sx-org-get-link "sx-interaction" nil t)
(autoload #'sx-ask "sx-interaction" nil t)
(autoload #'sx-authenticate "sx-auth" nil t)
(autoload #'sx-bug-report "sx" nil t)
(autoload #'sx-inbox "sx-inbox" nil t)
(autoload #'sx-inbox-notifications "sx-inbox" nil t)

(with-eval-after-load 'sx-tab
  ;; supposed to be autolaoded?
  (define-prefix-command 'sx-switchto-map)

  ;; TODO: this is not removing the 100 max limit. make it work.
  ;; using 'around' advice on `sx-tab-newest'
  (defadvice sx-tab-newest (around no-helm-limit)
    ;; temporarily remove the helm candidate limit. (via dynamic binding).
    (let ((helm-candidate-number-limit nil))
      ad-do-it))
  (ad-activate 'sx-tab-newest)

  ;; turn off ido completion for sx. Prefer the many columns of the default
  ;; emacs completion so I can see more options.
  (defadvice sx-completing-read (around no-ido)
    (let ((ido-mode nil))
      ad-do-it))
  (ad-activate 'sx-completing-read)

  (when my-use-evil-p
    ;; use emacs bindings (not evil).
    (push '("\\*question-list" . emacs) evil-buffer-regexps)
    (push '("\\*sx-" . emacs) evil-buffer-regexps)))



;;;----------------------------------------------------------------------------
;;; my-date-stuff.el
;;;----------------------------------------------------------------------------
(autoload #'my-insert-date-big "my-date-stuff" nil t)
(autoload #'my-insert-date-short "my-date-stuff" nil t)
(autoload #'my-insert-date-string-new-buff "my-date-stuff" nil t)
(global-set-key (kbd "C-c i") #'my-insert-date-short)
(global-set-key (kbd "C-c C-i") #'my-insert-date-short)
(global-set-key (kbd "C-c C-c i") #'my-insert-date-string-new-buff)
(global-set-key (kbd "C-c C-c C-i") #'my-insert-date-string-new-buff)

(when my-use-evil-p
  (with-eval-after-load "my-date-stuff"
    ;; TODO: figure out how to get "q" to work for evil normal mode.
    (define-key my-date-mode-map (kbd "C-c c") #'my-quit-window-date)))

;;;----------------------------------------------------------------------------
;;; iedit. dependency from lispy.
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/iedit" load-path)
(autoload #'iedit-mode "iedit" nil t)
(autoload #'iedit-mode-toggle-on-function "iedit" nil t)
(autoload #'iedit-rectangle-mode "iedit-rect" nil t)

;; avoid warning popup for global keybind of C-;
;; I use that for the universal vim escape.
;; Doesn't work in eval-after-load because the warning code runs during the
;; load.
;; TODO: submit a patch upstream so I can set the var nil in eval-after-load.
(setq iedit-toggle-key-default nil)

;;;----------------------------------------------------------------------------
;;; universal vim escape. Without key-chord dependence
;;;----------------------------------------------------------------------------
;; rebind iedit-mode to another key. (it used C-; by default)
(global-set-key (kbd "C-c ;") #'iedit-mode)

(global-set-key (kbd "C-;") #'keyboard-escape-quit)
;; NOTE: can't wrap eval-after-loads in a let because it doesn't evaluate
;; the key, and then key doesn't exist by the time the file loads. Just
;; duplicate the `kbd' for now
(with-eval-after-load 'evil
  (let ((esc (kbd "C-;")))
    (define-key evil-insert-state-map esc #'evil-normal-state)
    (define-key evil-visual-state-map esc #'evil-exit-visual-state)
    (define-key evil-normal-state-map esc #'evil-force-normal-state)
    (define-key evil-ex-completion-map esc #'abort-recursive-edit)
    (define-key evil-read-key-map esc #'keyboard-quit)
    (define-key evil-replace-state-map esc #'evil-normal-state)))
(with-eval-after-load "helm"
  (define-key helm-map (kbd "C-;") #'helm-keyboard-quit))
(with-eval-after-load 'ivy
  (define-key ivy-mode-map (kbd "C-;") #'keyboard-escape-quit))

;;;----------------------------------------------------------------------------
;;; list-processes
;;;----------------------------------------------------------------------------
(with-eval-after-load 'simple

  (defun my-delete-process-at-point ()
    (interactive)
    (let ((process (get-text-property (point) 'tabulated-list-id)))
      (cond ((and process
                  (processp process))
             (delete-process process)
             (revert-buffer))
            (t
             (error "No process at point!")))))

  (define-key process-menu-mode-map (kbd "C-k") #'my-delete-process-at-point))

;;;----------------------------------------------------------------------------
;;; shell-script-mode. (alias for sh-mode)
;;;----------------------------------------------------------------------------
(push '("\\.gitignore$" . shell-script-mode) auto-mode-alist)
;; (add-to-list 'auto-mode-alist '("\\.ratpoisonrc$" . sh-mode))



;;;----------------------------------------------------------------------------
;;; whitespace
;;;----------------------------------------------------------------------------
;;show trailing whitespace.
;; (add-hook 'prog-mode-hook (lambda ()
;;                             (setq show-trailing-whitespace t)))

;; (defun my-toggle-show-trailing-whitespace ()
;;   (interactive)
;;   ;; toggle bool flag.
;;   (setq show-trailing-whitespace (not show-trailing-whitespace))
;;   ;;visual state makes the display refresh.
;;   (evil-visual-char)
;;   (evil-exit-visual-state))
;; (global-set-key (kbd "C-c t") #'my-toggle-show-trailing-whitespace)
;; (global-set-key (kbd "C-c C-t") #'my-toggle-show-trailing-whitespace)

;;******** whitespace-mode *******
;; (require 'whitespace)
(with-eval-after-load 'whitespace
  (setq-default whitespace-line-column 79)
  ;;(setq whitespace-style '(face lines-tail))
  (setq-default whitespace-style
                '(face
                  trailing
                  ;; show hidden spaces before a tab. Useful when using tabs
                  ;; for indentation because spaces before a tab are invisible
                  ;; if they don't overflow the tab-stop.
                  space-before-tab::tab)))

(with-eval-after-load 'prog-mode
  (defun my-setup-prog-mode ()
    (whitespace-mode 1))
  (add-hook 'prog-mode-hook #'my-setup-prog-mode))
;;(global-whitespace-mode 1)

;;;----------------------------------------------------------------------------
;;; ov
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/ov" load-path)
(autoload #'ov-clear "ov" nil t)
;; at the moment ov is just used as a dependency for sallet.

;;;----------------------------------------------------------------------------
;;; deferred
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/emacs-deferred" load-path)
;; at the moment deferred is just used as a dependency for sallet.

;;;----------------------------------------------------------------------------
;;; sallet. from fuco. saved to notElpa folder as a git submodule.
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/sallet" load-path)
(autoload #'sallet-buffer "sallet" nil t)

;; NOTE: the sallet package is not ready yet. Just configuring to give it a
;;       test run.
(when (eq my-narrow-type 'sallet)
  ;; install packages sallet depends on.
  ;; TODO: look into a way to make this happen without code for a specific
  ;; case. (sallet is from git, not package manager). Maybe scrape out the
  ;; require symbols?
  (when nil ; no longer using package.el but keep this code for doc purposes.
    (require 'package)
    (dolist (pkg '(dash s async flx ov f))
      (unless (package-installed-p pkg)
        (package-install pkg))))

  ;; keybinds
  (when my-use-evil-p
    (evil-leader/set-key "b" #'sallet-buffer)))


;;;----------------------------------------------------------------------------
;;; ctrlf. by raxod502. saved to notElpa folder as a git submodule.
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/ctrlf" load-path)
;; NOTE: ctrlf-mode sets up keybindings on (C-s, C-r, C-M-s, C-M-r, M-s w,
;; M-s _, M-s .)
(autoload #'ctrlf-mode "ctrlf" nil t)
;; NOTE: will use this in conjunction with selectrum by the same author.

;;;----------------------------------------------------------------------------
;;; prescient. by raxod502. saved to notElpa folder as a git submodule.
;;;----------------------------------------------------------------------------
;; the author names the base git repo folder "prescient.el". It's a folder,
;; not an elisp file
(push "~/.emacs.d/notElpa/prescient.el" load-path)
(autoload #'selectrum-prescient-mode "selectrum-prescient" nil t)
(autoload #'ivy-prescient-mode "ivy-prescient" nil t)
(autoload #'prescient-persist-mode "prescient" nil t)

;;;----------------------------------------------------------------------------
;;; selectrum. by raxod502. saved to notElpa folder as a git submodule.
;;;----------------------------------------------------------------------------
;; NOTE: selectrum git submodule removed. It has been replaced by vertico.
;; (push "~/.emacs.d/notElpa/selectrum" load-path)
;; ;; add an autoload so I can use selectrum even if it's not `my-narrow-type'.
;; (autoload #'selectrum-mode "selectrum" nil t)

;; (with-eval-after-load 'selectrum
;;   (when my-use-evil-p
;;     (evil-leader/set-key "b" #'switch-to-buffer))

;;   ;; NOTE: the selecturm author reccomends using his prescient for sorting.
;;   (selectrum-prescient-mode 1)
;;   (prescient-persist-mode))

;; (when (eq my-narrow-type 'selectrum)
;;   (selectrum-mode 1)
;;   ;; swiper-like search by the same author raxod502
;;   (ctrlf-mode))


;;;----------------------------------------------------------------------------
;;; sunrise-commander. saved to notElpa folder as a git submodule.
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/sunrise-commander" load-path)
(autoload #'sunrise-cd "sunrise" nil t)

;;;----------------------------------------------------------------------------
;;; winner-mode
;;;----------------------------------------------------------------------------
(setq winner-dont-bind-my-keys t) ; doesn't work when set in eval-after-load.
(with-eval-after-load 'winner
  ;; reducing size from 200. Just need to facilitate a few quick undos.
  (setq winner-ring-size 8)
  (define-key winner-mode-map (kbd "C-c u") #'winner-undo)
  ;; NOTE: `winner-redo' only works if invoked immediately after `winner-undo'.
  ;; TODO: find a way to make this keybind exist temporarily after the undo.
  (define-key winner-mode-map (kbd "C-c r") #'winner-redo))
;; (winner-mode 1) ; portable dumper doens't like winner. keep off for now.

;;;----------------------------------------------------------------------------
;;; js2-highlight-vars
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/js2-highlight-vars.el" load-path)
(autoload #'js2-highlight-vars-mode "js2-highlight-vars" nil t)

(with-eval-after-load 'js2-highlight-vars

  ;; re-define `js2--do-highlight-vars' without calling `top-level'.
  ;; The package author calls it to overcome some sort of bug in emacs, but
  ;; it overwrites the error messages of js2.
  ;; NOTE: periodically sync this re-definition up with the latest from the
  ;;       package.
  ;; TODO: look into the issue deeper. Find/resolve the root issue upstream
  ;;       either in Emacs itself of the js2-highlight-vars package.
  (defun js2--do-highlight-vars ()
    "Highlight variable under cursor within the defining scope"
    (interactive)
    (setq js2--highlight-vars-post-command-timer nil)
    (unless js2--highlight-vars-tokens
      (let ((node (js2-node-at-point))
            (tokens nil)
            name
            scope)
        (unless (js2-name-node-p node)
          (setq node (js2-node-at-point (- (point) 1))))
        (when (and node (js2-name-node-p node))
          (setq scope (js2-node-get-enclosing-scope node)
                name (js2-name-node-name node)
                js2--highlight-vars-current-token (js2-node-abs-pos node)
                js2--highlight-vars-current-token-name name)
          (setq scope (js2-get-defining-scope scope name))
          (with-silent-modifications
            (js2-visit-ast
             scope
             (lambda (node end-p)
               (when (and (not end-p)
                          (js2-name-node-p node)
                          (string= name (js2-name-node-name node)))
                 (let* ((beg (js2-node-abs-pos node))
                        (end (+ beg (js2-node-len node)))
                        (new-scope (js2-node-get-enclosing-scope node))
                        (new-scope (js2-get-defining-scope new-scope name))
                        (ovl (make-overlay beg end)))
                   (add-to-list 'tokens beg t)
                   (overlay-put ovl 'keymap js2-highlight-vars-local-keymap)
                   (overlay-put ovl 'face
                                (if (eq new-scope scope)
                                    'js2-highlight-vars-face
                                  'js2-highlight-vars-second-face))
                   (overlay-put ovl 'evaporate t)
                   (overlay-put ovl 'js2-highlight-vars t)))
               t)))
          (setq js2--highlight-vars-tokens tokens)
          ;; (top-level)
          ))))

  (defvar my-js2-highlight-var-delay 0.0)
  ;; redefine `js2-highlight-vars-post-command-hook' to replace a hard coded
  ;; value 0.5 with a variable.
  ;; TODO: contribute upstream so I don't have to redefine the function.
  (defun js2-highlight-vars-post-command-hook ()
    (ignore-errors
      (let* ((overlays (overlays-at (point)))
             (ovl (and overlays
                       (catch 'found
                         (dolist (ovl overlays)
                           (when (overlay-get ovl 'js2-highlight-vars)
                             (throw 'found ovl)))
                         nil))))
        (if (and ovl
                 (string= js2--highlight-vars-current-token-name
                          (buffer-substring (overlay-start ovl)
                                            (overlay-end ovl))))
            (setq js2--highlight-vars-current-token (overlay-start ovl))
          (js2--unhighlight-vars)
          (when js2--highlight-vars-post-command-timer
            (cancel-timer js2--highlight-vars-post-command-timer))
          (setq js2--highlight-vars-post-command-timer
                (run-with-timer my-js2-highlight-var-delay
                                nil
                                'js2--do-highlight-vars)))))))

(when my-use-js2-highlight-vars-p
  (with-eval-after-load 'js2-mode
    (add-hook 'js2-mode-hook #'js2-highlight-vars-mode)))


;;;----------------------------------------------------------------------------
;;; bufftodo. ~/.emacs.d/notElpa/mine/bufftodo.el
;;;----------------------------------------------------------------------------
;; (autoload #'bufftodo-ui "bufftodo" nil t)

;; (when my-use-evil-p
;;   (define-key evil-normal-state-map (kbd "\\") #'bufftodo-ui))

;; (with-eval-after-load 'bufftodo
;;   (setq bufftodo-open-new-window-p nil))

;;;----------------------------------------------------------------------------
;;; function-args
;;;----------------------------------------------------------------------------

;;;----------------------------------------------------------------------------
;;; my-relative-num
;;;----------------------------------------------------------------------------
;; (when nil
;;   ;; TODO: put in a package. look for the ideal built in functions.
;;   (defun my-curr-line ()
;;     "Like `what-line' but return an integer instead of a message."
;;     (interactive)
;;     (let ((start (point-min))
;;           (n (line-number-at-pos)))
;;       (if (= start 1)
;;           n
;;         (save-excursion
;;           (save-restriction
;;             (widen)
;;             (+ n (line-number-at-pos start) -1))))))

;;   (defun my-top-screen-line ()
;;     (interactive)
;;     (line-number-at-pos (window-start)))

;;   (defun my-bottom-screen-line ()
;;     (interactive)
;;     (line-number-at-pos (- (window-end) 1)))

;;   (when nil ;; interactive testing
;;     (my-top-screen-line)
;;     (my-bottom-screen-line)))

;;;----------------------------------------------------------------------------
;;; my-test
;;;----------------------------------------------------------------------------
(autoload 'my-deftest "my-test" nil nil 'macro)


;;;----------------------------------------------------------------------------
;;; highlight-indent-guides
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/highlight-indent-guides" load-path)
(autoload #'highlight-indent-guides-auto-set-faces "highlight-indent-guides"
  nil t)
(autoload #'highlight-indent-guides-mode "highlight-indent-guides" nil t)

(with-eval-after-load 'highlight-indent-guides
  (setq highlight-indent-guides-method 'fill ;;'character
        highlight-indent-guides-character ?\|))


;;;----------------------------------------------------------------------------
;;; python-ts-mdoe
;;;----------------------------------------------------------------------------
(when (treesit-language-available-p 'python)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

;; python-ts-mode lives in the same file as python-mode. So it may not need
;; to duplicate every part of the old python config below. just set the hook
;; stuff?
(with-eval-after-load 'python
  (defun my-setup-python-ts-mode ()
    (setq tab-width 4) ; python.el sets this to 8, so make sure to overwrite here
    (setq python-indent-offset 4)
    ;; (setq indent-tabs-mode my-use-tabs-python-p) ; buffer local

    (yas-minor-mode 1)
    ;; (rainbow-delimiters-mode-enable)
    (my-turn-on-electric-pair-local-mode)
    (when (fboundp #'display-fill-column-indicator-mode)
      (setq display-fill-column-indicator-column 79) ; PEP 8
      (display-fill-column-indicator-mode 1))

    ;; lsp stuff
    ;; (require 'lsp-python-ms)
    ;; ;; (add-hook 'python-mode-hook #'lsp) ; or lsp-deferred
    )
  (add-hook 'python-ts-mode-hook #'my-setup-python-ts-mode))

;;;----------------------------------------------------------------------------
;;; python-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'python
  ;; defualt to python3 for command `run-python'.
  (setq python-shell-interpreter "python3")

  (defun my-setup-inferior-python-mode ()
    (yas-minor-mode 1)
    (rainbow-delimiters-mode-enable)
    (my-turn-on-electric-pair-local-mode))
  (add-hook 'inferior-python-mode-hook #'my-setup-inferior-python-mode)

  ;; use custom fn to avoid alignment issues with horizontal comments. Tabs
  ;; can't be used *after* code, only before. (otherwise alignment issues
  ;; occur at different tab widths.
  (define-key python-mode-map (kbd "M-;") #'my-comment-dwim-align-with-spaces)


  (defun my-setup-python-smart-tabs-mode ()
    ;; keeps comments closer to the code. buffer local
    (setq comment-column 1) ; buffer local

    (setq tab-width my-indent-width) ; buffer local
    (setq python-indent-offset my-indent-width)
    (setq indent-tabs-mode t) ; buffer local

    (smart-tabs-advice python-indent-line python-indent-offset)
    (smart-tabs-advice python-indent-region python-indent-offset)
    (when (featurep 'python-mode)
      (smart-tabs-advice py-indent-line py-indent-offset)
      (smart-tabs-advice py-newline-and-indent py-indent-offset)
      (smart-tabs-advice py-indent-region py-indent-offset))
    (smart-tabs-mode-enable))

  (defvar my-use-tabs-python-p nil)
  ;; INFO: when working with a "spaces ident" code base switch this "when" to
  ;; nil. I prefer "smart tabs" where tabs are used for indent, spaces for
  ;; alignment. But most people follow PEP 8 which says use 4 spaces.
  (when my-use-tabs-python-p
    ;; hook for smart-tabs-mode
    (add-hook 'python-mode-hook #'my-setup-python-smart-tabs-mode))

  (defun my-setup-python ()
    (setq tab-width my-indent-width) ; python.el sets this to 8, so make sure to overwrite here
    (setq python-indent-offset my-indent-width)
    (setq indent-tabs-mode my-use-tabs-python-p) ; buffer local

    (yas-minor-mode 1)
    (rainbow-delimiters-mode-enable)
    (my-turn-on-electric-pair-local-mode)
    (when (fboundp #'display-fill-column-indicator-mode)
      (setq display-fill-column-indicator-column 79) ; PEP 8
      (display-fill-column-indicator-mode 1))

    ;; lsp stuff
    ;; (require 'lsp-python-ms)
    ;; ;; (add-hook 'python-mode-hook #'lsp) ; or lsp-deferred
    )
  (add-hook 'python-mode-hook #'my-setup-python))

;;;----------------------------------------------------------------------------
;;; calendar
;;;----------------------------------------------------------------------------
(with-eval-after-load 'calendar
  ;; default is 8 which *should* be correct but seems I need to bump up to 9
  ;; to stop the calendar window from resizing.
  (setq calendar-minimum-window-height 9)

  (when my-use-evil-p ;; use emacs, not evil bindings in calendar.
    (push '("\\*Calendar*" . emacs) evil-buffer-regexps)))


;;;----------------------------------------------------------------------------
;;; twelve-m-calendar.el
;;;----------------------------------------------------------------------------
;; (autoload #'year-calendar "twelve-m-calendar" nil t)

;;;----------------------------------------------------------------------------
;;; smart-tabs-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/smarttabs" load-path)
(autoload 'smart-tabs-when "smart-tabs-mode" nil nil t)
(autoload 'smart-tabs-create-advice-list "smart-tabs-mode" nil nil t)
(autoload 'smart-tabs-create-language-advice "smart-tabs-mode" nil nil t)
(autoload 'smart-tabs-mode "smart-tabs-mode" nil t nil)
(autoload 'smart-tabs-mode-enable "smart-tabs-mode" nil nil nil)
(autoload 'smart-tabs-advice "smart-tabs-mode" nil nil t)
(autoload 'smart-tabs-insinuate "smart-tabs-mode" nil nil nil)
(autoload 'smart-tabs-add-language-support "smart-tabs-mode" nil nil t)

;; NOTE: just setting up hooks manually in eval-after-load for specific langs.
;; (smart-tabs-insinuate 'c)
(smart-tabs-insinuate 'javascript)

;;;----------------------------------------------------------------------------
;;; lua-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/lua-mode" load-path)
(autoload #'lua-mode "lua-mode" nil t nil)
(autoload #'lua-start-process "lua-mode" nil t)
(push '("\\.lua\\'" . lua-mode) auto-mode-alist)
;; (push '("lua" . lua-mode) auto-mode-alist)

(with-eval-after-load 'lua-mode
  ;; key binds
  (define-key lua-mode-map (kbd "C-x C-e") #'lua-send-current-line)
  (define-key lua-mode-map (kbd "C-M-x") #'lua-send-defun)
  (define-key lua-mode-map (kbd "C-c C-k") #'lua-send-buffer)
  (define-key lua-mode-map (kbd "C-c C-r") #'lua-send-region)

  (define-key lua-mode-map (kbd "C-c C-c") #'compile)
  (define-key lua-mode-map (kbd "C-c c") #'compile)

  (when my-use-evil-p
    ;; use emacs bindings in lua REPL
    (push '("^*lua*" . emacs) evil-buffer-regexps))

  (setq lua-indent-level 3) ; defualt is 3 but set anyway

  ;; TODO: set to "luajit" on case by case basis, per computer.
  ;; (setq lua-default-application "luajit")

  (cond ((eq my-curr-computer 'work-laptop-2019)
         (setq lua-default-application
               "C:/Users/mtz/programs/lua-5.3.6_Win32_bin/lua53.exe"))
        ((eq my-curr-computer 'work-laptop-mac)
         (setq lua-default-application "/opt/homebrew/bin/lua"))
        ((eq my-curr-computer 'wild-dog)
         (setq lua-default-application "luajit")))

  (defun my-setup-lua-mode ()
    (when t ;; coment out this `when' to use spaces for indentation.
      (setq indent-tabs-mode t)
      ;; NOTE: `tab-width' and `lua-indent-level' must be the same.
      (setq tab-width lua-indent-level)
      (progn ;; smart-tabs-mode
        (smart-tabs-advice lua-indent-line lua-indent-level)
        (smart-tabs-mode-enable)))

    ;; set to 1 so comments on the same line are kept close to the code
    (setq comment-column 1) ;; buffer local

    (when (fboundp #'display-fill-column-indicator-mode)
      (setq display-fill-column-indicator-column 79) ; buffer local
      (display-fill-column-indicator-mode 1))

    (yas-minor-mode 1)
    (rainbow-delimiters-mode 1)
    ;; (electric-spacing-mode 1)
    (my-turn-on-electric-pair-local-mode))
  (add-hook 'lua-mode-hook #'my-setup-lua-mode))

;;;----------------------------------------------------------------------------
;;; swift-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/swift-mode" load-path)
(autoload #'swift-mode "swift-mode" nil t)
;; use swift-mode for .swift files
(push '("\\.swift\\'" . swift-mode) auto-mode-alist)

(with-eval-after-load 'swift-mode
  (defun my-setup-swift-mode ()
    (yas-minor-mode 1)
    (rainbow-delimiters-mode 1)
    (my-turn-on-electric-pair-local-mode))
  (add-hook 'swift-mode-hook #'my-setup-swift-mode))

;;;----------------------------------------------------------------------------
;;; ggtags
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/ggtags" load-path)
(autoload #'ggtags-find-project "ggtags" nil nil nil)
(autoload #'ggtags-find-tag-dwim "ggtags" nil t nil)
(autoload #'ggtags-mode "ggtags" nil t nil)
(autoload #'ggtags-build-imenu-index "ggtags" nil nil nil)
(autoload #'ggtags-build-imenu-index "ggtags" nil nil nil)
(autoload #'ggtags-try-complete-tag "ggtags" nil nil nil)
(autoload #'ggtags-create-tags "ggtags" nil t nil)

;; TODO: fix all the key bindings `ggtags-mode' clobbers. Like M-n, M-p.
(with-eval-after-load 'ggtags
  ;; this messes up keybinds like M-< and M->. Even in buffers not using
  ;; ggtags-mode??? stop that.
  (setq ggtags-enable-navigation-keys nil)
  ;; Don't try to update GTAGS on each save; makes the system sluggish for huge
  ;; projects.
  (setq ggtags-update-on-save nil)
  ;; Don't auto-highlight tag at point.. makes the system really sluggish!
  (setq ggtags-highlight-tag nil)
  (setq ggtags-sort-by-nearness nil) ; Enabling nearness requires global 6.5+
  (setq ggtags-navigation-mode-lighter nil)
  (setq ggtags-mode-line-project-name nil)
  (setq ggtags-oversize-limit (* 30 1024 1024)) ; 30 MB

  (cond
   ((eq my-curr-computer 'work-laptop-2019)
    (setq ggtags-executable-directory "C:/Users/mtz/programs/glo663wb/bin")))

  ;; doesn't work, added to windows path instead.
  ;; (when (eq my-curr-computer 'work-laptop)
  ;;   (add-to-list 'exec-path "C:/Users/mtz/programs/glo653wb/bin"))

  (define-key ggtags-mode-map (kbd "C-c C-d d") #'ggtags-show-definition)
  (define-key ggtags-mode-map (kbd "C-c C-d C-d") #'ggtags-show-definition)

  (when my-use-evil-p
    (evil-define-key 'normal ggtags-mode-map (kbd "M-.")
      #'ggtags-find-tag-dwim)
    ;; `evil-define-key' doesn't work here but `define-key' does?
    (define-key ggtags-mode-map (kbd "M-,") #'pop-tag-mark)

    ;; use emacs bindings (not evil) in the tag list buffer
    (push '("^*ggtags" . emacs) evil-buffer-regexps)))


;;;----------------------------------------------------------------------------
;;; follow-mode
;;;----------------------------------------------------------------------------
(autoload #'my-follow-mode "my-misc" nil t)

(with-eval-after-load 'follow
  ;; scroll the height of all windows combined, not just 1.
  (define-key follow-mode-map (kbd "C-v") #'follow-scroll-up)
  (define-key follow-mode-map (kbd "M-v") #'follow-scroll-down)
  (when my-use-evil-p
    ;; TODO: the keybinds for evil mode don't seem to take hold. Fix it.
    (evil-define-key 'normal follow-mode-map (kbd "C-v") #'follow-scroll-up)
    (evil-define-key 'normal follow-mode-map (kbd "M-v")
      #'follow-scroll-down)))

;;;----------------------------------------------------------------------------
;;; clojure-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'clojure-mode
  (defun my-setup-clojure-mode ()
    (rainbow-delimiters-mode-enable)
    (enable-paredit-mode)
    (when my-use-lispy-p
      (lispy-mode 1)))
  (add-hook 'clojure-mode-hook #'my-setup-clojure-mode))

;;;----------------------------------------------------------------------------
;;; cider
;;;----------------------------------------------------------------------------
(with-eval-after-load 'cider

  ;; when looking up docs with C-c C-c C-d, pop up the doc immediately.
  (setq cider-prompt-for-symbol nil)

  (when (eq my-curr-computer 'work-laptop)
    (push "C:/Users/mtz/.lein/bin" exec-path)
    ;; 7zip so source lookup will work
    ;; (add-to-list 'exec-path "C:/Users/mtz/programs/7zip")
    ))

;; (with-eval-after-load "cider-interaction"
;;   (when my-use-evil-p
;;     (evil-leader/set-key-for-mode 'clojure-mode "e"
;;       #'cider-eval-last-sexp)))

(with-eval-after-load 'cider-repl
  (setq cider-repl-display-help-banner nil) ;; disable wall of text.

  ;; emulate some SLIME keybinds
  (define-key cider-repl-mode-map (kbd "C-c M-o") #'cider-repl-clear-buffer)

  (defun my-setup-cider-repl ()
    (rainbow-delimiters-mode-enable)
    (enable-paredit-mode)
    (when my-use-lispy-p
      (lispy-mode 1)))
  (add-hook 'cider-repl-mode-hook #'my-setup-cider-repl))

;;;----------------------------------------------------------------------------
;;; overlay for emacs lisp.  Dependant on cider.
;;; copied from:
;;; http://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html
;;;----------------------------------------------------------------------------
;; (autoload 'cider--make-result-overlay "cider-overlays")

;; (defun endless/eval-overlay (value point)
;;   (cider--make-result-overlay (format "%S" value)
;;     :where point
;;     :duration 'command)
;;   ;; Preserve the return value.
;;   value)

;; (advice-add 'eval-region :around
;;             (lambda (f beg end &rest r)
;;               (endless/eval-overlay
;;                (apply f beg end r)
;;                end)))

;; (advice-add 'eval-last-sexp :filter-return
;;             (lambda (r)
;;               (endless/eval-overlay r (point))))

;; (advice-add 'eval-defun :filter-return
;;             (lambda (r)
;;               (endless/eval-overlay
;;                r
;;                (save-excursion
;;                  (end-of-defun)
;;                  (point)))))

;;;----------------------------------------------------------------------------
;;; custom-file.
;;; Settings saved from the menu -> Options -> Save Options.
;;;----------------------------------------------------------------------------
(defconst my-load-custom-file-p nil
  "When t load the external custom file on start up.")

;; Store saved settings in a separate file. To keep init.el clean.
;; Also custom.el will be kept out of source control. As it will tend to have
;; many machine specific things like fonts, etc. I don't use the Save Options
;; feature so I'm not concerned about those settings escaping source control.
(setq custom-file "~/.emacs.d/custom.el")

(when my-load-custom-file-p
  (when (file-exists-p custom-file)
    (load custom-file)))

;;;----------------------------------------------------------------------------
;;; occur
;;;----------------------------------------------------------------------------
;; (defun my-occur ()
;;   (interactive)
;;   ;; TODO: get `call-interactively' to work with
;;   ;;       `occur'. Seems to break when I attempt
;;   ;;       to set nlines via `curr-prefix-arg'.
;;   ;; add 2 context lines to occur
;;   (occur (read-string "search: ") 2))

(with-eval-after-load "replace" ;; `occur' lives in replace.el
  ;; ;; automatically scroll buffer to matches like `swiper' or `helm-swoop'.
  ;; (add-hook 'occur-mode-hook #'next-error-follow-minor-mode)

  (defvar my-blink-fn
    (if (>= emacs-major-version 25)
        (progn
          (require 'xref) ;; for fn `xref-pulse-momentarily'. to flash match.
          #'xref-pulse-momentarily)
      #'hl-line-flash) ;; TODO: avoid hl-line+ dependency ;; (hl-line-mode 1)
    "Function to blink the matching line in the buffer from occur-buffer.")

  (progn ;; functions copied from https://github.com/emacsfodder/occur-follow
    (defun my--occur-move (move-fn)
      (funcall move-fn)
      (occur-mode-goto-occurrence-other-window)
      (recenter)
      (funcall my-blink-fn)
      (switch-to-buffer-other-window "*Occur*"))
    (defun my-occur-next ()
      (interactive)
      (my--occur-move #'occur-next))
    (defun my-occur-prev ()
      (interactive)
      (my--occur-move #'occur-prev))
    (defun my-occur-mode-goto-occurrence ()
      "Same as the built in `occur-mode-goto-occurrence', but add a blink."
      (interactive)
      (occur-mode-goto-occurrence)
      (funcall my-blink-fn))
    (define-key occur-mode-map (kbd "n") #'my-occur-next)
    (define-key occur-mode-map (kbd "p") #'my-occur-prev)
    ;; NOTE: purposely not binding C-n, C-p. To navigate without the jump.
    (define-key occur-mode-map (kbd "M-n") #'my-occur-next)
    (define-key occur-mode-map (kbd "M-p") #'my-occur-prev)
    (define-key occur-mode-map (kbd "RET") #'my-occur-mode-goto-occurrence))

  ;; turn off the line highlight when jumping back to the buffer.
  (defadvice occur-mode-goto-occurrence (after turn-off-highlight)
    ;; (hl-line-mode 0)
    ;; close occur window.
    (quit-window nil (get-buffer-window "*Occur*")))
  (ad-activate 'occur-mode-goto-occurrence)

  (defun my-occur-wild-spaces (regexp &optional nlines)
    "Same as `occur'.  But treat spaces as wild cards like in `swiper'."
    (interactive (occur-read-primary-args))
    (occur-1 (replace-regexp-in-string " " ".*" regexp)
             nlines
             (list (current-buffer))))
  ;; ;; treat spaces as wild cards. Like in `swiper'.
  ;; (defadvice occur (around space-to-wild activate compile)
  ;;   (let* ((new-regexp (replace-regexp-in-string " " ".*" regexp))
  ;;          (regexp new-regexp))
  ;;     ad-do-it))

  (defun my--occur-jump-to-first-match ()
    ;; switch to the results window immediately.
    (switch-to-buffer-other-window "*Occur*")
    ;; jump to the first match.
    (my-occur-next))
  (add-hook 'occur-hook #'my--occur-jump-to-first-match))

;; NOTE: replace.el seems to load during init, so `my-occur-wild-spaces' is
;;       defined despite being in eval-after-load.
(global-set-key (kbd "C-c o") #'my-occur-wild-spaces)

;;;----------------------------------------------------------------------------
;;; eldoc
;;;----------------------------------------------------------------------------
(when (and (boundp global-eldoc-mode) ;;(>= emacs-major-version 25)
           global-eldoc-mode)
  (global-eldoc-mode 0))

(with-eval-after-load 'eldoc
  ;; avoid spamming the echo area when the *eldoc* buffer is visible
  (setq eldoc-echo-area-prefer-doc-buffer t))

;;;----------------------------------------------------------------------------
;;; scheme-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'scheme
  (defun my-setup-scheme ()
    (rainbow-delimiters-mode-enable)
    (enable-paredit-mode)
    (when my-use-lispy-p
      (lispy-mode 1)))
  (add-hook 'scheme-mode-hook #'my-setup-scheme))

;;;----------------------------------------------------------------------------
;;; geiser
;;;----------------------------------------------------------------------------
(with-eval-after-load 'geiser
  (when (eq my-curr-computer 'work-laptop)
    (setq geiser-default-implementation 'racket
          geiser-active-implementations '(racket))
    (push "C:/Users/mtz/programs/Racket" exec-path)))

(with-eval-after-load 'geiser-repl
  (defun my-setup-geiser-repl ()
    (rainbow-delimiters-mode-enable)
    (enable-paredit-mode)
    (when my-use-lispy-p
      (lispy-mode 1)))
  (add-hook 'geiser-repl-mode-hook #'my-setup-geiser-repl))

;;;----------------------------------------------------------------------------
;;; bookmark
;;;----------------------------------------------------------------------------
(with-eval-after-load 'bookmark
  ;; use LIFO order, not alphabetical order.
  (setq bookmark-sort-flag nil)
  ;; Don't wait for emacs to be shutdown to save marks to file. Save as we go.
  (setq bookmark-save-flag 1)
  ;; remove hook on emacs-exit. Since we save as we go, it shouldn't be needed.
  (remove-hook 'kill-emacs-hook 'bookmark-exit-hook-internal))


;;;----------------------------------------------------------------------------
;;; autorevert (built into emacs)
;;;----------------------------------------------------------------------------
(with-eval-after-load 'autorevert
  ;; try to use native OS file nodifications if possible
  (setq auto-revert-use-notify t
        auto-revert-avoid-polling t)

  ;; increase the timestamp ping a little.
  (setq auto-revert-interval 7)
  ;; default is t, but set anyway to guarantee
  (setq auto-revert-stop-on-user-input t)
  (setq global-auto-revert-non-file-buffers nil)
  (setq auto-revert-remote-files nil))

;; reload buffer if it changes on disk outside emacs.
(global-auto-revert-mode t)
;; auto refresh dired when file changes
(add-hook 'dired-mode-hook #'auto-revert-mode)


;;;----------------------------------------------------------------------------
;;; markup-faces. dependency for adoc-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/markup-faces" load-path)

;;;----------------------------------------------------------------------------
;;; adoc-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/adoc-mode" load-path)
(autoload #'adoc-mode "adoc-mode" nil t)
(push '("\\.asc\\'" . adoc-mode) auto-mode-alist)
(push '("\\.adoc\\'" . adoc-mode) auto-mode-alist)

(with-eval-after-load 'adoc-mode
  (defun my-setup-adoc-mode ()
    ;; usually for reading books, so use word wrap.
    (visual-line-mode))
  (add-hook 'adoc-mode-hook #'my-setup-adoc-mode))

;;;----------------------------------------------------------------------------
;;; markdown-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/markdown-mode" load-path)
(autoload #'markdown-mode "markdown-mode" nil t)
(autoload #'gfm-mode "markdown-mode" nil t)
(autoload #'markdown-view-mode "markdown-mode" nil t)
(autoload #'gfm-view-mode "markdown-mode" nil t)
(autoload #'markdown-live-preview-mode "markdown-mode" nil t)
(push
 '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
 auto-mode-alist)

(with-eval-after-load 'markdown-mode
  (defun my-setup-markdown-mode ()
    ;; usually for reading books, so use word wrap.
    (visual-line-mode))
  (add-hook 'markdown-mode-hook #'my-setup-markdown-mode))

;;;----------------------------------------------------------------------------
;;; typescript-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/typescript.el" load-path)
(autoload #'typescript-mode "typescript-mode" nil t)
(push '("\\.ts$" . typescript-mode) auto-mode-alist)

(with-eval-after-load 'typescript-mode
  (put 'typescript-indent-level 'safe-local-variable #'integerp)
  (setq typescript-indent-level my-indent-width)

  (defun my-setup-typescript-mode ()
    (yas-minor-mode 1)
    (rainbow-delimiters-mode 1)
    (my-turn-on-electric-pair-local-mode))
  (add-hook 'typescript-mode-hook #'my-setup-typescript-mode))

;;;----------------------------------------------------------------------------
;;; tide
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/tide" load-path)
(autoload #'company-tide "tide" nil t)
(autoload #'tide-format-before-save "tide" nil t)
(autoload #'tide-format "tide" nil t)
(autoload #'tide-setup "tide" nil t)
(autoload #'tide-mode "tide" nil t)
(autoload #'tide-project-errors "tide" nil t)
(autoload #'tide-unhighlight-identifiers "tide" nil t)
(autoload #'tide-hl-identifier "tide" nil t)
(autoload #'tide-hl-identifier-mode "tide" nil t)

(with-eval-after-load 'typescript-mode
  (defun my-setup-tide-for-ts ()
    "The example fn in the tide readme file."
    (interactive)
    (tide-setup)
    (flycheck-mode 1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode 1)
    (tide-hl-identifier-mode 1)
    (company-mode 1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  (add-hook 'typescript-mode-hook #'my-setup-tide-for-ts))



;; set up steps for using tide on regular javascript.
;; see: https://www.reddit.com/r/emacs/comments/68zacv/using_tidemode_to_typech
;; eck_javascript/
;; step 1: download typescript npm package:
;;     npm install -g typescript
;; step 2: put file jsconfig.json in root of project
;; {
;;   "compilerOptions": {
;;     "target": "es2017",
;;     "allowSyntheticDefaultImports": true,
;;     "noEmit": true,
;;     "checkJs": true,
;;     "jsx": "react",
;;     "lib": [ "dom", "es2017" ]
;;   },
;;   "include": [
;;     "./src/"
;;   ]
;; }
;; step 3 (optional): install typings for libraries you use in a project to get
;; even better intelliSense:
;; npm install --save-dev @types/lodash @types/rx @types/react @types/react-dom

;; currently using tide for it's regular javascript features, so have
;; eval-after-load on js2-mode.
(with-eval-after-load 'js2-mode
  (defun my-setup-tide-for-js ()
    "Just like the example fn in the tide readme file, but tailored for use on
vanilla javascript buffers."
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)

    ;; turn off js2-highlight-vars-mode. it duplicates a tide feature
    (when my-use-js2-highlight-vars-p
      (js2-highlight-vars-mode 0)))

  ;; Do not automatically call `my-setup-tide-for-js' for js2 buffers. Because
  ;; I won't always have jsconfig.json set up. Call `my-setup-tide-for-js'
  ;; manually.
  ;; (add-hook 'js2-mode-hook #'my-setup-tide-for-js)
  )


;;;----------------------------------------------------------------------------
;;; display-line-numbers. Native implementation.
;;;----------------------------------------------------------------------------
(when native-line-numbers-p
  ;; (custom-theme-set-faces
  ;;  'zenburn
  ;;  `(line-number ((t (:background "#151515" ;;"#4F4F4F"
  ;;                                 :foreground "gray50"))))
  ;;  ;; `(line-number-current-line ((t :foreground "gray50" :height 1.7)))
  ;;  )

  (setq display-line-number-width 1) ;; 1 seemed to behave like 2.
  (setq display-line-numbers-current-absolute nil)


  (autoload #'my-line-numbers-on "my-line-nums" nil t)
  (autoload #'my-line-numbers-relative-on "my-line-nums" nil t)
  (autoload #'my-line-numbers-off "my-line-nums" nil t)

  (autoload #'my-line-numbers-cycle "my-line-nums" nil t)
  (global-set-key (kbd "<f6>") #'my-line-numbers-cycle))

;;;----------------------------------------------------------------------------
;;; powershell
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/powershell.el" load-path)
(autoload #'powershell-mode "powershell" nil t)
(autoload #'powershell "powershell" nil t)
(push '("\\.ps[dm]?1\\'" . powershell-mode) auto-mode-alist)

(with-eval-after-load 'powershell
  (defun my-setup-powershell-mode ()
    (yas-minor-mode 1)
    (my-turn-on-electric-pair-local-mode)
    (rainbow-delimiters-mode-enable))
  (add-hook 'powershell-mode-hook #'my-setup-powershell-mode))


;;;----------------------------------------------------------------------------
;;; grep settings. built into emacs.
;;;----------------------------------------------------------------------------
(with-eval-after-load 'grep
  (setq grep-highlight-matches t)

  (when my-use-evil-p
    ;; avoid opening help with the "h" key. I can use "C-h m" instead.
    (evil-define-key 'normal grep-mode-map (kbd "h") #'evil-backward-char)))


;;;----------------------------------------------------------------------------
;;; horizontal scrolling
;;;----------------------------------------------------------------------------
(autoload #'my-scroll-right "my-horizontal-scroll" nil t)
(autoload #'my-scroll-left "my-horizontal-scroll" nil t)

(global-set-key (kbd "C-M-h") #'my-scroll-right)
(global-set-key (kbd "C-M-l") #'my-scroll-left)
;; TODO: rebind `mark-defun' (kbd "C-M-h")
;; TODO: rebind `reposition-window' (kbd "C-M-l")

;;;----------------------------------------------------------------------------
;;; css-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'css-mode
  (defun my-setup-css-mode ()
    (my-turn-on-electric-pair-local-mode)
    (rainbow-delimiters-mode-enable))
  (add-hook 'css-mode-hook #'my-setup-css-mode))

;;;----------------------------------------------------------------------------
;;; ibuffer
;;;----------------------------------------------------------------------------
(global-set-key (kbd "C-x C-b") #'ibuffer) ; the way C-x C-b should be.

(with-eval-after-load 'ibuffer
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil) ; hide headers on empty groups
  (setq ibuffer-saved-filter-groups
        '(("lots" ; lots of groups
           ("Source control" (or (name . "^\*magit")
                                 (name . "^\magit") ; stars removed recently?
                                 (name . "^\*vc")))
           ("Emacs-conifg" (or (filename . ".emacs.d")
                               (filename . "init.el")))
           ("Web Dev" (or (mode . web-mode)
                          (mode . html-mode)
                          (mode . js2-mode)
                          (mode . css-mode)))
           ("ERC" (mode . erc-mode))
           ("Help" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*info\*")))
           ("Special" (name . "^\*")))

          ("less" ; fewer groups
           ("Special" (or (name . "^\*")
                          (name . "^\magit")))
           ("ERC" (mode . erc-mode)))))

  (defun my-setup-ibuffer-mode ()
    (ibuffer-switch-to-saved-filter-groups "less"))
  (add-hook 'ibuffer-mode-hook #'my-setup-ibuffer-mode)

  ;; (progn ;; advice for collapsing some groups by default. From emacswiki.

  ;;   (defvar my-ibuffer-collapsed-groups '("Source control" "Special"))

  ;;   (defadvice ibuffer (after collapse-helm)
  ;;     (dolist (group my-ibuffer-collapsed-groups)
  ;;       (progn
  ;;         (goto-char 1)
  ;;         (when (search-forward (concat "[ " group " ]") (point-max) t)
  ;;           (progn
  ;;             (move-beginning-of-line nil)
  ;;             (ibuffer-toggle-filter-group)))))
  ;;     (goto-char 1)
  ;;     (search-forward "[ " (point-max) t)
  ;;     (ibuffer-forward-line)
  ;;     (move-beginning-of-line))
  ;;   (ad-activate 'ibuffer))
  )

;;;----------------------------------------------------------------------------
;;; feebleline
;;;----------------------------------------------------------------------------
(autoload #'feebleline-mode "feebleline" nil t)

(with-eval-after-load 'feebleline
  (setq feebleline-mode-line-text
        '(;; ("%3s" ((format "%s"
          ;;                 (format-mode-line "%l")))
          ;;  (face feebleline-linum-face))
          ;; (" : %s" ((buffer-name))
          ;;  (face feebleline-bufname-face))
          (" %s" ((buffer-name))
           (face feebleline-bufname-face))
          ;; ("%s" ((if (and (buffer-file-name) (buffer-modified-p))
          ;;            "*"
          ;;          "" ))
          ;;  (face feebleline-asterisk-face))
          )))

;; (feebleline-mode 1) ; turn on feebleline

;;;----------------------------------------------------------------------------
;;; my-cycle-line-position
;;;----------------------------------------------------------------------------
(autoload #'my-cycle-line-position "my-cycle-line-position" nil t)
(autoload #'my-evil-goto-page-mid "my-cycle-line-position" nil t)
(global-set-key (kbd "M-r") #'my-cycle-line-position)

;;;----------------------------------------------------------------------------
;;; tern
;;;----------------------------------------------------------------------------

;;;----------------------------------------------------------------------------
;;; company-tern
;;;----------------------------------------------------------------------------
;; (with-eval-after-load 'company
;;   (when has-nodejs-p
;;     (push 'company-tern company-backends)))

;;;----------------------------------------------------------------------------
;;; browse-kill-ring
;;;----------------------------------------------------------------------------
;; (global-set-key (kbd "M-y") #'browse-kill-ring) ; autoloaded fn


;;;----------------------------------------------------------------------------
;;; eglot
;;;----------------------------------------------------------------------------
(unless (fboundp #'eglot) ;; on emacs-29+ use built-in eglot
  (push "~/.emacs.d/notElpa/eglot" load-path)
  (autoload #'eglot "eglot" nil t)
  (autoload #'eglot-ensure "eglot" nil t))

(with-eval-after-load 'eglot
  ;; ;; Show all of the available eldoc information when we want it. This way Flymake errors
  ;; ;; don't just get clobbered by docstrings.
  ;; (add-hook 'eglot-managed-mode-hook
  ;;           (lambda ()
  ;;             "Make sure Eldoc will show us all of the feedback at point."
  ;;             (setq-local eldoc-documentation-strategy
  ;;                         #'eldoc-documentation-compose)))

  ;; is the event buffer needed for anything? disable until I find a use.
  (setq eglot-events-buffer-size 0)
  (setq eglot-autoshutdown t) ; kill eglot if all managed buffers are closed
  (setq eglot-ignored-server-capabilities
        '(
          ;; Instead I will access docs on demand via <C-h .> or <C-c C-d d>
          ;; TODO: eldoc doesn't work at all with this disabled? look into it.
          ;; :hoverProvider

          ;; Disable variable highlight as it seems overkill to constantly send
          ;; messages to a server for that.
          ;; TODO: use a tree-sitter imp for variable highlight.
          :documentHighlightProvider))

  (defun my-eglot-toggle-eldoc ()
    (interactive)
    (if (memq :hoverProvider eglot-ignored-server-capabilities)
        ;; remove
        (setq eglot-ignored-server-capabilities
              (delq :hoverProvider eglot-ignored-server-capabilities))
      ;; else add
      (push :hoverProvider eglot-ignored-server-capabilities))))


;;;----------------------------------------------------------------------------
;;; ht. dependency of lsp-mode.
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/ht.el" load-path)

;;;----------------------------------------------------------------------------
;;; lsp-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/lsp-mode" load-path)
(push "~/.emacs.d/notElpa/lsp-mode/clients" load-path)
(push "~/.emacs.d/notElpa/lsp-mode/test" load-path)
(autoload #'lsp "lsp-mode" nil t)

;; (with-eval-after-load 'lsp-pwsh ; powershell
;;   ;; powershell server
;;   ;; NOTE: first must manually download .zip folder from:
;;   ;; https://github.com/PowerShell/PowerShellEditorServices/releases/latest/
;;   ;; download/PowerShellEditorServices.zip
;;   ;; to:
;;   ;; ~/.emacs.d/.cache/lsp/pwsh
;;   )

;;;----------------------------------------------------------------------------
;;; company-lsp
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/company-lsp" load-path)

(with-eval-after-load 'lsp-mode
  (require 'company-lsp)
  (push 'company-lsp company-backends))

;;;----------------------------------------------------------------------------
;;; cquery. Not an elisp project. Built separately.
;;; NOTE: put compile_commands.json in each project root (or symlink).
;;;----------------------------------------------------------------------------
;; (when nil ;(eq my-curr-computer 'wild-dog)
;;   (push "/home/mike/proj/cquery/build/release/bin" exec-path))

;;;----------------------------------------------------------------------------
;;; cquery. Melpa elisp package. (works with cquery binary above.)
;;;----------------------------------------------------------------------------
;; (with-eval-after-load 'cquery
;;   (when (eq my-curr-computer 'wild-dog)
;;     (setq cquery-executable
;;           "/home/mike/proj/cquery/build/release/bin/cquery")))

;; (when nil ;(eq my-curr-computer 'wild-dog)
;;   (defun my-setup-cquery ()
;;     ;; autoload for `lsp-cquery-enable' is broken so just require the
;;     ;; `cquery' library to make it available.
;;     (require 'cquery)
;;     (lsp-cquery-enable))
;;   ;; turn on cquery automatically.  But might go wonky if
;;   ;; compile_commands.json is not in the project root.
;;   (add-hook 'c-mode-common-hook #'my-setup-cquery))


;;;----------------------------------------------------------------------------
;;; ccls. Not an elisp project. C++ proj build separetly.
;;; NOTE: put compile_commands.json or .ccls in each project root (or symlink).
;;;----------------------------------------------------------------------------
;; (defvar my-ccls-folder (cond ((eq my-curr-computer 'wild-dog)
;;                               "/home/mike/proj/ccls/Release/")
;;                              (t nil)))

;; (when (eq my-curr-computer 'wild-dog)
;;   ;; binary is /home/mike/proj/ccls/Release/ccls
;;   (push my-ccls-folder exec-path))

;;;----------------------------------------------------------------------------
;;; ccls. Melpa elisp package. (works with ccls binary above.)
;;;----------------------------------------------------------------------------
;; (with-eval-after-load 'ccls
;;   (when (eq my-curr-computer 'wild-dog)
;;     (setq ccls-executable (concat my-ccls-folder "ccls"))
;;     ;; ;; use flycheck
;;     ;; (setq lsp-prefer-flymake nil)
;;     ;; (setq-default flycheck-disabled-checkers
;;     ;;               '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;;     ))

;; (defun my-setup-ccls ()
;;   "Hook to run on c/c++ files to set up ccls."
;;   (require 'ccls)
;;   (lsp))
;; (when nil ; (eq my-curr-computer 'wild-dog)
;;   ;; turn on ccls automatically.  But might go wonky if compile_commands.json
;;   ;; or .ccls is not in the project root.
;;   (add-hook 'c-mode-common-hook #'my-setup-ccls))


;;;----------------------------------------------------------------------------
;;; Indium (formerly Jade). Javascript IDE.
;;;----------------------------------------------------------------------------
;; NOTE: must install server with the following command.
;;       npm install -g indium
;; NOTE: must install package dependency `websocket'. Currently getting it from
;;       melpa.
(push "~/.emacs.d/notElpa/Indium" load-path) ; git submodule
(autoload 'indium-connect "indium-interaction" nil t)
;; (with-eval-after-load 'indium
;;   )

;;;----------------------------------------------------------------------------
;;; ispell
;;;----------------------------------------------------------------------------
(with-eval-after-load 'ispell
  ;; NOTE: instructions to set up hunspell on windows:
  ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2014-04/msg00030.html
  (when (memq my-curr-computer '(work-laptop-2019))
    ;; (push "C:/Users/mtz/programs/hunspell-1.3.2-3-w32-bin/bin"
    ;;       exec-path)
    ;; (setq ispell-program-name (locate-file "hunspell"
    ;;                                        exec-path
    ;;                                        exec-suffixes
    ;;                                        'file-executable-p))
    (setq ispell-program-name
          "C:/Users/mtz/programs/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe")))


;;;----------------------------------------------------------------------------
;;; my-code-snippet-url
;;;----------------------------------------------------------------------------
(autoload #'my-code-snippet-url "my-code-snippet-url" nil t)

;;;----------------------------------------------------------------------------
;;; offline.el. optionally load some settings not stored in source control
;;;----------------------------------------------------------------------------
(when nil ;;(eq my-curr-computer 'wild-dog)
  (autoload #'my-erc-set-data "offline" nil t))


;;;----------------------------------------------------------------------------
;;; diff-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'diff-mode
  (when my-use-evil-p
    (define-key diff-mode-map (kbd "M-h") #'evil-window-left)
    ;; rebind the keys we just clobbered
    (define-key diff-mode-map (kbd "C-c h") #'describe-mode)))


;;;----------------------------------------------------------------------------
;;; my-ruler
;;;----------------------------------------------------------------------------
(autoload #'my-insert-ruler "my-ruler" nil t)
(autoload #'my-longest-line "my-ruler" nil t)

;;;----------------------------------------------------------------------------
;;; my-pdump
;;;----------------------------------------------------------------------------
(autoload #'my-load-everything-for-pdump "my-pdump" nil t)
(autoload #'my-make-pdump "my-pdump" nil t)


;;;----------------------------------------------------------------------------
;;; deadgrep
;;;----------------------------------------------------------------------------
(with-eval-after-load 'deadgrep
  (setq deadgrep-max-line-length 250))

;;;----------------------------------------------------------------------------
;;; wgrep. dependency of rg
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/Emacs-wgrep" load-path)
(autoload #'wgrep-setup "wgrep" nil nil)

;; allows wgrep to work in *grep* buffers.
(add-hook 'grep-setup-hook #'wgrep-setup)

;;;----------------------------------------------------------------------------
;;; rg
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/rg.el" load-path)
;; adding /test/ folder helps with byte compiler errors
(push "~/.emacs.d/notElpa/rg.el/test" load-path)
(autoload #'rg "rg" nil t)

(with-eval-after-load 'rg
  (require 'wgrep)
  (setq rg-command-line-flags
        `("-M 250" ; truncate long lines in display.
          ;; search dot files/folders but not .git
          ;; TODO: figure out how to get it to searh .classpath
          "--hidden"
          ,(if (eq system-type 'windows-nt)
               "-g \"!.git/\""
             "-g '!.git/'")))

  (setq rg-show-columns nil)
  (setq rg-group-result nil)
  (setq rg-align-position-numbers nil)
  (setq rg-ignore-case 'force) ; case insensitive always

  (when my-use-evil-p
    (define-key rg-mode-map (kbd "l") #'evil-forward-char)
    (define-key rg-mode-map (kbd "h") #'evil-backward-char)
    (define-key rg-mode-map (kbd "w") #'evil-forward-word-begin)
    (define-key rg-mode-map (kbd "s") my-swoop-fn)
    (define-key rg-mode-map (kbd "S") #'evil-change-whole-line)
    ;; rebind the keys we just clobbered
    (define-key rg-mode-map (kbd "C-c l") #'rg-list-searches)
    (define-key rg-mode-map (kbd "C-c h") #'describe-mode)
    (define-key rg-mode-map (kbd "C-c w") #'wgrep-change-to-wgrep-mode)
    (define-key rg-mode-map (kbd "C-c s") #'rg-save-search-as-name)
    (define-key rg-mode-map (kbd "C-c S") #'rg-save-search)))

(when my-use-evil-p
  (let ((search-fn (if (and my-has-rg-exe-p my-install-rg-p)
                       #'rg
                     ;; i use git to control the emacs config so git grep is
                     ;; always available.
                     #'my-grep-dwim)))
    (evil-leader/set-key "g" search-fn)))


;; INFO: alterantive using ripgrep with built-in emacs commands
;; (with-eval-after-load 'grep
;;   (grep-apply-setting
;;    'grep-find-command
;;    '("rg --no-heading --with-filename '' --glob='' " . 34)))


;;;----------------------------------------------------------------------------
;;; tramp
;;;----------------------------------------------------------------------------
;; https://old.reddit.com/r/emacs/comments/b9u049/remote_development_with_an_od
;; d_situation/ek6x43b/
;; TRAMP doesn't really introduce very much overhead on top of whatever
;; protocol you're using in my experience.
;; If TRAMP is slow for you, then using something like rsync, scp or ssh to
;; transfer files on save will also be slow.
;; You can configure the protocol that tramp uses:
;; (setq tramp-default-method "ssh")
;; You can configure ssh connection to keep the socket alive:
;; Host * ControlMaster auto ControlPath ~/.ssh/master-socket/%r@%h:%p ControlP
;; ersist 3s
;; There are other options too.

;;;----------------------------------------------------------------------------
;;; eros
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/eros" load-path)
(autoload #'eros-mode "eros" nil t)

(with-eval-after-load 'eros
  (setq eros-eval-result-prefix "")
  (setq eros-eval-result-duration 'command))

;;;----------------------------------------------------------------------------
;;; so-long
;;;----------------------------------------------------------------------------
(when (fboundp #'global-so-long-mode)
  (global-so-long-mode 1))

;;;----------------------------------------------------------------------------
;;; ctags. universal ctags or exuberant ctags.
;;;----------------------------------------------------------------------------
(defvar my-ctags-exe
  (cond ((eq my-curr-computer 'work-laptop-2019)
         "C:/Users/mtz/programs/ctags-2019-12-04_2ebf5b1b-x64/ctags.exe")
        ((eq my-curr-computer 'wild-dog)
         "~/proj/ctags/ctags" ; universal
         ;; "/usr/bin/ctags-exuberant"
         )
        ((eq my-curr-computer 'mac-mini-m1-2021) "/opt/homebrew/bin/ctags")
        ((eq my-curr-computer 'work-laptop-mac) "/opt/homebrew/bin/ctags")
        (t nil)))

(defvar my-universal-ctags-p (memq my-curr-computer '(work-laptop-2019
                                                      wild-dog
                                                      mac-mini-m1-2021
                                                      work-laptop-mac))
  "Non-nil if the ctags used on this computer is universial ctags.")

(cl-defun my-create-ctags (dir-name)
  "Create tags file using DIR-NAME as project root."
  (interactive "DDirectory: ")
  ;; GUARD: must have ctags program configured in `my-ctags-exe'.
  (when (null my-ctags-exe)
    (message "Set path to ctags in my-ctags-exe.")
    (cl-return-from my-create-ctags))

  (cond
   ;; select the lang if universal-ctags
   (my-universal-ctags-p
    (require 's) ; for `s-split'
    (let* ((lang-str (shell-command-to-string (format "%s --list-languages"
                                                      my-ctags-exe)))
           (supported-langs (s-split "\n" lang-str))
           (lang (completing-read "Lang: " supported-langs)))
      ;; create tags.
      (shell-command
       (format "%s --languages=%s -f TAGS -e -R %s"
               my-ctags-exe
               lang
               (directory-file-name dir-name)))))
   ;; not unviersal-ctags. explicit language selection not supported?
   (t
    ;; create tags
    (shell-command
     (format "%s -f TAGS -e -R %s"
             my-ctags-exe
             (directory-file-name dir-name)))))

  ;; `xref' stores TAGS file in global variable `tags-file-name'. Set it as
  ;; that's usually what I want.
  ;; NOTE: when switching to a new project, must manually set `tags-file-name'
  ;; or regenrate the TAGS file with this fn.
  (visit-tags-table (concat (directory-file-name dir-name)
                            "/TAGS"))
  ;; (setq tags-file-name (concat (directory-file-name dir-name)
  ;;                              "/TAGS"))
  )

;;;----------------------------------------------------------------------------
;;; xref. used in conjunction with ctags when jumping to definition.
;;;----------------------------------------------------------------------------
(with-eval-after-load 'xref
  (when my-use-evil-p
    ;; use emacs bindings (not evil)
    (push '("^*xref" . emacs) evil-buffer-regexps)))

;; jump to TAG definition in a new window
(global-set-key (kbd "M-.") #'xref-find-definitions)
;; (global-set-key (kbd "M-.") #'xref-find-definitions-other-window)


;;;----------------------------------------------------------------------------
;;; citre. ctags/readtags IDE
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/citre" load-path)
(autoload 'citre-mode "citre" nil t nil)

(with-eval-after-load 'citre
  (define-key citre-mode-map (kbd "C-c p") #'citre-peek)
  (setq citre-ctags-program my-ctags-exe)
  (setq citre-readtags-program
        (cond ((eq my-curr-computer 'mac-mini-m1-2021)
               "/opt/homebrew/bin/readtags"))))


;;;----------------------------------------------------------------------------
;;; libvterm
;;;----------------------------------------------------------------------------
(when (eq my-curr-computer 'wild-dog)
  (push "~/proj/emacs-libvterm/" load-path)
  ;; (require 'vterm)
  (autoload #'vterm "vterm" nil t))

;;;----------------------------------------------------------------------------
;;; mini-modeline
;;;----------------------------------------------------------------------------
(with-eval-after-load 'mini-modeline
  (setq mini-modeline-update-interval 0.2))

;;;----------------------------------------------------------------------------
;;; minesweeper
;;;----------------------------------------------------------------------------
(autoload #'minesweeper "minesweeper" nil t)
(with-eval-after-load 'minesweeper
  (setq *minesweeper-board-width* 30)
  (setq *minesweeper-board-height* 10)
  (setq *minesweeper-mines* 75))

;;;----------------------------------------------------------------------------
;;; esxml. dependency of nov
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/esxml" load-path)

;;;----------------------------------------------------------------------------
;;; nov
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/nov.el" load-path)
(autoload #'nov-mode "nov" nil t)

;; Make sure you have an unzip executable on PATH, otherwise the extraction of
;; EPUB files will fail. If you for some reason have unzip in a non-standard
;; location, customize `nov-unzip-program' to its path. You'll also need an
;; Emacs compiled with libxml2 support, otherwise rendering will fail.
(push '("\\.epub\\'" . nov-mode) auto-mode-alist)

(with-eval-after-load 'nov
  (setq nov-text-width 80)
  (setq nov-variable-pitch nil) ; use configured emacs font

  (when (eq my-curr-computer 'work-laptop-2019)
    (setq nov-unzip-program
          "C:/Users/mtz/programs/unzip-5.51-1-bin/bin/unzip.exe"))

  ;; TODO: this keybind isn't working. fix it.
  (define-key nov-mode-map (kbd "C-o") #'nov-goto-toc))


;;;----------------------------------------------------------------------------
;;; display-fill-column-indicator
;;;----------------------------------------------------------------------------
;; a new native built-in line display in Emacs.
;; It can replace package `fill-column-indicator' and it's `fci-mode'.
;; call `display-fill-column-indicator-mode' to toggle the new mode.

(with-eval-after-load 'display-fill-column-indicator
  (setq-default display-fill-column-indicator-column 79)
  ;; pipe | (ascii 124). Unicode alternative ?\u2502
  (setq-default display-fill-column-indicator-character ?\|))

(let ((cols '(nil 79 80 100 110))
      (curr nil))
  (defun my-cycle-col-line ()
    (interactive)
    ;; TODO: remove this require. move fn to it's own lib where the require
    ;; occurs during initial load of lib.
    (require 'display-fill-column-indicator)

    (setq curr (car (or (cdr (memq curr cols))
                        cols)))
    (cond
     ;; if mode on and cycled to nil col.
     ((and display-fill-column-indicator-mode
           (eq curr nil))
      (display-fill-column-indicator-mode 0))
     ;; if mode off and cycled to integer col.
     ((and (not display-fill-column-indicator-mode)
           (integerp curr))
      (display-fill-column-indicator-mode 1)))

    (setq display-fill-column-indicator-column curr)
    (message "display-fill-column-indicator-column: %s" curr)))

(declare-function my-cycle-col-line 'suppress)
(global-set-key (kbd "<f7>") #'my-cycle-col-line)

;; use it for lisps
(when (fboundp #'display-fill-column-indicator-mode)
  (add-hook 'emacs-lisp-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'lisp-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'scheme-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'clojure-mode-hook #'display-fill-column-indicator-mode))

;;;----------------------------------------------------------------------------
;;; slime-volleyball
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/slime-volleyball" load-path)
(autoload #'slime-volleyball "slime-volleyball" nil t)

;;;----------------------------------------------------------------------------
;;; autothemer
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/autothemer" load-path)
(autoload #'autothemer-deftheme "autothemer" nil nil)
(autoload #'autothemer-generate-templates "autothemer" nil t)

;;;----------------------------------------------------------------------------
;;; php-mode
;;;----------------------------------------------------------------------------
;; (push "~/.emacs.d/notElpa/php-mode" load-path)

;; looks like they moved the elisp files to a sub folder
(push "~/.emacs.d/notElpa/php-mode/lisp" load-path)
(autoload #'php-mode "autothemer" nil t)

;;;----------------------------------------------------------------------------
;;; yaml-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/yaml-mode" load-path)
(autoload 'yaml-mode "yaml-mode" nil t nil)

(push '("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode) auto-mode-alist)

;;;----------------------------------------------------------------------------
;;; sicp
;;;----------------------------------------------------------------------------
;; (push "~/.emacs.d/notElpa/sicp-info" load-path)

;;;----------------------------------------------------------------------------
;;; haskell-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/haskell-mode" load-path)

;;;----------------------------------------------------------------------------
;;; spinner
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/spinner.el" load-path)
(autoload #'spinner-create "spinner" nil nil)
(autoload #'spinner-start "spinner" nil nil)


;;;----------------------------------------------------------------------------
;;; electric-indent
;;;----------------------------------------------------------------------------
;; default emacs behavior got weird in Emacs 24.4. Turn it off.
;; see https://emacs.stackexchange.com/questions/5939/how-to-disable-auto-inden
;; tation-of-new-lines/5941

;; turn off electric-indent-mode globally.
;; It makes newlines in fundamental-mode and text-mode annoying.
(when (fboundp #'electric-indent-mode)
  (electric-indent-mode -1))
;; also avoid auto ident on evil "o" keybind
(when my-use-evil-p
  (setq-default evil-auto-indent nil))

;; But I usually want electric-indent in programming mode buffers.
;; Except for sql-mode which has bad indentation.
(with-eval-after-load 'prog-mode
  (defun my-setup-prog-mode-indent ()
    (electric-indent-local-mode 1)
    (setq evil-auto-indent t))
  (add-hook 'prog-mode-hook #'my-setup-prog-mode-indent))

;;;----------------------------------------------------------------------------
;;; treesit. Emacs 29+ built-in.
;;;----------------------------------------------------------------------------
(with-eval-after-load 'treesit
  (setq treesit-font-lock-level 4) ;; color more things

  ;; repo info for compiling language grammars via
  ;; `treesit-install-language-grammar'
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                      "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
               "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                      "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod"))))

;;;----------------------------------------------------------------------------
;;; tree-sitter, tree-sitter-langs. 3rd party package. works for pre-Emacs 29
;;;----------------------------------------------------------------------------
(with-eval-after-load 'tree-sitter
  (progn ;; handle issue where tree-sitter goes crazy during yasnippet
         ;; expansion. Just turn it off temporarily.
    (defun my-turn-off-tree-sitter-hl ()
      (interactive)
      (tree-sitter-hl-mode -1)
      (tree-sitter-mode -1))
    (defun my-turn-on-tree-sitter-hl ()
      (interactive)
      (tree-sitter-mode 1)
      (tree-sitter-hl-mode 1))

    (defun my-add-yas-tree-sitter-hooks ()
      (interactive)
      (add-hook 'yas-before-expand-snippet-hook #'my-turn-off-tree-sitter-hl)
      (add-hook 'yas-after-exit-snippet-hook #'my-turn-on-tree-sitter-hl))
    ;; TODO: figure out an automatic way (hooks?) to remove these hooks. This
    ;; way requires manually invoking the fn below.
    (defun my-remove-yas-tree-sitter-hooks ()
      "Manually invoke this fn to remove tree-sitter hooks from yasnippet.
The hooks are only relevant in buffers using tree-sitter.
If the hooks are not removed, they will turn on tree-sitter during yasnippet
expansion in cases where you aren't even using tree-sitter."
      (interactive)
      (remove-hook 'yas-before-expand-snippet-hook
                   #'my-turn-off-tree-sitter-hl)
      (remove-hook 'yas-after-exit-snippet-hook
                   #'my-turn-on-tree-sitter-hl))))

(cl-defun my-start-tree-sitter-hl ()
  "Manually invoke this method in a buffer for syntax highlighting.
TODO: delete this fn and replace with hooks, etc."
  (interactive)
  ;; GUARD
  (unless my-dyn-modules-p
    (cl-return-from my-start-tree-sitter-hl))

  ;; init
  (progn
    (package-initialize) ; using melpa temporarily.
    (require 'tree-sitter)

    ;; ;; prevent downloads with dummy fn on windows. I manually extract the
    ;; ;; dll's with 7zip as gzip is not available.
    ;; (when (eq system-type 'windows-nt)
    ;;   (defun tree-sitter-langs-install-grammars
    ;;       (&optional skip-if-installed version os keep-bundle)
    ;;     (print skip-if-installed)
    ;;     (print version)
    ;;     (print os)
    ;;     (print keep-bundle))
    ;;   (defun tree-sitter-langs-compile (lang-symbol &optional clean)
    ;;     (print lang-symbol)
    ;;     (print clean)))
    (require 'tree-sitter-langs)

    ;; add hooks to disable tree-sitter during yasnippet expansion. Yasnippet
    ;; borks tree-sitter.
    (my-add-yas-tree-sitter-hooks))

  ;; enable modes
  (tree-sitter-mode 1)
  (tree-sitter-hl-mode 1))

;;;----------------------------------------------------------------------------
;;; cookie1. replacement for yow? also see 'fortune
;;;----------------------------------------------------------------------------
(with-eval-after-load 'cookie1
  (when (eq my-curr-computer 'wild-dog)
    ;; http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/etc/yow.lines?revisi
    ;; on=1.10&amp;root=emacs
    (setq cookie-file "~/Downloads/yow.lines")))

;;;----------------------------------------------------------------------------
;;; rust-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/rust-mode" load-path)
(push '("\\.rs\\'" . rust-mode) auto-mode-alist) ; use rust-mode for .rs files
(push '("\\.lock\\'" . conf-toml-mode) auto-mode-alist)
(autoload #'rust-mode "rust-mode" nil t)
(autoload #'rust-dbg-wrap-or-unwrap "rust-mode" nil t)

;; silence byte compiler warning
(declare-function my-setup-rust-mode 'suppress)

(with-eval-after-load 'rust-mode
  ;; get `rust-format-buffer' to work on mac. it uses `executable-find' to
  ;; look for rstfmt. Which expects the path to be in elisp var `exec-path'.
  (when (eq my-curr-computer 'mac-mini-m1-2021)
    (add-to-list 'exec-path "~/.cargo/bin"))

  ;; keybinds
  (define-key rust-mode-map (kbd "C-c C-c") #'compile)
  (define-key rust-mode-map (kbd "C-c c") #'compile)

  ;; hook
  (defun my-setup-rust-mode ()
    (setq compile-command "cargo check")
    (yas-minor-mode 1)
    (my-turn-on-electric-pair-local-mode)
    (rainbow-delimiters-mode-enable)
    ;; set to 1 so comments on the same line are kept close to the code.
    (setq comment-column 1) ; buffer local
    (when (fboundp #'display-fill-column-indicator-mode)
      (setq display-fill-column-indicator-column 79) ; buffer local
      (display-fill-column-indicator-mode 1)))
  (add-hook 'rust-mode-hook #'my-setup-rust-mode))

;;;----------------------------------------------------------------------------
;;; racer
;;;----------------------------------------------------------------------------
;; having issues with racer. Dont' use for now.
;; (push "~/.emacs.d/notElpa/emacs-racer" load-path)
;; (autoload #'racer-find-definition "racer" nil t)
;; (autoload #'racer-find-definition-other-window "racer" nil t)
;; (autoload #'racer-find-definition-other-frame "racer" nil t)
;; (autoload #'racer-mode "racer" nil t)

;; (with-eval-after-load 'rust-mode
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode) ; show fn sig in minibuffer
;;   )

;;;----------------------------------------------------------------------------
;;; my-rust-stuff. extra's, like opening local docs, etc.
;;;----------------------------------------------------------------------------
(autoload #'my-rust-open-top-level-docs "my-rust-stuff" nil t)
(autoload #'my-rust-open-the-book "my-rust-stuff" nil t)

;;;----------------------------------------------------------------------------
;;; compile.  built-in mode in emacs.
;;;----------------------------------------------------------------------------
(with-eval-after-load 'compile
  ;; swiper keybind in compilation buffer
  (define-key compilation-mode-map (kbd "s") my-swoop-fn)
  ;; TODO: figure out why `evil-define-key' is not working.
  ;; (when my-use-evil-p
  ;;   (evil-define-key 'normal compilation-mode-map
  ;;     (kbd "s") my-swoop-fn))
  )

(defun my-compile-at-root-dir ()
  "Shadow `default-directory' with project root.  Then call `compile'.
This is needed if you are in a sub-dir of the project and want to invoke make
via M-x compile."
  (interactive)
  (require 'project)
  (let* ((ask-for-root? t) ;; if project root cannot be auto detected, get it
                           ;; via user input.
         (default-directory (project-root (project-current ask-for-root?))))
    (call-interactively #'compile)))

;;;----------------------------------------------------------------------------
;;; text-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'text-mode

  ;; from chen bin on https://emacs.stackexchange.com/questions/18304/optimal-s
  ;; ettings-for-auto-complete-for-writing-papers-prose-in-natural-languag/1852
  ;; 2#18522
  ;; TODO: look into predictive-mode.
  (defun my-setup-prose-completion ()
    (interactive)
    ;; make `company-backends' local is critcal or else, you will have completion
    ;; in every major mode, that's very annoying!
    (make-local-variable 'company-backends)
    ;; company-ispell is the plugin to complete words
    (add-to-list 'company-backends 'company-ispell)


    (make-local-variable 'company-idle-delay)
    (setq company-idle-delay 0.5)
    (make-local-variable 'company-minimum-prefix-length)
    (setq company-minimum-prefix-length 3)

    ;; (setq company-ispell-dictionary
    ;;       (file-truename "~/.emacs.d/misc/english-words.txt"))
    )

  ;; (add-hook 'text-mode-hook #'my-setup-prose-completion)
  )

;;;----------------------------------------------------------------------------
;;; dank-mode. reddit browser
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/dank-mode/lisp" load-path)
(autoload #'dank-mode "dank-mode" nil t)

;;;----------------------------------------------------------------------------
;;; .NET dotnet core stuff
;;;----------------------------------------------------------------------------

;; make sure emacs can find the dotnet command. from eshell, etc.
(cond ((eq system-type 'darwin)
       (push "/usr/local/share/dotnet" exec-path)
       (push "/usr/local/bin" exec-path)))

;;;----------------------------------------------------------------------------
;;; csv stuff
;;;----------------------------------------------------------------------------
(autoload #'csv-highlight "csv-stuff" nil t)

;;;----------------------------------------------------------------------------
;;; ruby-ts-mode
;;;----------------------------------------------------------------------------
(when (treesit-language-available-p 'ruby)
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode)))

;; NOTE: ruby-ts-mode loads ruby-mode so any variables there will be available.
(with-eval-after-load 'ruby-ts-mode
  ;; keybinds
  (define-key ruby-ts-mode-map (kbd "C-c C-c") #'compile)
  (define-key ruby-ts-mode-map (kbd "C-c c") #'compile)

  (defun my-setup-ruby-ts-mode ()
    ;; set to 1 so comments on the same line are kept close to the code.
    (setq comment-column 1) ; buffer local
    (yas-minor-mode 1)
    (rainbow-delimiters-mode 1)
    (my-turn-on-electric-pair-local-mode))
  (add-hook 'ruby-ts-mode-hook #'my-setup-ruby-ts-mode))


;;;----------------------------------------------------------------------------
;;; ruby-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'ruby-mode
  ;; keybinds
  (define-key ruby-mode-map (kbd "C-c C-c") #'compile)
  (define-key ruby-mode-map (kbd "C-c c") #'compile)

  (defun my-setup-ruby-mode ()
    ;; set to 1 so comments on the same line are kept close to the code.
    (setq comment-column 1) ; buffer local
    (yas-minor-mode 1)
    (rainbow-delimiters-mode 1)
    (my-turn-on-electric-pair-local-mode))
  (add-hook 'ruby-mode-hook #'my-setup-ruby-mode))


;;;----------------------------------------------------------------------------
;;; puni. Structural editing similar to paredit but for any language.
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/puni" load-path)
(autoload #'puni-mode "puni" nil t)

;; use `puni-mode' for the following modes (hooks).
;; NOTE: temporarily don't use puni. It messes up C-M-a, C-M-e.
;; TODO: rebind C-M-a and C-M-e keys to default for puni.
(when nil
  (dolist (hook '(web-mode-hook
                  ;; NOTE: puni doesn't work in html-mode
                  nxml-mode-hook
                  csharp-mode-hook
                  c-mode-hook
                  c++-mode-hook
                  java-mode-hook
                  sql-mode-hook
                  js-mode-hook
                  js2-mode-hook
                  ruby-mode-hook))
    (add-hook hook #'puni-mode)))

(with-eval-after-load 'puni
  ;; mimic my paredit key binds
  (define-key puni-mode-map (kbd "C-9") #'puni-barf-forward)
  (define-key puni-mode-map (kbd "C-0") #'puni-slurp-forward)
  (define-key puni-mode-map (kbd "C-M-9") #'puni-slurp-backward)
  (define-key puni-mode-map (kbd "C-M-0") #'puni-barf-backward)
  (define-key puni-mode-map (kbd "C-M-r") #'puni-raise)
  (define-key puni-mode-map (kbd "M-S") #'puni-split)
  ;; NOTE: there is no join fn to match `puni-split'
  (define-key puni-mode-map (kbd "M-s") #'puni-splice)
  ;; (define-key puni-mode-map (kbd "M-t") #'puni-transpose)
  )

;;;----------------------------------------------------------------------------
;;; posframe
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/posframe" load-path)
(autoload #'posframe-workable-p "posframe" nil nil)
(autoload #'posframe-show "posframe" nil nil)
(autoload #'posframe-hide-all "posframe" nil t)
(autoload #'posframe-delete-all "posframe" nil t)
(autoload #'posframe-benchmark "posframe" nil t)

;;;----------------------------------------------------------------------------
;;; ivy-posframe
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/ivy-posframe" load-path)
(autoload #'ivy-posframe-mode "ivy-posframe" nil t)

(with-eval-after-load 'ivy-posframe
  ;; (setq ivy-posframe-height-alist '((swiper-isearch . 20)
  ;;                                   (t . 40)))

  ;; (setq ivy-posframe-parameters
  ;;     '((left-fringe . 8)
  ;;       (right-fringe . 8)))


  (setq ivy-posframe-display-functions-alist
        '((swiper-isearch . ivy-display-function-fallback)
          (complete-symbol . ivy-posframe-display-at-point)
          (counsel-M-x . ivy-posframe-display-at-point
                       ;;ivy-posframe-display-at-window-bottom-left
                       )
          (t . ivy-posframe-display))))

(when nil ;; Don't turn on automatically. Want to expiriment first.
  (ivy-posframe-mode 1))


;;;----------------------------------------------------------------------------
;;; visual-indentation-mode. alternative to highlight-indent-guides
;;;----------------------------------------------------------------------------
(autoload #'visual-indentation-mode "visual-indentation-mode" nil t)


;;;----------------------------------------------------------------------------
;;; fennel-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/fennel-mode" load-path)
(autoload #'fennel-repl-mode "fennel-mode" nil t)
(autoload #'fennel-mode "fennel-mode" nil t)
(autoload #'fennel-repl "fennel-mode" nil t)
(autoload #'fennel-scratch "fennel-scratch" nil t)

(push '("\\.fnl\\'" . fennel-mode) auto-mode-alist)

(with-eval-after-load 'fennel-mode
  ;; (when (eq my-curr-computer 'mac-mini-m1-2021)
  ;;   (setq lua-default-application "/opt/homebrew/bin/lua")
  ;;   ;; (setq inferior-lisp-program "/opt/homebrew/bin/fennel --repl")
  ;;   (setq fennel-program "/opt/homebrew/bin/fennel --repl"))

  (defun my-setup-fennel-mode ()
    (when (fboundp #'display-fill-column-indicator-mode)
      (setq display-fill-column-indicator-column 79) ; buffer local
      (display-fill-column-indicator-mode 1))

    (rainbow-delimiters-mode-enable)
    (enable-paredit-mode)
    (when my-use-lispy-p
      (lispy-mode 1)))
  (add-hook 'fennel-mode-hook #'my-setup-fennel-mode)
  (add-hook 'fennel-repl-mode-hook #'my-setup-fennel-mode))

;;;----------------------------------------------------------------------------
;;; exec-path-from-shell
;;;----------------------------------------------------------------------------
;; this gets environment variables to work on mac. So things like fennel-repl
;; will work when it tries to find the "lua" program.
(autoload #'exec-path-from-shell-copy-envs "exec-path-from-shell" nil nil)
(autoload #'exec-path-from-shell-copy-env "exec-path-from-shell" nil t)
(autoload #'exec-path-from-shell-initialize "exec-path-from-shell" nil t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;;;----------------------------------------------------------------------------
;;; comint.  emacs built-in repl mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'comint
  ;; Don't echo passwords when dealing with interactive programs
  (add-hook 'comint-output-filter-functions
            #'comint-watch-for-password-prompt)

  (defun my-setup-comint ()
    (rainbow-delimiters-mode-enable)
    ;; TODO: detect if this comint is for a lisp lang and use `paredit' instead
    ;; of `electric-pair-mode'.
    (my-turn-on-electric-pair-local-mode))
  (add-hook 'comint-mode-hook #'my-setup-comint))

;;;----------------------------------------------------------------------------
;;; key binds for terminals. Where the meta key doesn't work. (ie M-x)
;;;----------------------------------------------------------------------------
(global-set-key (kbd "C-c x") #'execute-extended-command)


;;;----------------------------------------------------------------------------
;;; consult
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/consult" load-path)
(autoload #'consult-compile-error "consult-compile" nil t)
(autoload #'consult-flymake "consult-flymake" nil t)
(autoload #'consult-imenu "consult-imenu" nil t)
(autoload #'consult-imenu-multi "consult-imenu" nil t)
(autoload #'consult-info "consult-info" nil t)
(autoload #'consult-kmacro "consult-kmacro" nil t)
(autoload #'consult-org-heading "consult-org" nil t)
(autoload #'consult-org-agenda "consult-org" nil t)
(autoload #'consult-register-window "consult-register" nil nil)
(autoload #'consult-register-format "consult-register" nil nil)
(autoload #'consult-register "consult-register" nil t)
(autoload #'consult-register-load "consult-register" nil t)
(autoload #'consult-register-store "consult-register" nil t)
(autoload #'consult-xref "consult-xref" nil nil)

(autoload #'consult-buffer "consult" nil t)
(autoload #'consult-theme "consult" nil t)
(autoload #'consult-mark "consult" nil t)
(autoload #'consult-global-mark "consult" nil t)
(autoload #'consult-goto-line "consult" nil t)
(autoload #'consult-recent-file "consult" nil t)
(autoload #'consult-minor-mode-menu "consult" nil t)

;; temporarily defining this macro so consult.el will byte compile.
;; TODO: properly include this dependency. it's not in 'cl-macs in emacs 28.2
(defmacro cl-with-gensyms (names &rest body)
  "Bind each of NAMES to an uninterned symbol and evaluate BODY."
  (declare (debug (sexp body)) (indent 1))
  `(let ,(cl-loop for name in names collect
                  `(,name (gensym (symbol-name ',name))))
     ,@body))

;;;----------------------------------------------------------------------------
;;; vertico
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/vertico" load-path)
(push "~/.emacs.d/notElpa/vertico/extensions" load-path)
(autoload #'vertico-mode "vertico" nil t)
(autoload #'vertico-grid-mode "vertico-grid" nil t)

(with-eval-after-load 'vertico
  ;; insert a hyphen - on space like in vanilla M-x
  (define-key vertico-map (kbd "<SPC>") (lambda ()
                                          (interactive)
                                          (insert ?-)))

  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)

  ;; (when my-use-evil-p
  ;;   (evil-leader/set-key "b" #'consult-buffer))

  ;; set max window height for vertico buffer.
  (setq vertico-count 40))

(with-eval-after-load 'vertico-grid
  (setq vertico-grid-max-columns 12)
  ;; allows annoations next to the item to work in grid mode
  (setq vertico-grid-annotate 20)

  ;; key binds
  (define-key vertico-grid-map (kbd "C-h") #'vertico-grid-left)
  (define-key vertico-grid-map (kbd "C-l") #'vertico-grid-right)
  (define-key vertico-grid-map (kbd "C-v") #'vertico-grid-scroll-up)
  (define-key vertico-grid-map (kbd "M-v") #'vertico-grid-scroll-down))



(when (eq my-narrow-type 'vertico)
  (vertico-mode 1)
  (vertico-grid-mode 1)
  (when my-use-evil-p
    (evil-leader/set-key "b" #'switch-to-buffer)))


;;;----------------------------------------------------------------------------
;;; zone-sl
;;;----------------------------------------------------------------------------
(autoload #'zone-sl "zone-sl" nil t)
(autoload #'zone-pgm-sl "zone-sl" nil nil)

(defvar zone-programs)
(with-eval-after-load 'zone
  ;; since zone-programs is a fixed length array, adding an item to it is a bit
  ;; complex.
  (let* ((len (length zone-programs))
         (arr (make-vector (1+ len) nil)))
    ;; copy to new array with 1 empty slot at the end.
    (cl-loop for i from 0 to (1- len)
             do
             (aset arr i (aref zone-programs i))
             (print i))
    ;; add zone-pgm-sl to the last index
    (aset arr len #'zone-pgm-sl)
    (setq zone-programs arr)))

;;;----------------------------------------------------------------------------
;;; lolcat
;;;----------------------------------------------------------------------------
(autoload #'lolcat-this-buffer "lolcat" nil t)
(autoload #'lolcat-view-file "lolcat" nil t)
(autoload #'lolcat-view-buffer "lolcat" nil t)
(autoload #'lolcat-message "lolcat" nil t)
(autoload #'eshell/lolcat "lolcat" nil t)

;;;----------------------------------------------------------------------------
;;; zone-nyan
;;;----------------------------------------------------------------------------
(autoload #'zone-nyan-preview "zone-nyan" nil t)
(autoload #'zone-nyan "zone-nyan" nil)

;;;----------------------------------------------------------------------------
;;; zone-rainbow
;;;----------------------------------------------------------------------------
(autoload #'zone-rainbow "zone-rainbow" nil t)
(autoload #'zone-pgm-rainbow "zone-rainbow" nil nil)

;;;----------------------------------------------------------------------------
;;; white-christmas
;;;----------------------------------------------------------------------------
(autoload #'white-christmas "white-christmas" nil t)

;;;----------------------------------------------------------------------------
;;; jsonian. performance oriented alternative to json-mode
;;;----------------------------------------------------------------------------
(autoload #'jsonian-mode "jsonian" nil t)

(with-eval-after-load 'jsonian
  (setq jsonian-indentation my-indent-width)

  (defun my-setup-jsonian ()
    (setq tab-width jsonian-indentation)
    (rainbow-delimiters-mode 1)
    (my-turn-on-electric-pair-local-mode))

  (add-hook 'jsonian-mode-hook #'my-setup-jsonian))


;;;----------------------------------------------------------------------------
;;; repeat
;;;----------------------------------------------------------------------------
(with-eval-after-load 'repeat
  (setq repeat-exit-timeout nil) ;; don't timeout

  ;; redefine `repeat-mode' to avoid the startup message. I find it annoying.
  ;; TODO: contribute a patch upstream with a config var?
  ;; (define-minor-mode repeat-mode
;;     "Toggle Repeat mode.
;; When Repeat mode is enabled, and the command symbol has the property named
;; `repeat-map', this map is activated temporarily for the next command.
;; See `describe-repeat-maps' for a list of all repeatable commands."
;;     :global t :group 'convenience
;;     (if (not repeat-mode)
;;         (remove-hook 'post-command-hook 'repeat-post-hook)
;;       (add-hook 'post-command-hook 'repeat-post-hook)
;;       (let* ((keymaps nil)
;;              (commands (all-completions
;;                         "" obarray (lambda (s)
;;                                      (and (commandp s)
;;                                           (get s 'repeat-map)
;;                                           (push (get s 'repeat-map) keymaps))))))
;;         ;; (message "Repeat mode is enabled for %d commands and %d keymaps; see `describe-repeat-maps'."
;;         ;;          (length commands)
;;         ;;          (length (delete-dups keymaps)))
;;         )))
  )

;; For now don't turn on repeat mode.
;; (when (fboundp #'repeat-mode) ;; emacs 28+
;;   (repeat-mode))


;;;----------------------------------------------------------------------------
;;; my-proj-c-intro-and-ref
;;;----------------------------------------------------------------------------
(autoload #'cir-open-book "my-c-intro-and-ref" nil t)
;; `my-proj-c-intro-and-ref' is an alias for `cir-open-book'
(autoload #'my-proj-c-intro-and-ref "my-c-intro-and-ref" nil t)


;;;----------------------------------------------------------------------------
;;; zig-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/zig-mode" load-path)
(autoload 'zig-mode "zig-mode" nil t)
(push '("\\.zig\\'" . zig-mode) auto-mode-alist)

(with-eval-after-load 'zig-mode
  ;; 4 is the blessed indent width used by "zig fmt".
  ;; `zig-indent-offset' is already 4 by default but set it explictily just
  ;; to document the fact it needs to be 4 to conform with "zig fmt".
  (setq zig-indent-offset 4)
  ;; but when temporarily changing to tabs, i might change `zig-indent-offset'
  ;; to a non-4 value matching `tab-width'. I'll need to roll it back to 4
  ;; when spaces are enabled again. I do this with hook
  ;; `my-zig-untabify-buffer-hook' after saving the file.

  (defun my-zig-tabify-buffer ()
    "Convert space indentation to tabs. And turn on smart tabs mode.
Meant to be a temporary state while you are editing a buffer. As zig only
allows spaces. But this way you can temporarily view things with tabs, then
have the autoformater revert things back to spaces when you save."
    (interactive)
    (indent-tabs-mode 1)
    (progn ;; smart-tabs-mode
      (smart-tabs-advice zig-mode-indent-line zig-indent-offset) ;; 4
      (smart-tabs-mode-enable))
    (tabify (point-min) (point-max))
    ;; assumes smart-tabs-mode is configured for current mode. If not this may
    ;; inject tabs to handle alignments *after* the indentation level is reached
    ;; which would be "wrong".
    (indent-region (point-min) (point-max))
    ;; finally, view things in my prefered width
    (let ((width 3))
      (setq zig-indent-offset width
            tab-width width)))

  (defun my-zig-untabify-buffer-hook ()
    "Hook fn to run after saving file.
Acutally doesn't untabify the buffer. zig fmt does that on before-save-hook.
But restores `zig-indent-offset' and `tab-width' to 4.
And turns off `indent-tabs-mode'."
    ;; GUARD: in theory this is only needed when I have been viewing the zig
    ;; buffer with tabs.
    (when indent-tabs-mode
      ;; may have been changed to match `tab-width' earlier.
      (setq zig-indent-offset 4)
      (setq tab-width 4) ;; tab-width and ident offsets need stay in sync.
      (indent-tabs-mode -1)))

  ;; key binds
  (define-key zig-mode-map (kbd "C-c t") #'my-zig-tabify-buffer)
  (define-key zig-mode-map (kbd "C-c C-c") #'compile)
  (define-key zig-mode-map (kbd "C-c c") #'compile)

  (define-key zig-mode-map (kbd "C-c C-d d") #'eldoc-print-current-symbol-info)
  (define-key zig-mode-map (kbd "C-c C-d C-d") #'eldoc-print-current-symbol-info)

  (defvar my-zls-installed-p (executable-find "zls"))

  ;; hook
  (defun my-setup-zig-mode ()
    ;; M-x compile
    (when buffer-file-name ;; if buffer has a file on disk.
      ;; wireup M-x compile.
      (set (make-local-variable 'compile-command)
           "zig build run"))

    ;; At the moment spaces are the blessed way to indent via "zig fmt".
    (indent-tabs-mode -1) ;; turn off tab indent
    (yas-minor-mode 1)
    (my-turn-on-electric-pair-local-mode)
    (rainbow-delimiters-mode-enable)
    ;; set to 1 so comments on the same line are kept close to the code.
    (setq comment-column 1) ; buffer local
    (when (fboundp #'display-fill-column-indicator-mode)
      (setq display-fill-column-indicator-column 79) ; buffer local
      (display-fill-column-indicator-mode 1))
    ;; NOTE: hook is "after" saving. zig fmt will have run by the time this
    ;; hook executes. If I was temporarily viewing the zig buffer with tabs,
    ;; this hook will restore some settings to be space friendly again.
    (add-hook 'after-save-hook #'my-zig-untabify-buffer-hook 0 'local)

    ;; eglot. keep this at the end to reduce chance of weird timer issues?
    (when (and buffer-file-name
               my-zls-installed-p)
      (eglot-ensure)
      (when (eq system-type 'darwin)
        ;; this timer solution doesn't work on windows but does on mac
        (run-with-timer 0.25 nil (lambda () (eldoc-mode -1))))))

  (add-hook 'zig-mode-hook #'my-setup-zig-mode))

;;;----------------------------------------------------------------------------
;;; vc-fossil
;;;----------------------------------------------------------------------------
;; ~/.emacs.d/notElpa already on load-path
(push 'Fossil vc-handled-backends)
(autoload #'vc-fossil-registered "vc-fossil" nil nil)

;;;----------------------------------------------------------------------------
;;; my-go-doc. custom doc lookup functions
;;;----------------------------------------------------------------------------
(autoload #'my-go-doc-local "my-go-doc" nil t)
(autoload #'my-go-doc-website "my-go-doc" nil t)
(autoload #'my-go-doc-website-overview "my-go-doc" nil t)

(with-eval-after-load 'my-go-doc
  (setq my-go-doc-assume-pkg-correct-p t))

;;;----------------------------------------------------------------------------
;;; my-go-helpers. helper fns for go
;;;----------------------------------------------------------------------------
;; fns not quite big enough to justify their own dedicated package.
(autoload #'my-go-errcheck "my-go-helpers" nil t)
(autoload #'my-go-ineffassign "my-go-helpers" nil t)
(autoload #'my-go-heap-escape "my-go-helpers" nil t)
(autoload #'my-go-lint "my-go-helpers" nil t)
(autoload #'my-go-lint-and-fix "my-go-helpers" nil t)
(autoload #'my-go-insert-type "my-go-helpers" nil t)
(autoload #'my-go-install-lib "my-go-helpers" nil t)
(autoload #'my-go-commands-hydra/body "my-go-helpers" nil t)

;;;----------------------------------------------------------------------------
;;; go-mod-ts-mode
;;;----------------------------------------------------------------------------
(when (treesit-language-available-p 'gomod)
  (add-to-list 'major-mode-remap-alist '(go-dot-mod-mode . go-mod-ts-mode)))

;;;----------------------------------------------------------------------------
;;; go-ts-mode
;;;----------------------------------------------------------------------------
;; prefer go-ts-mode for ".go" files. But only if it's available.
(when (treesit-language-available-p 'go)
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))

(with-eval-after-load 'go-ts-mode
  ;; key binds
  (define-key go-ts-mode-map (kbd "C-c C-c") #'compile)
  (define-key go-ts-mode-map (kbd "C-c c") #'compile)

  (define-key go-ts-mode-map (kbd "C-c C-d d") #'eldoc-print-current-symbol-info)
  (define-key go-ts-mode-map (kbd "C-c C-d C-d") #'eldoc-print-current-symbol-info)

  (define-key go-ts-mode-map (kbd "C-c g") #'my-go-commands-hydra/body)
  (define-key go-ts-mode-map (kbd "C-c C-g") #'my-go-commands-hydra/body)

  (defvar my-gofmt-installed-p (executable-find "gofmt"))

  (defun my-setup-go-ts-mode ()
    ;; (when buffer-file-name ;; if buffer has a file on disk.
    ;;   ;; wireup M-x compile. TODO: revisit this
    ;;   (set (make-local-variable 'compile-command)
    ;;        (concat "go run " (shell-quote-argument buffer-file-name))))

    ;; Run gofmt on save. Use "local" buffer hook to avoid polluting the
    ;; save-hook for non-go files.
    (when my-gofmt-installed-p
      (require 'go-mode) ;; for `gofmt'. Not using `gofmt-before-save' becuase
                         ;; it checks for go-mode but we are in go-ts-mode
      (add-hook 'before-save-hook #'gofmt 0 'local))
    ;; set to 1 so comments on the same line are kept close to the code
    (setq comment-column 1) ;; buffer local

    (setq tab-width 3)      ;; buffer local
    (setq go-ts-mode-indent-offset 3)
    (indent-tabs-mode 1)

    (yas-minor-mode 1)
    (when buffer-file-name ;; eglot gets weird if the buffer is not visiting a file.
      (eglot-ensure)
      ;; turn off mode to avoid spam. will call eldoc via keybind instead.
      ;; I'd prefer not to use a timer, but eglot doens't turn on eldoc
      ;; immediately so I need to give it time. TODO: figure out how to prevent
      ;; eglot from turning on eldoc-mode in the first place.
      (when (eq system-type 'darwin)
        ;; this timer solution doesn't work on windows but does on mac
        (run-with-timer 0.25 nil (lambda () (eldoc-mode -1))))

      ;; (when eldoc-mode
      ;;   (eldoc-mode -1))
      )
    ;; (citre-mode 1)
    (my-turn-on-electric-pair-local-mode)
    (rainbow-delimiters-mode))
  (add-hook 'go-ts-mode-hook #'my-setup-go-ts-mode))

;;;----------------------------------------------------------------------------
;;; go-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/go-mode.el" load-path)
(autoload #'go-mode "go-mode" nil t)
(autoload #'go-dot-mod-mode "go-mode" nil t)
(autoload #'go-dot-work-mode "go-mode" nil t)
(autoload #'gofmt-before-save "go-mode" nil t)
(push '("\\.go\\'" . go-mode) auto-mode-alist)
(push '("go\\.work\\'" . go-dot-work-mode) auto-mode-alist)
(push '("go\\.mod\\'" . go-dot-mod-mode) auto-mode-alist)


(declare-function my-setup-go-mode 'suppress) ;; silence byte compiler

(with-eval-after-load 'go-mode
  ;; key binds
  (define-key go-mode-map (kbd "C-c C-c") #'compile)
  (define-key go-mode-map (kbd "C-c c") #'compile)

  ;; unbind `godef-describe' so I can use "C-c C-d" as a prefix.
  (define-key go-mode-map (kbd "C-c C-d") nil)
  (define-key go-mode-map (kbd "C-c C-d d") #'eldoc-print-current-symbol-info)
  (define-key go-mode-map (kbd "C-c C-d C-d") #'eldoc-print-current-symbol-info)

  (define-key go-mode-map (kbd "C-c g") #'my-go-commands-hydra/body)
  (define-key go-mode-map (kbd "C-c C-g") #'my-go-commands-hydra/body)

  (defvar my-gofmt-installed-p (executable-find "gofmt"))

  (defun my-setup-go-mode ()
    ;; (when buffer-file-name ;; if buffer has a file on disk.
    ;;   ;; wireup M-x compile. TODO: revisit this
    ;;   (set (make-local-variable 'compile-command)
    ;;        (concat "go run " (shell-quote-argument buffer-file-name))))

    ;; Run gofmt on save. Use "local" buffer hook to avoid polluting the
    ;; save-hook for non-go files.
    (when my-gofmt-installed-p
      (add-hook 'before-save-hook #'gofmt-before-save 0 'local))
    ;; set to 1 so comments on the same line are kept close to the code
    (setq comment-column 1) ;; buffer local
    (setq tab-width 3) ;; buffer local
    (yas-minor-mode 1)
    (when buffer-file-name ;; eglot gets weird if the buffer is not visiting a file.
      (eglot-ensure)
      ;; turn off mode to avoid spam. will call eldoc via keybind instead.
      ;; I'd prefer not to use a timer, but eglot doens't turn on eldoc
      ;; immediately so I need to give it time. TODO: figure out how to prevent
      ;; eglot from turning on eldoc-mode in the first place.
      (when (eq system-type 'darwin)
        ;; this timer solution doesn't work on windows but does on mac
        (run-with-timer 0.25 nil (lambda () (eldoc-mode -1))))

      ;; (when eldoc-mode
      ;;   (eldoc-mode -1))
      )
    ;; (citre-mode 1)
    (my-turn-on-electric-pair-local-mode)
    (rainbow-delimiters-mode))
  (add-hook 'go-mode-hook #'my-setup-go-mode))


;;;----------------------------------------------------------------------------
;;; my-which-func
;;;----------------------------------------------------------------------------
;; My alternative to `which-function-mode'.
(autoload #'my-which-func "my-which-func" nil t)

(define-key prog-mode-map (kbd "C-c w") #'my-which-func)
(define-key prog-mode-map (kbd "C-c C-w") #'my-which-func)

(with-eval-after-load 'my-which-func
  (setq my-which-func-use-postip t))

;;;----------------------------------------------------------------------------
;;; reformatter
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/emacs-reformatter" load-path)
(autoload #'reformatter-define "reeformatter" nil nil)

;;;----------------------------------------------------------------------------
;;; browse-kill-ring
;;;----------------------------------------------------------------------------
(autoload #'browse-kill-ring "browse-kill-ring" nil t)
(autoload #'browse-kill-ring-default-keybindings "browse-kill-ring" nil t)

(when nil ;; don't use browse-kill-ring until i figure out some issues with
          ;; evil-mode

  ;; set up keybinds
  (browse-kill-ring-default-keybindings)

  ;; apply the same key bind advice to evil-paste-pop (kbd "M-y")
  (when my-use-evil-p
    (defadvice evil-paste-pop (around kill-ring-browse-maybe (arg))
      "If last action was not a yank, run `browse-kill-ring' instead."
      ;; evil-paste-pop has an (interactive "*p") form which does not allow
      ;; it to run in a read-only buffer.  We want browse-kill-ring to
      ;; be allowed to run in a read only buffer, so we change the
      ;; interactive form here.  In that case, we need to
      ;; barf-if-buffer-read-only if we're going to call evil-paste-pop with
      ;; ad-do-it
      (interactive "p")
      (if (not (eq last-command 'yank))
          (browse-kill-ring)
        (barf-if-buffer-read-only)
        ad-do-it))
    (ad-activate 'evil-paste-pop)))

;;;----------------------------------------------------------------------------
;;; perl-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'perl-mode
  (defun my-setup-perl-mode ()
    (my-turn-on-electric-pair-local-mode)
    (rainbow-delimiters-mode-enable))
  (add-hook 'perl-mode-hook #'my-setup-perl-mode))

;;;----------------------------------------------------------------------------
;;; combobulate
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/combobulate" load-path)
(autoload #'combobulate-mode "combobulate" nil t)

;;;----------------------------------------------------------------------------
;;; look into these packages later, TODO
;;;----------------------------------------------------------------------------
;; dwim-coder-mode, treesitter based mode that does stuff as you type
;; eldev. emacs build tool needed for a few packages like lsp-mode
;; undercover. test lib a few packages rely on. test build breaks without
;; emacs-gc-stats


;;;----------------------------------------------------------------------------
;;; buttercup. testing library used by several packages
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/emacs-buttercup" load-path)
(autoload #'buttercup-run-at-point "buttercup" nil t)
(autoload #'buttercup-run-discover "buttercup" nil nil)
(autoload #'buttercup-run-markdown-buffer "buttercup" nil t)
(autoload #'buttercup-run-markdown "buttercup" nil nil)
(autoload #'buttercup-run-markdown-file "buttercup" nil t)
(autoload #'buttercup-minor-mode "buttercup" nil t)


;;;----------------------------------------------------------------------------
;;; klondike
;;;----------------------------------------------------------------------------
(with-eval-after-load 'klondike
  ;; use emacs key binds
  (push '("^Klondike" . emacs) evil-buffer-regexps))



;;;----------------------------------------------------------------------------
;;; MISC options.
;;;----------------------------------------------------------------------------
;; blink effect on current line when switching windows or buffers.
(when nil
  (defun my-pulse-line-on-window-selection-change (frame)
    (when (eq frame (selected-frame))
      (pulse-momentary-highlight-one-line)))

  (add-hook 'window-selection-change-functions
            #'my-pulse-line-on-window-selection-change))

(defun my-unbreak-emacs-29 ()
  "Emacs 29, built 2022-07-18 has some features not working on windows.
Do things to unbreak it.
Hopefully this fn is only needed temporarily."
  (interactive)
  ;; this unbreaks the `rg' package
  (require 'files-x)

  ;; this unbreaks `list-packages' by loading `header-line-indent-mode'.
  (require 'display-line-numbers)
  ;; unbreaks package update
  (with-eval-after-load 'package
    (load "c:/Users/mtz/scratch/emacs/lisp/emacs-lisp/loaddefs-gen.el")))

(when (and (eq my-curr-computer 'work-laptop-2019)
           (= emacs-major-version 29))
  (my-unbreak-emacs-29))

;; INFO: elpa mirrors to get around situations where package manager cannot be
;; reached.
;; https://github.com/d12frosted/elpa-mirror


;; NOTE: at time of writing it seems `go-mode' does not have it's own indent
;; width var. Might be something to do with the standardization on tabs for
;; that langauge.
(defvar my-mode-indent-var-map
  '((c-mode c-basic-offset) (c++-mode c-basic-offset)
    (objc-mode c-basic-offset) (java-mode c-basic-offset)
    (idl-mode c-basic-offset) (pike-mode c-basic-offset)
    (awk-mode c-basic-offset) (csharp-mode c-basic-offset)

    (js2-mode js-indent-level) (js-mode js-indent-level)
    (js-ts-mode js-indent-level)

    (c-ts-mode c-ts-mode-indent-offset)
    (go-ts-mode go-ts-mode-indent-offset)
    (lua-mode lua-indent-level)
    (python-mode python-indent-offset)

    (ruby-mode ruby-indent-level) (ruby-ts-mode ruby-indent-level)

    (rust-mode rust-indent-offset)
    (zig-mode zig-indent-offset)
    (perl-mode perl-indent-level)))

(defun my-set-tab-width (width)
  "Set `tab-width' to WIDTH.
For most programming modes you need to change an additional variable as well.
Such as `c-basic-offset' for cc-mode. Or `lua-indent-level' for lua-mode.
This function will try to set the additional variable for a variety of
programming modes."
  (interactive "nindent width: ")

  ;; get "indent-width-var" for the current `major-mode'
  (let ((indent-var-sym (cadr (assoc major-mode my-mode-indent-var-map))))
    (set indent-var-sym width))
  (setq tab-width width))


(cl-defun my-tabify-buffer ()
  (interactive)

  ;; GUARD: don't allow this to run in a python-mode buffer
  (when (eq major-mode 'python-mode)
    (message "don't use this for python as indentation can't be inferred.")
    (cl-return-from my-tabify-buffer))

  (setq indent-tabs-mode t) ;; buffer local
  (tabify (point-min) (point-max))
  ;; assumes smart-tabs-mode is configured for current mode. If not this may
  ;; inject tabs to handle alignments *after* the indentation level is reached
  ;; which would be "wrong".
  (indent-region (point-min) (point-max)))

(cl-defun my-untabify-buffer ()
  (interactive)

  ;; GUARD: don't allow this to run in a python-mode buffer
  (when (eq major-mode 'python-mode)
    (message "don't use this for python as indentation can't be inferred.")
    (cl-return-from my-tabify-buffer))

  (setq indent-tabs-mode nil) ;; buffer local
  (untabify (point-min) (point-max))
  ;; this should be OK with or without smart-tabs-mode.
  (indent-region (point-min) (point-max)))


;; for emacs default completion style.
;; The advantage of vertical is it's easier to scan consecutive items with your
;; eyes. The disadvantage is not as many items of the same prefix can fit on
;; the screen as they are in 1 column consecutively. So more scrolling via tab
;; is needed.
(setq completions-format 'horizontal)

(defun my-paste-below ()
  "Paste to line below.  Preserve indentation of current line."
  (interactive)
  (move-end-of-line 1)
  (newline)
  (yank)
  (indent-region (mark) (point)))

;; new emacs 29 feature. show matching start of delimiter if off screen.
(setq show-paren-context-when-offscreen nil)


;; make fn `display-buffer' behave like it did in XEmacs, as JWZ expects.
;; see blog:
;; https://www.jwz.org/blog/2021/01/very-much-do-not-have-my-emacs-setup-just-h
;; ow-i-like-it/
;; https://old.reddit.com/r/emacs/comments/kr8mg2/jwz_very_much_do_not_have_my_
;; emacs_setup_just_how/
;; (when nil ; don't do this for now, it's switching buffers in multiple windows
;;           ; at once!
;;  (when (fboundp #'display-buffer-use-least-recent-window)
;;    (setq display-buffer-alist
;;          '((".*" display-buffer-use-least-recent-window)))))


(defun my-load-common ()
  "Load some commonly used features.  Where it's a gray area whether I should
load during init, or wait with autoloads."
  (interactive)
  (require 'company)
  (require 'expand-region)
  (require 'hydra)
  ;; (require 'my-hydras)
  (require 'swiper)
  (require 'ivy)
  (require 'counsel)
  (require 'lispy)
  (with-current-buffer (get-buffer-create "*scratch*")
    (unless (eq major-mode #'emacs-lisp-mode)
      (emacs-lisp-mode))))

(when (memq my-curr-computer '(mac-mini-m1-2021
                               wild-dog
                               work-laptop-2019
                               work-laptop-mac))
  (my-load-common))


(setq read-process-output-max 65536)

(when (eq system-type 'windows-nt)
  ;; performance tweak for weird fonts on windows.
  ;; see https://github.com/sabof/org-bullets/issues/11
  (setq inhibit-compacting-font-caches t))

(autoload #'my-inject-newlines "my-misc" nil t)
(autoload #'my-delete-brackets "my-misc" nil t)
(autoload #'my-list-holidays "my-misc" nil t)

(autoload #'my-find-file-omni "my-misc" nil t)
(when my-use-evil-p
  (evil-leader/set-key "h" #'my-find-file-omni))


(autoload #'my-indent-defun "my-misc" nil t)
(define-key prog-mode-map (kbd "C-c <tab>") #'my-indent-defun)


(when (or (> emacs-major-version 25)
          (and (= emacs-major-version 25)
               (>= emacs-minor-version 2)))
  ;; Disable the weird quote characters used in help buffers in emacs 25.
  ;; var `use-default-font-for-symbols' was introduced in emacs 25.2 to disable
  ;; the bad behavior introduced in emacs 25.
  (setq use-default-font-for-symbols nil))

(cond ((memq my-curr-computer '(work-laptop-2019))
       (setq find-function-C-source-directory
             "c:/users/mtz/scratch/emacs/src"))
      ((eq my-curr-computer 'mac-mini-m1-2021)
       (setq find-function-C-source-directory
             "~/proj/emacs/src")))

(autoload #'my-win-count "my-misc" nil t)


(autoload #'find-shell "my-misc" nil t)
(define-key global-map (kbd "C-c C-z") #'find-shell) ; mimic slime repl binding

;; (when (and nil   ;don't start server for now.
;;            ;;`server-start' doesn't seem to work on MS-windows?
;;            (eq system-type 'gnu/linux))
;;   (server-start))

;; prevents warnings where you must select encoding (like in `list-packages')
(prefer-coding-system 'utf-8)


(autoload #'what-face "my-misc" nil t)


;; (defmacro C-u (&rest args)
;;   "Make it easier to programmatically call a function with `C-u' prefix.
;; Gotten from #Emacs on freenode.
;; ARGS here to satisfy flycheck."
;;   (let ((prefix (list 4)))
;;     (while (cdr args)
;;       (cond
;;        ((eq (car args) 'C-u)
;;         (setf (car prefix) (* 4 (car prefix))))
;;        ((eq (car args) 'M-x)
;;         ;; ignore
;;         t)
;;        (t
;;         (error "Unknown arg %S" (car args))))
;;       (setq args (cdr args)))
;;     (unless (functionp (car args))
;;       (error "%S is not a function" (car args)))
;;     `(lambda ()
;;        (interactive)
;;        (let ((current-prefix-arg ',prefix))
;;          (call-interactively ',(car args))))))
;;(global-set-key (kbd "<f12>") (C-u M-x org-refile))


;;;----------------------------------------------------------------------------
;;; scrolling
;;;----------------------------------------------------------------------------
(setq fast-but-imprecise-scrolling t)
;; scroll like vim when moving 1 line off screen with j/k.
;; has some weird rules about re-centering, but >=101 is supposed to
;; not recenter. I had an issue with value 1 where if i held down
;; j to scroll, it would periodically recenter.
(setq scroll-conservatively 101)
;; maintain cursor location when scrolling
(setq scroll-preserve-screen-position nil)

;; scrolling performance increase?
;; see https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-moveme
;; nt-lag/28746
(setq auto-window-vscroll nil)






(progn ;;window navigation.
  (when my-use-evil-p
    (evil-leader/set-key "2" #'split-window-below)
    (evil-leader/set-key "3" #'split-window-right)
    (global-set-key (kbd "M-h") #'evil-window-left)
    (global-set-key (kbd "M-j") #'evil-window-down)
    (global-set-key (kbd "M-k") #'evil-window-up)
    (global-set-key (kbd "M-l") #'evil-window-right)))

;; cycle the buffers really fast. Not doing this anymore since these are error
;; handling shortcuts in some modes.
;; (global-set-key (kbd "M-n") #'next-buffer)
;; (global-set-key (kbd "M-p") #'previous-buffer)

(autoload #'my-cycle-spacing "my-misc" nil t)
(global-set-key (kbd "M-\\") #'my-cycle-spacing)


(cond
 ((eq my-curr-computer 'wild-dog)
  (setq browse-url-generic-program "firefox"
        browse-url-browser-function #'browse-url-generic))

 ((eq my-curr-computer 'work-laptop)
  (setq
   ;; browse-url-generic-program
   ;; "C:\\Program Files (x86)\\conkeror\\conkeror.exe"

   browse-url-generic-program
   "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"

   browse-url-browser-function #'browse-url-generic))

 ((eq my-curr-computer 'hp-tower-2009)
  (setq browse-url-generic-program "conkeror"
        browse-url-browser-function #'browse-url-generic))

 ((eq my-curr-computer 'a-laptop-faster)
  (setq browse-url-generic-program "conkeror"
        browse-url-browser-function #'browse-url-generic))

 ((or (eq my-curr-computer 'raspberry-pi)
      (eq my-curr-computer 'utilite))
  (setq browse-url-generic-program "surf"
        browse-url-browser-function #'browse-url-generic)))


;; (defun my-insert-img ()
;;   (interactive)
;;   (let ((i 0))
;;     (while (< i 10)
;;       (insert-image nyan-prompt-nyan-cat-image)
;;       (cl-incf i)))
;;   (insert "\n\n")
;;   (let ((i 0))
;;     (while (< i 10)
;;       (insert-image nyan-prompt-nyan-cat-image)
;;       (cl-incf i))))


;;transparent
;;(set-frame-parameter (selected-frame) 'alpha '(100 93))

;; (let* ((alpha-focused 93)
;;        (alpha-unfocused 93)
;;        (alpha-lst (list alpha-focused alpha-unfocused)))
;;   (set-frame-parameter (selected-frame) 'alpha alpha-lst)
;;   (add-to-list 'default-frame-alist alpha-lst))



;; Only browse interesting buffers. Not *scratch*, *messages*, etc.
;;(global-set-key "\C-x\C-b" 'bs-show)


;; case insensitive for emacs completion
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completion-ignore-case t)

(progn ;; allows you to use M-x even if you're in the minibuffer.
  (setq enable-recursive-minibuffers t)
  ;; displays depth in minibuffer.
  ;; (minibuffer-depth-indicate-mode)
  )

;; if we haven't bound leader-b to buffer switching yet.
(when (or (eq my-curr-computer 'raspberry-pi)
          (eq my-narrow-type 'icicles)
          (null my-narrow-type))
  (when my-use-evil-p
    (evil-leader/set-key "b" #'switch-to-buffer)
    ;;(evil-leader/set-key "b" #'ibuffer)
    ))


(line-number-mode 0) ; show/hide line # in mode line. altternative `what-line'
(column-number-mode 0) ; show/hide column # in mode line.
;; do not display modes in the mode-line. They take up too much space.
;; Function `describe-mode' (kbd "C-h m") is better to see active modes anyway.
(setq mode-line-modes nil)
;; hide the % of the buffer you are viewing. Used to set to nil, but that
;; broke `nyan-mode' which manipulates this variable.
;; TODO: look into this more to understand the format of this variable.
(setq mode-line-position '((size-indication-mode nil)
                           (line-number-mode nil)))


;; Don't suggest keybinds in minibuffer.  It messes up functions that use the
;; minibuffer for output.  Instead use "C-h f func" to see the current keybind
;; of a function.
(setq suggest-key-bindings nil)


;; replacing position info in mode line with a function called on demand.
;; Bound to "g a".
(autoload #'my-what-line "my-misc" nil t)
(autoload #'my-what-position "my-misc" nil t)
(autoload #'my-what-time "my-misc" nil t)
(when my-use-evil-p
  ;; (evil-define-key 'normal global-map (kbd "g a") #'my-what-position)
  (define-key evil-normal-state-map "ga" #'my-what-position))


(progn ;; show time in mode line
  (with-eval-after-load 'time
    ;; disable process average display. Not sure why this is mixed in with time
    ;; display.
    (setq display-time-default-load-average nil)
    (setq display-time-load-average nil)
    (setq display-time-load-average-threshold nil)
    ;;(setq-default display-time-day-and-date t)
    ;;(setq-default display-time-format "%-m/%-d %-I:%M%#p")
    ;; (setq display-time-format "%-I:%M%#p")
    (setq display-time-format "%-m-%-d %a %-I:%M%#p"))
  (display-time-mode 1))

;; show lambdas with the Greek symbol
;; (when (or (> emacs-major-version 24)
;;           (and (= emacs-major-version 24)
;;                (>= emacs-minor-version 4)))
;;   (unless (eq my-curr-computer 'raspberry-pi)
;;     (global-prettify-symbols-mode 1)))

;;indent keyword args properly. Use common lisp-style for (if) indentation too?
;;(setq lisp-indent-function 'common-lisp-indent-function)

(progn
  ;; turn off start up screen
  (setq inhibit-startup-message t)

  ;; don't display info in the modeline on start up.
  ;; Need to override this method to do nothing as it looks at the current
  ;; user-name.
  ;; TODO: find an alternative solution???
  (defun display-startup-echo-area-message ()))

;;(setq initial-scratch-message ";; Scratch buffer ;;\n\n\n\n")
(setq initial-scratch-message "\n\n\n\n\n")
;; (setq initial-buffer-choice (lambda () (get-buffer-create "foo")))
(setq initial-major-mode #'fundamental-mode) ;;for faster startup.


(progn ;; highlight current line stuff
  (with-eval-after-load 'hl-line
    (setq hl-line-sticky-flag nil)
    (setq global-hl-line-sticky-flag nil))
  ;; (global-hl-line-mode 1)
  ;; (hl-line-mode 1) ; highlight the current line
  )


(setq-default line-spacing nil)


;; use tilde's as the fringe graphic for empty lines. Like Vim.
(when my-graphic-p
  (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)

  (defface my-tilde-face
    '((((background light) (class color))
       :foreground "black")
      (((background dark) (class color))
       :foreground "dark gray"))
    "Face for Vim tildes ~ in the fringe."
    :group 'basic-faces)

  (set-fringe-bitmap-face 'tilde 'my-tilde-face))

(setq-default indicate-empty-lines t) ;Like vim's tildes
;; (setq-default indicate-buffer-boundaries '((up . nil) (down . nil)
;;                                            (top . left) (bottom . left)))

(setq-default transient-mark-mode t)  ;show selected regions
;;(setq-default visible-bell t)
(setq ring-bell-function 'ignore)

(progn ;; paren match highlight
  (setq show-paren-delay 0)
  (show-paren-mode 1))


(progn ;; tab handling
  (setq-default indent-tabs-mode nil) ;;Use only spaces, no tabs.
  (setq-default tab-width my-indent-width)
  (setq-default indent-line-function #'insert-tab))

(progn ;; for better or worse, prevent creation of tmp backup files
  (setq make-backup-files nil)          ;No annoying backup files
  (setq-default backup-inhibited t)
  (setq auto-save-default nil)          ;No annoying auto-save files
  )




;; disable annoying newline emacs automatically adds to the end of a file when
;; saving.
(setq require-final-newline nil)
;; for certain modes (like c-mode) a final new line is required per the
;; language specification. For now do not add newlines per mode.
(setq mode-require-final-newline nil)

;; (defun sneak-forward ()
;;   (interactive)
;;   ;(isearch-forward-word)
;;   (isearch-forward)
;;   (evil-backward-word-begin))
;; ;(define-key evil-normal-state-map "s" 'sneak-forward)

;;;; increase/decrease font size
;; (global-set-key (kbd "M-=")
;;                 '(lambda () (interactive) (text-scale-increase 1)))
;; (global-set-key (kbd "M--")
;;                 '(lambda () (interactive) (text-scale-decrease 1)))



;;;----------------------------------------------------------------------------
;;; my-square-one
;;;----------------------------------------------------------------------------
(autoload #'my-square-one "my-square-one" nil t)
(when my-use-evil-p
  (evil-leader/set-key "0" #'my-square-one))



;;;----------------------------------------------------------------------------
;;; Turn on disabled functions
;;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
;;(put 'dired-find-alternate-file 'disabled nil)


;; (defun eval-prev ()
;;   "Evaluates the previous Sexp and inserts at point.
;; Searches backward to the last ) to find the prev."
;;   (interactive)
;;   (save-excursion
;;     (search-backward ")")
;;     (forward-char)
;;     (eval-last-sexp t) ;print result in buffer
;;     (search-backward ")") ;go back to ) again so we can cut/paste it.
;;     (forward-char)
;;     (kill-line))
;;   (yank))
;; (evil-leader/set-key "p" 'eval-prev)

;; (defun mytest ()
;;   (interactive)
;;   (let ((txt (read-string "type something: "
;;                           nil
;;                           'my-history)))
;;     (message "you said: %s" txt)))

;;;----------------------------------------------------------------------------
;;; rand
;;;----------------------------------------------------------------------------
(autoload 'rand "my-rand" nil t)

;; (setq msgDb '("hi"))

;; (setq msgIndex 0)

;; (defun msg ();Use random once database is large enough to rarely get a dupe.
;;   "Display a message."
;;   (interactive)
;;   (let* ((max (- (length msgDb) 1))
;;          (msg (nth msgIndex msgDb)))
;;     (setq msgIndex (+ 1 msgIndex))
;;     (when (> msgIndex max)
;;       (setq msgIndex 0))
;;     (clippy-say msg)))

;; ;; (defun msg ()
;; ;;   "Display a random message."
;; ;;   (interactive)
;; ;;   (let* ((max (- (length msgDb) 1))
;; ;;          (i (rand 0 max))
;; ;;          (msg (nth i msgDb)))
;; ;;     ;(message msg)
;; ;;     (clippy-say msg)
;; ;;     ;(clippy-say (yow))
;; ;;     ))

;; (evil-leader/set-key "m" 'msg)

;;;----------------------------------------------------------------------------
;;; touch typing
;;;----------------------------------------------------------------------------
;; defined in ~/.emacs.d/notElpa/mine/my-type-tutor.el
(autoload 'my-type-tutor "my-type-tutor" nil t)


;;;----------------------------------------------------------------------------
;;; interact with Microsoft SQL Server
;;;----------------------------------------------------------------------------
;;sqlcmd -S 127.0.0.1,42000\OSHE
;;sqlcmd -S 127.0.0.1,42000\OSHE -q "SELECT 'hello';"

;;;----------------------------------------------------------------------------
;;; remove all bold face attributes
;;;----------------------------------------------------------------------------
;; (defun my-remove-bold ()
;;   "Remove all bold face attributes."
;;   (interactive)
;;   (mapc
;;    (lambda (face)
;;      (set-face-attribute face
;;                          nil ;; all frames
;;                          :weight 'normal))
;;    (face-list)))


;;;----------------------------------------------------------------------------
;;; restore values set earlier for startup time. JUMPrestore
;;;----------------------------------------------------------------------------
(setq file-name-handler-alist file-name-handler-alist-backup)
(setq gc-cons-threshold gc-cons-threshold-backup)

;; Let the junk vars live for now. Unbinding them breaks profile-dotemacs.el
;; ;; unbind junk variables. Avoid namespace pollution.
;; (makunbound 'file-name-handler-alist-backup)
;; (makunbound 'gc-cons-threshold-backup)


;;; init.el ends here
