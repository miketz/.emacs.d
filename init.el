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
;;;     git clone https://github.com/miketz/.emacs.d.git
;;;
;;; Get latest changes from github:
;;;     git pull origin master
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
;;;     git submodule update --recursive

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
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))



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
(defvar mor-switch-buff-fn)
(defvar mor-readonly-for-extra-protection-p)
(defvar mor-mode-fn)
(defvar helm-candidate-number-limit)
(defvar ivy-height)
(defvar w3-default-homepage)
(defvar w3--args)
(defvar lispy-do-pprint)
(defvar charcoal-color-cnt)
(defvar company-selection-wrap-around)

;; dynamic vars not let bound (yet). Just suppressing the free var warnings.
(defvar evil-normal-state-map)
(defvar my-packages)
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
(defvar ediff-diff3-program)
(defvar darkroom-margins)
(defvar darkroom-fringes-outside-margins)
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
(defvar mor-tmp-buffer-mode-map)
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
(declare-function counsel-cl 'suppress)
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
(declare-function counsel-tmm 'suppress)
(declare-function counsel-M-x 'suppress)
(declare-function counsel-find-file 'suppress)
(declare-function counsel-load-theme 'suppress)
(declare-function my-counsel-load-theme 'suppress)
(declare-function counsel-describe-variable 'suppress)
(declare-function counsel-describe-function 'suppress)
(declare-function counsel-yank-pop 'suppress)
(declare-function counsel-git 'suppress)
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
(declare-function counsel-el 'suppress)
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
(declare-function my-change-font-size 'suppress)
(declare-function my-load-theme 'suppress)
(declare-function color 'suppress)
(declare-function my-cycle-theme 'suppress)
(declare-function my-cycle-light-bg 'suppress)
(declare-function my-handle-weird-theme-setups 'suppress)
(declare-function my-load-theme-wrapper 'suppress)
(declare-function my-toggle-inverse-video 'suppress)
(declare-function my-rainbow-parens-dark-bg 'suppress)
(declare-function my-rainbow-parens-dark-bg-bold 'suppress)
(declare-function my-rainbow-parens-light-bg 'suppress)
(declare-function my-rainbow-parens-light-bg2 'suppress)
(declare-function my-rainbow-parens-light-bg3 'suppress)
(declare-function my-color-grandshell 'suppress)
(declare-function my-color-zenburn 'suppress)
(declare-function my-color-github 'suppress)
(declare-function my-color-badger 'suppress)
(declare-function my-color-gruvbox 'suppress)
(declare-function my-color-gruvbox-dark 'suppress)
(declare-function my-color-monokai 'suppress)
(declare-function my-color-tommyh 'suppress)
(declare-function my-color-default 'suppress)
(declare-function my-color-default-fancy 'suppress)
(declare-function my-color-gandalf 'suppress)
(declare-function my-color-leuven 'suppress)
(declare-function my-color-dichromacy 'suppress)
(declare-function my-color-firebelly 'suppress)
(declare-function my-color-molokai 'suppress)
(declare-function my-color-majapahit 'suppress)
(declare-function my-color-deeper-blue 'suppress)
(declare-function my-color-kosmos 'suppress)
(declare-function my-color-niflheim 'suppress)
(declare-function my-color-spacemacs-light 'suppress)
(declare-function my-color-tango-dark 'suppress)
(declare-function my-color-sunburn 'suppress)
(declare-function my-set-alpha 'suppress)
(declare-function my-change-alpha 'suppress)
(declare-function my-change-alpha-more-solid 'suppress)
(declare-function my-change-alpha-less-solid 'suppress)
;; (declare-function evil-define-key 'evil-core)
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
(declare-function hydra-expand-region/body 'suppress)
(declare-function paredit-forward-barf-sexp 'suppress)
(declare-function paredit-forward-slurp-sexp 'suppress)
(declare-function paredit-backward-slurp-sexp 'suppress)
(declare-function paredit-backward-barf-sexp 'suppress)
(declare-function paredit-raise-sexp 'suppress)
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
(declare-function my-open-user-folder 'suppress)
(declare-function my-open-dev-folder 'suppress)
(declare-function my-current-file-path 'suppress)
(declare-function my-current-folder-path 'suppress)
(declare-function my-folder-nav 'suppress)
(declare-function my-choose-hydra 'suppress)
(declare-function hydra-easyscroll/body 'suppress)
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
(declare-function my-cycle-line-position 'suppress)
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

;;;----------------------------------------------------------------------------
;;; Helper functions and macros
;;;----------------------------------------------------------------------------
;; eval-when-compile used to prevent flycheck `cl' warning, but only works for
;; macros?
(require 'cl-lib)

(cl-defmacro my-time-task (&body body)
  "Wrap around code to time how long it takes to execute."
  (interactive)
  `(let ((start (float-time)))
     ;; the work
     ,@body
     ;; print elapsed time
     (message
      (format "elapsed seconds: %f"
              (time-to-seconds
               (time-subtract (float-time)
                              start))))))

;; (defun my-getAtIndex (i lst)
;;   "Return the element at I from LST."
;;   (cond
;;    ((null lst) nil)
;;    ((= i 0) (car lst))
;;    (t (my-getAtIndex (- i 1) (cdr lst)))))

(defun my-str-starts-with-p (string prefix)
  "Return t if STRING begins with PREFIX."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(defun my-str-ends-with-p (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

(defun my-get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(let ((positions '(mid bot top)))
  (defun my-next-cycle-pos (pos)
    "Get the next position from `positions' based on the current POS.
Closure over `positions'."
    (car (or (cdr (memq pos positions))
             positions))))

(defun my-turn-on-electric-pair-local-mode ()
  "Attempt to turn on `electric-pair-local-mode'.
But check for existence first to avoid breaks on older Emacs versions.
Do not fall back to function `electric-pair-mode' because it's global."
  (if (fboundp #'electric-pair-local-mode)
      (electric-pair-local-mode 1)
    (message "electric-pair-local-mode not supported.")))


;;;----------------------------------------------------------------------------
;;; flags used for conditional execution
;;;----------------------------------------------------------------------------
;; (display-graphic-p)
;; system-type
;; my-curr-computer

(defvar my-computers
  '(work-laptop
    work-laptop-bash
    raspberry-pi
    utilite
    old-sony-vaio
    a-tower
    a-laptop-old
    a-laptop-faster
    leyna-laptop
    hp-tower-2009
    wild-dog)
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

(defvar my-narrow-type (cond ((eq my-curr-computer 'work-laptop) 'bare-ido)
                             ((eq my-curr-computer 'wild-dog) 'bare-ido)
                             (t 'bare-ido))
  "The package I'm currently using for narrowing completions.
Use nil for the Emacs default.
Use bare-ido for ido without the extra ido packages.
Choices: ivy ido bare-ido helm icicles sallet mish-mash nil")

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

(defvar my-use-ido-p (eq my-narrow-type 'ido)
  "If I'm using ido at the moment.
Just a convenience to avoid checks against `my-narrow-type'.")

(defvar my-use-bare-ido-p (eq my-narrow-type 'bare-ido)
  "If I'm using bare-ido at the moment.  Without lots of extra ido packages.
Just a convenience to avoid checks against `my-narrow-type'.")

(defvar my-use-mish-mash-p (eq my-narrow-type 'mish-mash)
  "If I'm using combination of several narrowing packages.
Just a convenience to avoid checks against `my-narrow-type'.")

(defvar my-swoop-fn (cond ((eq my-curr-computer 'wild-dog) #'swiper)
                          ((eq my-curr-computer 'work-laptop) #'swiper)
                          ((eq my-curr-computer 'work-laptop-bash) #'swiper)
                          ((eq my-curr-computer 'a-laptop-faster) #'swiper)
                          (my-use-ivy-p #'swiper)
                          ;; `ido-occur' is fast but does not split inputs on
                          ;; spaces. use swiper with ido for now.
                          (my-use-ido-p #'swiper)
                          (my-use-bare-ido-p #'my-occur-wild-spaces)
                          (my-use-helm-p #'helm-swoop)
                          (my-use-mish-mash-p #'swiper)
                          ;; `sallet-occur' is unusably slow. Don't use it.
                          ;; `icicle-occur' is unusably slow. Don't use it.
                          (t #'my-occur-wild-spaces))
  "Function for searching with an overview.
Choices: helm-swoop swiper")
(when my-use-evil-p
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "s") my-swoop-fn)))

(defvar my-use-lispy-p (eq my-curr-computer 'wild-dog)
  "Whether to use lispy or not.
Lispy pulls in ivy as a dependency so avoiding on slow computers.")

;;;----------------------------------------------------------------------------
;;; Packages
;;;----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/notElpa/") ; stores elisp files that are
                                               ; not "packages".
(add-to-list 'load-path "~/.emacs.d/notElpa/mine/")
(setq custom-theme-directory "~/.emacs.d/notElpa/themes/") ;color themes.
(add-to-list 'custom-theme-load-path
             "~/.emacs.d/notElpa/themes/replace-colorthemes/")

(let ((executed-p nil))
  (defun my-handle-weird-theme-setups ()
    "Some themes work in a special way with custom code to initialize them.
Originally this code would be run in the autoloads when the themes were melpa
packages.  But I am no longer using the themes as packages (for init
performance reasons).
Closure over executed-p."
    (interactive)
    (unless executed-p
      (load (concat custom-theme-directory "base16-theme"))
      (load (concat custom-theme-directory "solarized"))
      (load (concat custom-theme-directory "solarized-theme-utils"))
      (when nil
        ;; this actually turns on zonokai so don't run this automatically.
        (load (concat custom-theme-directory "zonokai")))
      (load (concat custom-theme-directory "alect-themes"))
      (load (concat custom-theme-directory "doom-themes"))
      (load (concat custom-theme-directory "doom-themes-common"))
      (load (concat custom-theme-directory "eziam-common"))
      (load (concat custom-theme-directory "farmhouse-theme-common"))
      (load (concat custom-theme-directory "punpun-common"))
      (load (concat custom-theme-directory "tao-theme"))
      (load (concat custom-theme-directory "apropospriate"))
      (progn ;; kaolin stuff
        (load (concat custom-theme-directory "kaolin-themes-lib"))
        (load (concat custom-theme-directory "kaolin-themes")))
      (load (concat custom-theme-directory "one-themes"))
      (load (concat custom-theme-directory "purp-common"))
      ;; record the fact we did the set up. To avoid doing it again.
      (setq executed-p t))))


(defvar native-line-numbers-p (boundp 'display-line-numbers)
  "Non-nil if Emacs supports native line number display.")


(defvar has-nodejs-p
  (memq my-curr-computer '(work-laptop work-laptop-bash wild-dog)))

;; TODO: set up clang on more machines.
(defvar my-has-clang-p (memq my-curr-computer
                             '(;; work-laptop
                               wild-dog
                               hp-tower-2009)))

(defvar my-install-slime-p (memq my-curr-computer
                                 '(wild-dog work-laptop utilite hp-tower-2009
                                   a-laptop-faster)))

(defvar my-install-slime-company-p (and my-install-slime-p
                                        (not (version< emacs-version
                                                       "24.4"))))

;; TODO: specify if it should use elpa or melpa version of a package.
;; NOTE: to limit package installation to specific computers (or other
;; conditions), the second place in each list item is a true/false value.
(defvar my-packages
  `((s t) ;; string library
    (evil t)
    (evil-leader t)
    (pos-tip t) ;; for pop up on eval with leader "e"
    (key-chord t)
    (slime ,my-install-slime-p)
    (paredit t)
    ;;paxedit
    ;;smartparens
    ;;redshank
    ;;auto-complete
    ;;ac-slime
    (company t)
    (company-web t)
    (slime-company ,my-install-slime-company-p)
    (ace-window t)
    (csharp-mode t)
    (js2-mode t)
    (js2-highlight-vars ,(not (version< emacs-version "24.4")))
    (skewer-mode ,(memq my-curr-computer '(work-laptop)))
    (json-mode t)

    (helm ,my-use-helm-p)
    (helm-cmd-t nil) ;; broken with recent helm versions?
    (helm-swoop ,my-use-helm-p)
    (helm-w32-launcher ,(and my-use-helm-p
                             (eq my-curr-computer 'work-laptop)))
    ;;helm-git-grep ;search text of files.
    ;;helm-ls-git ;search for files. Similar to helm-cmd-t but with git.
    ;;helm-flycheck
    ;;helm-descbinds

    (icicles ,(eq my-narrow-type 'icicles))
    ;;projectile
    ;;clippy
    ;;yasnippet
    (rainbow-delimiters t)
    (rainbow-mode t)
    (expand-region t)
    ;;multiple-cursors
    ;;(omnisharp (work-laptop))
    ;;sublimity

    (nyan-mode t)
    ;;(nyan-prompt t) ; nyan-prompt removed from melpa?

    ;;powerline
    ;;dired-details ;default feature in emacs 24.4+
    (web-mode t)
    (htmlize t)
    (magit ,(not (version< emacs-version "25.1")))
    (vimrc-mode t)
    (sicp t)
    ;;neotree
    (num3-mode t)
    (powershell t)
    (irony ,my-has-clang-p)
    (company-irony ,my-has-clang-p)
    (flycheck-irony ,my-has-clang-p)
    ;;(rtags)
    ;;aggressive-indent
    (sx ,(not (version< emacs-version "24.4")))
    (leerzeichen t)
    (darkroom t)
    ;;vim-empty-lines-mode
    (fill-column-indicator ,(not (version< emacs-version "25")))
    (flycheck t)
    (hydra t)
    ;;linum-relative
    ;;(guide-key)
    (unkillable-scratch t)
    (speed-type t)
    (bug-hunter t)

    (ivy t)
    (swiper t)
    (counsel t)
    (flx t) ;; can be used by ivy and flx-ido for ordering flx matches.

    ;;color-identifiers-mode
    ;;svg-mode-line-themes ;; only works on gnu/linux
    (avy t)
    (lispy ,my-use-lispy-p)
    ;;(worf t)
    (elisp-slime-nav t)
    ;; on 11-28-2016 electric-spacing had an unbalanced paren
    ;; seems to be fixed now. Using it again.
    (electric-spacing t)
    ;;w3
    ;;w3m
    ;;flymake-jslint
    ;; (nlinum ,(not native-line-numbers-p))
    ;; (nlinum-relative ,(not native-line-numbers-p))

    (ido-vertical-mode ,my-use-ido-p)
    (ido-grid-mode ,my-use-ido-p)
    (ido-ubiquitous ,my-use-ido-p)
    (flx-ido ,my-use-ido-p)
    ;; (ido-occur ,my-use-ido-p)
    (smex ,(or my-use-ido-p
               my-use-bare-ido-p
               my-use-ivy-p ;; smex can be used by `counsel-M-x'
               my-use-mish-mash-p))
    ;; (ov nil) ;; ov is no longer a needed dependency? keep it as a comment
    ;; because may useful for my own purposes later.
    (highlight-tail nil) ;; removed from melpa (emacs wiki purge?)
    (function-args nil)
    (highlight-indent-guides t)
    (ace-link t)
    (smart-tabs-mode t)
    (lua-mode t)
    (ggtags ,(let ((has-gnu-global-p (memq my-curr-computer
                                           '(work-laptop wild-dog))))
               has-gnu-global-p))
    (clojure-mode ,(not (version< emacs-version "25.1")))
    (iedit t) ;; include explicitly. Originally got it as a lispy dependency.
    (cider ,(memq my-curr-computer '(wild-dog)))
    ;; (hl-line+ ;; used for custom `occur' mods, but only pre emacs 25
    ;;  ,(<= emacs-major-version 24))
    (geiser nil) ;;,(memq my-curr-computer '(work-laptop))
    (debbugs ,(memq my-curr-computer '(work-laptop wild-dog)))
    (adoc-mode t)
    (markdown-mode ,(not (version< emacs-version "24.4")))
    (typescript-mode t)
    (tide ,(memq my-curr-computer '(work-laptop wild-dog)))
    (context-coloring t)
    (nov ,(not (version< emacs-version "24.4"))) ;; an epub reader
    (autothemer t) ;; dependency for some themes.
    (erc-hl-nicks t)
    (sql-indent t)
    (vdiff nil)
    (tern ,has-nodejs-p)
    (company-tern ,has-nodejs-p)
    (browse-kill-ring t)
    (git-gutter ,(not (version< emacs-version "24.3")))
    (lsp-mode ,(not (version< emacs-version "25.1")))
    (company-lsp ,(not (version< emacs-version "25.1")))
    (cquery ,(memq my-curr-computer '(wild-dog)))
    (websocket t))
  "Packages I use from elpa/melpa.")

(require 'package)

(defun my-ssl-p ()
  "True if the Emacs instance has ssl setup/enabled."
  (or (not (memq system-type '(windows-nt ms-dos)))
      (gnutls-available-p)))

;; set up package archives.
(let* ((protocol (if (my-ssl-p) "https" "http"))
       (url (concat protocol "://melpa.org/packages/")))
  (add-to-list 'package-archives `("melpa" . ,url) t))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(when (boundp 'package-pinned-packages) ; Emacs 24.4 or newer
  (setq package-pinned-packages
        '((sql-indent . "gnu")
          (electric-spacing . "melpa")
          (aggressive-indent . "melpa")
          (ggtags . "melpa")
          (vdiff . "melpa")
          (websocket . "melpa"))))

;; (setq package-enable-at-startup nil)
(cond
 ;; older emacs
 ((version< emacs-version "27.0")
  (package-initialize)) ;; activate all the packages (in particular autoloads)
 ;; newer emacs
 ((not package--initialized)
  (package-initialize t)))

(defun my-install-packages ()
  "Call this function on a new Emacs installation to install packages.
Installs packages in the list `my-packages'."
  (interactive)
  ;; fetch the list of packages available
  (unless package-archive-contents
    (package-refresh-contents))

  ;; install the missing packages
  (dolist (obj my-packages)
    (let ((pkg (cl-first obj))
          (installp (cl-second obj)))
      (when installp
        (unless (package-installed-p pkg)
          (message "installing pkg: %s" (symbol-name pkg))
          (package-install pkg))))))

(my-install-packages)

(defun my-upgrade-packages ()
  "Upgrade installed packages.
Code taken from http://oremacs.com/2015/03/20/managing-emacs-packages/"
  (interactive)
  (save-window-excursion
    (package-list-packages) ;; (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute t)))

(defun my-package-list-unaccounted-packages ()
  "Display unaccounted packages.
Like `package-list-packages', but only show packages that are installed and not
in `my-packages'.  Useful for cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (let ((my-packs (mapcar #'cl-first my-packages)))
     (cl-remove-if-not (lambda (x)
                         (and (not (memq x my-packs))
                              (not (package-built-in-p x))
                              (package-installed-p x)))
                       (mapcar 'car package-archive-contents)))))



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
;; NOTE: "fj" chord slows down movement when in visual mode when pressing "j"
;;       since it is looking for the chord.

(when my-use-evil-p

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
(defun my-cursor-light-bg ()
  "Set cursor colors and styles for a typical light background."
  (interactive)
  (custom-theme-set-variables
   (car custom-enabled-themes)

   `(evil-emacs-state-cursor    '(bar "blue"))
   `(evil-normal-state-cursor   '(hollow "black"))
   `(evil-insert-state-cursor   '(bar "black"))
   `(evil-visual-state-cursor   '(hollow "black"))
   `(evil-operator-state-cursor '(box "red"))
   `(evil-replace-state-cursor  '(hbar "orange red"))
   `(evil-motion-state-cursor   '(box "black")))
  ;; toggle visual mode to force cursor update
  (evil-visual-char)
  (evil-exit-visual-state))

(defun my-cursor-dark-bg ()
  "Set cursor colors and styles for a typical dark background."
  (interactive)
  (custom-theme-set-variables
   (car custom-enabled-themes)

   `(evil-emacs-state-cursor    '(bar "cyan"))
   `(evil-normal-state-cursor   '(hollow "spring green"))
   `(evil-insert-state-cursor   '(bar "spring green"))
   `(evil-visual-state-cursor   '(hollow "orange"))
   `(evil-operator-state-cursor '(box "red"))
   `(evil-replace-state-cursor  '(hbar "orange red"))
   `(evil-motion-state-cursor   '(box "spring green")))
  ;; toggle visual mode to force cursor update
  (evil-visual-char)
  (evil-exit-visual-state))



;;;----------------------------------------------------------------------------
;;; evil
;;;----------------------------------------------------------------------------
(with-eval-after-load 'evil

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
(defvar my-change-font-fn
  (if (and (not my-use-ivy-p)
           (not my-use-helm-p)
           (fboundp #'ivy-completing-read))
      ;; it's useful to have out of order matching while selecting the font.
      (defun my-set-frame-font-ivy ()
        (interactive)
        (let ((completing-read-function #'ivy-completing-read))
          (call-interactively #'set-frame-font)))
    ;; else default
    #'set-frame-font)
  "Function to select the font.
Prefers out of order matching if available.")

(global-set-key (kbd "<f5>") my-change-font-fn)

;; (when (or (eq my-curr-computer 'work-laptop)
;;           (eq my-curr-computer 'leyna-laptop))
;;   ;; configure default settings for fonts.
;;   (defvar my-default-font 'consolas)
;;   (defvar my-good-fonts
;;     '(
;;       ;; looks OK. fits a good number of lines on screen. flaky on bold.
;;       ;; no italic?
;;       (inconsolata "Inconsolata" 135 normal)
;;       ;; consolas is the best looking but fits fewer lines on screen.
;;       (consolas "Consolas" 125 normal)
;;       ;; good, but looks a bit "tall"
;;       (dejavu "DejaVu Sans Mono for Powerline" 120 normal)
;;       (fixedsys "FixedSys" 120 normal)))

;;   (cl-defun my-set-font (&optional
;;                          &key
;;                          (sym nil) (height nil) (weight nil)
;;                          (resize-window nil))
;;     "Sets the font.
;; If sym is not specified it uses the configured default set in
;; `my-default-font'. If height or weight are not specified then it uses the
;; configured defaults in `my-good-fonts'. Resize-window = t will adjust the
;; window so the modeline fits on screen, etc."
;;     (unless sym (setq sym my-default-font))
;;     (let ((the-font (assoc sym my-good-fonts)))
;;       (unless height (setq height (third the-font)))
;;       (unless weight (setq weight (fourth the-font)))
;;       (let ((font-str (second the-font)))
;;         (custom-set-faces
;;          `(default ((t (:family ,font-str
;;                                 :foundry "outline"
;;                                 :slant normal
;;                                 :weight ,weight
;;                                 :height ,height
;;                                 :width normal)))))))
;;     (when resize-window
;;       (my-w32-run 'restore-curr-frame)
;;       (my-w32-run 'max))))


;;;----------------------------------------------------------------------------
;;; inc/dec font size
;;;----------------------------------------------------------------------------
(let ((curr-font-size nil)) ;; Starts out unknown
  (defun my-change-font-size (step)
    "Make font bigger or smaller by STEP.
Closure over `curr-font-size'."
    (let* ((curr-size (if curr-font-size ;; use cached value if it's set
                          curr-font-size
                        (face-attribute 'default :height (selected-frame))))
           (new-size (+ curr-size step)))

      (custom-set-faces `(default ((t (:height ,new-size)))))

      ;; must cache the new value because :height does not actually inc until a
      ;; threshold is breached.
      (setq curr-font-size new-size)

      ;; commenting the window size toggle off. It's seems to have become
      ;; slow on `work-laptop'.
      ;; (when (fboundp 'my-w32-run)
      ;; TODO: make it work on non-Windows machines.
      ;;   (my-w32-run 'restore-curr-frame)
      ;;   (my-w32-run 'max))

      (message (int-to-string new-size)))))

(defun my-change-font-size-bigger ()
  "Make font size bigger."
  (interactive)
  (my-change-font-size 1)) ;; TODO: calculate "threshold" step increment.

(defun my-change-font-size-smaller ()
  "Make font size smaller."
  (interactive)
  (my-change-font-size -1)) ;; TODO: calculate "threshold" step decrement.

(global-set-key (kbd "M-=") #'my-change-font-size-bigger)
(global-set-key (kbd "M--") #'my-change-font-size-smaller)



;; (defun my-set-font-size ()
;;   "Interactive layer over my-set-font. Takes the font size as user input."
;;   (interactive)
;;   (let ((size (string-to-number (read-string "font-size: "
;;                                              nil
;;                                              'my-history))))
;;     (my-set-font :height size
;;                  :resize-window t)))
;; (defun my-set-font-weight ()
;;   "Interactive layer over my-set-font."
;;   (interactive)
;;   (let ((weight (intern (read-string "font-weight: "
;;                                              nil
;;                                              'my-history))))
;;     (my-set-font :weight weight
;;                  :resize-window t)))

(when (eq my-curr-computer 'raspberry-pi)
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(ansi-color-faces-vector
     [default default default italic underline success warning error])
   '(ansi-color-names-vector
     ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue"
      "gray50"])
   '(font-use-system-font t)
   '(tool-bar-mode nil))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "Fixed" :foundry "Misc"
                          :slant normal :weight normal :height 150
                          :width normal))))))



;;;----------------------------------------------------------------------------
;;; Color theme stuff.
;;;----------------------------------------------------------------------------
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
  ;; (autoload #'my-load-theme "my-load-theme" nil t)
  (autoload #'color "my-load-theme" nil t) ; trying to duplicate vim's :color
                                           ; interface
  (autoload #'my-cycle-theme "my-load-theme" nil t)
  (autoload #'my-cycle-light-bg "my-load-theme" nil t)

  (unless my-use-ivy-p
    (defun my-load-theme-wrapper ()
      (interactive)
      (my-handle-weird-theme-setups)
      ;; nil for no candidate limit. I want to scroll through all the themes.
      (let ((helm-candidate-number-limit nil))
        (call-interactively #'load-theme)))
    (global-set-key (kbd "<f9>") #'my-load-theme-wrapper))
  (global-set-key (kbd "<f10>") #'my-cycle-theme)
  (global-set-key (kbd "<f12>") #'my-cycle-light-bg))

(defun my-load-theme-make-bold-like-zenburn (&optional theme)
  "Activates THEME with the bolding taken from zenburn."
  (interactive)
  (let ((zen-bold-faces '())
        (zen-non-bold-faces '())
        (frame (selected-frame))
        ;; show more themes since I'm browsing in addition to selecting
        (ivy-height 25))
    (when (null theme)
      (setq theme (intern (completing-read "theme: "
                                           (mapcar 'symbol-name
                                                   (custom-available-themes))
                                           nil t))))
    ;; TODO: figure out a way to do this without actually turning on zenburn
    ;; TODO: handle :bold and the different kinds of :weight that are bold
    ;; TODO: also turn off bold on some faces to be like zenburn.
    (load-theme 'zenburn t)
    ;; collect bold and non-bold faces into lists
    (dolist (f (face-list))
      (if (eq (face-attribute f :weight frame) 'bold)
        ;;   (add-to-list 'zen-bold-faces f)
        ;; (add-to-list 'zen-non-bold-faces f)
          (push f zen-bold-faces)
        (push f zen-non-bold-faces)))
    ;; load theme and use zenburn's bolding.
    (load-theme theme t)
    (dolist (f zen-bold-faces)
      (set-face-attribute f nil :weight 'bold))
    (dolist (f zen-non-bold-faces)
      (set-face-attribute f nil :weight 'normal))))

(let ((inverse-video-p nil)) ;; Flag used by fn `my-toggle-inverse-video'.
  (cl-defun my-toggle-inverse-video (&optional (inv-p t supplied-p))
    "Toggle inverse video.
Closure over `inverse-video-p'"
    (interactive)
    (if supplied-p
        ;; if user specified
        (setq inverse-video-p inv-p)
      ;; else toggle
      (setq inverse-video-p (not inverse-video-p)))
    (dolist (f (face-list))
      (if (eq f 'region)
          (set-face-attribute f nil :inverse-video nil) ; (not inverse-video-p)
        (set-face-attribute f nil :inverse-video inverse-video-p)))))

(defun my-load-theme-inverse (&optional theme)
  "Set THEME to inverse of itself."
  (interactive)
  (when (null theme)
    (setq theme (intern (completing-read "theme: "
                                         (mapcar 'symbol-name
                                                 (custom-available-themes))
                                         nil t))))
  (load-theme theme t)        ; load theme
  (my-toggle-inverse-video t) ; invert it
  ;; (let* ((frame (selected-frame))
  ;;        ;; the foreground is the background during inverse.
  ;;        (new-bg (face-attribute 'default :foreground frame)))
  ;;   ;; TODO: convert the background to be the foreground.
  ;;   (dolist (f (face-list))
  ;;     (let ((new-fg (face-attribute f :background frame)))
  ;;       ;; setting the bg is like setting the fg during inverse
  ;;       (set-face-attribute f nil :background new-fg)
  ;;       ;; setting the fg is like setting the bg during inverse
  ;;       (set-face-attribute f nil :foreground new-bg))))
  )

(let ((file "my-color-theme-mods"))
  (autoload #'my-rainbow-parens-dark-bg file nil t)
  (autoload #'my-rainbow-parens-dark-bg-bold file nil t)
  (autoload #'my-rainbow-parens-light-bg file nil t)
  (autoload #'my-rainbow-parens-light-bg2 file nil t)
  (autoload #'my-rainbow-parens-light-bg3 file nil t)

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
  (autoload #'my-color-sunburn file nil t))

(when my-graphic-p ;; transparency stuff
  ;; TODO: auto load the transparency stuff

  ;; TODO: get `curr-alpha' on-the-fly rather than caching to avoid a
  ;; transparency "jump" if alpha var gets out of sync.
  (let ((curr-alpha 100)) ;; Starts out 100

    (cl-defun my-set-alpha (alpha)
      "Set frame's transparency to ALPHA."
      ;; exit early if not in range 1-100.
      (when (or (> alpha 100)
                (< alpha 0))
        (message (int-to-string curr-alpha))
        (cl-return-from my-set-alpha))
      (setq curr-alpha alpha)
      (set-frame-parameter (selected-frame) 'alpha
                           `(,curr-alpha ,curr-alpha))
      (message (int-to-string curr-alpha)))

    (defun my-change-alpha (step)
      "Make frame more or less transparent by STEP."
      (let ((new-alpha (+ curr-alpha step)))
        (my-set-alpha new-alpha))))

  (defun my-change-alpha-more-solid ()
    (interactive)
    (my-change-alpha 1))

  (defun my-change-alpha-less-solid ()
    (interactive)
    (my-change-alpha -1))

  (global-set-key (kbd "C-M-=") #'my-change-alpha-more-solid)
  (global-set-key (kbd "C-M--") #'my-change-alpha-less-solid))

;; theme of the week and corresponding settings. This may change often.
(progn
  ;; (when my-graphic-p ;; this isn't true for emacs daemon!
  ;;   (my-color-zenburn))

  (cond
   ((eq my-curr-computer 'wild-dog)
    (load-theme 'charcoal t)
    (set-frame-font
     "-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-9")
    ;; "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-7"
    ;; "-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso8859-7"
    ;; "-DAMA-Ubuntu Mono-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"
    ;; "-xos4-Terminus-normal-normal-normal-*-22-*-*-*-c-110-iso10646-1"
    ;; "-xos4-Terminus-bold-normal-normal-*-22-*-*-*-c-110-iso10646-1"
    ;; "-xos4-Terminus-normal-normal-normal-*-18-*-*-*-c-100-iso10646-1"
    ;; (concat "Ubuntu Mono:pixelsize=19:foundry=unknown:weight=normal:"
    ;;         "slant=normal:width=normal:spacing=100:scalable=true")
    )

   ((and (eq my-curr-computer 'work-laptop)
         my-graphic-p)
    (load-theme 'charcoal t)
    (set-frame-font
     "-raster-Dina-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1")
    ;; "-raster-ProFontWindows-normal-normal-normal-*-22-*-*-*-c-*-iso8859-1"
    ;; "-raster-Terminal-normal-normal-normal-mono-12-*-*-*-c-*-ms-oemlatin"
    ;; "-raster-Terminal-normal-normal-normal-mono-18-*-*-*-c-*-ms-oemlatin"
    ;; "-raster-r_ansi-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
    ;; "-raster-r_ansi-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
    ;; "-raster-Fixedsys-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
    ;; "-raster-Dina-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
    ;; "-outline-Consolas-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
    ;; "-raster-peep-normal-normal-normal-mono-16-*-*-*-c-*-ms-oemlatin"
    ;; "-raster-peep-normal-normal-normal-mono-21-*-*-*-c-*-ms-oemlatin"
    )

   ((eq my-curr-computer 'work-laptop-bash)
    (load-theme 'zenburn t))

   ((and (eq my-curr-computer 'work-laptop)
         (not my-graphic-p))
    (load-theme 'charcoal t))

   ((eq my-curr-computer 'leyna-laptop)
    (load-theme 'charcoal t)
    (set-frame-font
     "-raster-Terminal-normal-normal-normal-mono-18-*-*-*-c-*-ms-oemlatin"))

   ((eq my-curr-computer 'a-laptop-old)
    (load-theme 'charcoal t)
    (set-frame-font
     "-raster-Fixedsys-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"))

   ((eq my-curr-computer 'hp-tower-2009)
    (when my-graphic-p
      (load-theme 'charcoal t)
      (custom-set-faces
       '(default ((t (:family "Droid Sans Mono"
                              :foundry "unknown"
                              :slant normal
                              :weight normal
                              :height 140
                              :width normal)))))))

   ((eq my-curr-computer 'a-laptop-faster)
    (load-theme 'charcoal t)
    (set-frame-font
     "-unknown-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
    ;; (custom-set-faces
    ;;  '(default ((t (:family "Source Code Pro"
    ;;                         :foundry "adobe"
    ;;                         :slant normal
    ;;                         :weight semi-bold
    ;;                         :height 120
    ;;                         :width normal)))))
    )

   ;; unknown windows computer.
   ((and (null my-curr-computer)
         (eq system-type 'windows-nt))
    (load-theme 'charcoal t)
    (set-frame-font
     "-raster-Fixedsys-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1")))

  ;; ;; rainbow delimiters colors for windows terminal (cmd)
  ;; (when (and (not my-graphic-p)
  ;;            (eq system-type 'windows-nt))
  ;;   (custom-set-faces
  ;;    '(rainbow-delimiters-depth-1-face ((t (:foreground "white"))))
  ;;    '(rainbow-delimiters-depth-2-face ((t (:foreground "lightgreen"))))
  ;;    '(rainbow-delimiters-depth-3-face ((t (:foreground "lightred"))))
  ;;    '(rainbow-delimiters-depth-4-face ((t (:foreground "lightcyan"))))
  ;;    '(rainbow-delimiters-depth-5-face ((t (:foreground "lightmagenta"))))
  ;;    '(rainbow-delimiters-depth-6-face ((t (:foreground "lightblue"))))
  ;;    '(rainbow-delimiters-depth-7-face ((t (:foreground "brown"))))
  ;;    '(rainbow-delimiters-depth-8-face ((t (:foreground "red"))))
  ;;    '(rainbow-delimiters-depth-9-face ((t (:foreground "magenta"))))
  ;;    '(rainbow-delimiters-unmatched-face
  ;;      ((t (:foreground "lightred" :background "darkgray"))))))
  )


;;;----------------------------------------------------------------------------
;;; Recursively byte-compile every .el file
;;;----------------------------------------------------------------------------
;;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)



;;;----------------------------------------------------------------------------
;;; sly
;;;----------------------------------------------------------------------------
;; (setq my-use-sly nil)

;; (when my-use-sly
;;   (when (eq my-curr-computer 'work-laptop)
;;     (setq inferior-lisp-program
;;           "C:/Users/mtz/programs/ccl-1.10-windowsx86/ccl/wx86cl64")))

;;;----------------------------------------------------------------------------
;;; SLIME
;;;----------------------------------------------------------------------------
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
  (add-to-list 'slime-completion-at-point-functions
               'slime-fuzzy-complete-symbol)

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

  (progn
    (when (eq my-curr-computer 'wild-dog)
      (setq slime-default-lisp 'sbcl
            slime-lisp-implementations '((ccl ("~/proj/ccl/lx86cl64"))
                                         (sbcl ("sbcl")))))

    (when (eq my-curr-computer 'work-laptop)
      (setq slime-default-lisp 'ccl)
      (setq slime-lisp-implementations
            '((ccl
               ("C:/Users/mtz/programs/ccl-1.11.5-windowsx86/ccl/wx86cl64"))
              (sbcl
               ("C:/Program Files/Steel Bank Common Lisp/1.2.15/sbcl.exe"))
              (ecl ("C:/Users/mtz/programs/ecl/ecl.exe"))
              ;; clisp is just a fake example for now.
              (clisp ("~/path/to/clisp-2.49/clisp" "-modern")))))


    (when (eq my-curr-computer 'utilite)
      (setq slime-default-lisp 'ccl
            slime-lisp-implementations '((ccl ("armcl")))))

    (when (eq my-curr-computer 'hp-tower-2009)
      (setq slime-default-lisp 'ccl
            slime-lisp-implementations '((ccl ("~/software/ccl/lx86cl64"))
                                         (sbcl ("/usr/bin/sbcl")))))

    (when (eq my-curr-computer 'a-laptop-faster)
      (setq slime-default-lisp 'ccl
            slime-lisp-implementations '((ccl ("~/proj/ccl/lx86cl")))))

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
                      (save-excursion (slime))))))))

  ;; (add-hook 'slime-mode-hook #'lispy-mode)
  ;; (add-hook 'slime-repl-mode-hook #'lispy-mode)
  (defun my-setup-slime-repl ()
    ;; Turn off line numbers in the repl
    (linum-mode 0)
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
  (when my-use-evil-p
    ;; NOTE: `evil-leader/set-key-for-mode' doesn't work for minor modes
    ;;       like `slime-mode'. Binding for `lisp-mode' instead since I
    ;;       automatically turn on slime.
    ;; TODO: find alternative to fn `evil-leader/set-key-for-mode' or
    ;;       even an alternative to `evil-leader' itself.
    ;; (evil-leader/set-key-for-mode 'slime-mode "e" eval-fn)
    (evil-leader/set-key-for-mode 'lisp-mode "e"
      #'my-slime-eval-last-sexp-display)
    (evil-leader/set-key-for-mode 'slime-repl-mode "e"
      #'my-slime-eval-last-sexp-display))

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
    (define-key slime-mode-map (kbd "C-M-i") #'counsel-cl))
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
(add-hook 'after-init-hook #'global-company-mode) ; all buffers

(with-eval-after-load 'company
  (when my-use-evil-p
    ;; C-Space like Visual Studio
    (evil-define-key 'insert company-mode-map (kbd "C-SPC") #'company-complete)
    ;; C-SPC doesn't work in some terminals, so bind an alternative key.
    (evil-define-key 'insert company-mode-map (kbd "C-o") #'company-complete)
    ;; (define-key company-mode-map (kbd "C-SPC") #'company-complete)
    )

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

  (let* ((pos nil) ; cache values for repeated M-r presses.
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
        (setq pos (my-next-cycle-pos (if repeatp pos nil))))
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
;;; company-web
;;;----------------------------------------------------------------------------
(with-eval-after-load 'web-mode
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim)

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
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))


(defvar my-main-todo (cond ((eq my-curr-computer 'wild-dog)
                            "~/todo/TODO.org")

                           ((eq my-curr-computer 'work-laptop)
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
    (add-to-list 'org-todo-keywords '(sequence "HOLD") t)

    ;; (defface org-hold ;; font-lock-warning-face
    ;;   '((t (:foreground "powder blue" :weight bold)))
    ;;   "Face for HOLD keywords."
    ;;   :group 'org-faces)

    ;; icy color to make HOLD items look frozen.
    (setq org-todo-keyword-faces '(("HOLD" . (:foreground "deep sky blue"
                                                          :weight bold)))))

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
    (define-key org-mode-map (kbd "M-h") #'evil-window-left)))

;;;----------------------------------------------------------------------------
;;; worf. key shortcuts for org-mode
;;;----------------------------------------------------------------------------


;;;----------------------------------------------------------------------------
;;; csharp-mode
;;;----------------------------------------------------------------------------
;;(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

(with-eval-after-load 'csharp-mode
  (defun my-setup-csharp-mode ()
    (yas-minor-mode 1)
    (rainbow-delimiters-mode 1)

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
  (defun my-setup-js ()
    ;; set explicitly because shorter width in json mode corrupts it.
    (setq js-indent-level my-indent-width))
  (add-hook 'js-mode-hook #'my-setup-js))


;;;----------------------------------------------------------------------------
;;; js2-mode
;;;----------------------------------------------------------------------------
;;(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

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

  (defhydra my-hydra-js2-flymake (:color amaranth)
    "jslint:flymake: "
    ("n" flymake-goto-next-error)
    ("p" flymake-goto-prev-error)
    ("v" flymake-popup-current-error-menu)
    ("C-g" nil nil)
    ("q" nil))
  (when my-use-evil-p
    (evil-define-key 'normal js2-mode-map (kbd "C-c l")
      #'my-hydra-js2-flymake/body))

  ;; (evil-define-key 'normal js2-mode-map (kbd "C-c h") #'my-hydra-hs/body)

  ;; Add parsing of jslint output in compilation mode
  ;; (add-to-list 'compilation-error-regexp-alist-alist
  ;;              '(jslint "^\\(.*\\): line \\([0-9]+\\), col \\([0-9]+\\), "
  ;;                       1 2 3))
  ;; (add-to-list 'compilation-error-regexp-alist 'jslint)

  (defun my-js2-init ()
    (js2-highlight-unused-variables-mode t)
    ;; replace ambiguous name "Javascript-IDE" with "js2"
    (setq mode-name "js2")
    ;; (setq-default js2-global-externs "jQuery $")
    ;; (setq-default js2-indent-on-enter-key t)
    ;; (add-to-list 'js2-ecma-262-externs "setTimeout")

    ;; (when (featurep 'js2-highlight-vars)
    ;;   (js2-highlight-vars-mode))

    ;;(js2-imenu-extras-mode)

    (my-turn-on-electric-pair-local-mode)
    ;; (smartparens-mode 1)


    (yas-minor-mode 1)
    (rainbow-delimiters-mode-enable)
    (electric-spacing-mode 1)
    ;; use jslint, but only if editing a .js file on disk.
    ;; TODO: use with in-memory buffer, or narrowed region of html file.
    ;; TODO: use flycheck instead of flymake
    (when (and buffer-file-name
               (my-str-ends-with-p buffer-file-name ".js"))
      ;; wireup M-x compile
      (set (make-local-variable 'compile-command)
           (concat "jslint --terse " (shell-quote-argument buffer-file-name)))
      ;; ;; and turn on flymake-jslint. (only works on saved files)
      ;; (flymake-jslint-load)
      ;; ;; bind M-n, M-p to use flymake functions instead of js2 functions
      ;; (evil-define-key 'normal js2-mode-map (kbd "M-n")
      ;;   #'flymake-goto-next-error)
      ;; (evil-define-key 'normal js2-mode-map (kbd "M-p")
      ;;   #'flymake-goto-prev-error)
      ;; (evil-define-key 'normal js2-mode-map (kbd "C-c m")
      ;;   #'flymake-popup-current-error-menu)
      )
    ;; show a Greek lambda for function
    (setq prettify-symbols-alist '(("function" . 955)))
    ;; collapse/show sections of code
    (hs-minor-mode 1))

  (add-hook 'js2-mode-hook #'my-js2-init))

;;;----------------------------------------------------------------------------
;;; json-mode
;;;----------------------------------------------------------------------------
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
;;(evil-escape-mode 1)



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
(when my-use-evil-p
  (evil-leader/set-key "g" #'my-grep-dwim))

;;;----------------------------------------------------------------------------
;;; helm-swoop
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
;;; icomplete
;;;----------------------------------------------------------------------------
(with-eval-after-load 'icomplete
  (setq icomplete-compute-delay 0))

;;;----------------------------------------------------------------------------
;;; ido
;;; ido-vertical-mode
;;; ido-ubiquitous
;;; flx-ido
;;; smex (built on ido)
;;;----------------------------------------------------------------------------
(when (or my-use-ido-p
          my-use-bare-ido-p)
  ;;use swiper on "s" even when using ido.
  ;; (when my-use-evil-p
  ;;   (define-key evil-normal-state-map (kbd "s") #'swiper))

  ;; ;; icomplete's display is similar to ido. So use it for completions ido
  ;; ;; does not support. (ie `describe-function' `load-theme' etc)
  ;; (icomplete-mode 1)

  (setq ido-enable-flex-matching nil)
  (setq ido-everywhere t)
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

  (defun my-ido-find-file ()
    "Calls `ido-find-file'.
But shadows `ido-work-directory-list' to prevent ido from brining in
completions from folders other than the current one."
    (interactive)
    (let ((ido-work-directory-list '()))
      (ido-find-file)))

  (when (or my-use-ido-p
            my-use-bare-ido-p)
    (global-set-key (kbd "C-x C-f") #'my-ido-find-file))

  (when my-use-ido-p ;; GUARD against calling ido-ubiquitous-mode.

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

(with-eval-after-load 'smex
  ;; GUARD: smex is used for `counsel-M-x' too where this advice is not needed.
  (when (or my-use-ido-p
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
;;; Yasnippet
;;;----------------------------------------------------------------------------
;; ;;(add-to-list 'load-path "~/.emacs.d/yasnippet")


(progn
  ;; an older copy of yasnippet before it caused unwanted indentation on
  ;; each keystroke.
  (add-to-list 'load-path "~/.emacs.d/yasnippet-20160416.831_correctIndent")

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
;;; cc-mode
;;;----------------------------------------------------------------------------
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

  (defun my-setup-c-mode-common ()
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

    (electric-spacing-mode 1)
    (when (fboundp #'fci-mode)
      (fci-mode 1)))
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
      (smart-tabs-mode-enable)
      (smart-tabs-advice c-indent-line c-basic-offset)
      (smart-tabs-advice c-indent-region c-basic-offset)))
  (add-hook 'c-mode-hook #'my-setup-c-mode)

  (defun my-setup-c++-mode ()
    (progn ;; use linux style tabbing/indentation.
      ;; these values should be buffer local.
      (setq c-basic-offset my-indent-width-c)
      (setq tab-width my-indent-width-c)
      (setq indent-tabs-mode t))

    (progn ;; smart-tabs-mode
      (smart-tabs-mode-enable)
      (smart-tabs-advice c-indent-line c-basic-offset)
      (smart-tabs-advice c-indent-region c-basic-offset)))
  (add-hook 'c++-mode-hook #'my-setup-c++-mode)


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
;;; Dired
;;;----------------------------------------------------------------------------
(with-eval-after-load 'dired ; dired -> dired.el in `load-path'
  (setq-default dired-isearch-filenames t) ;search file names only in Dired.
  (defun my-setup-dired ()
    (when (fboundp #'dired-hide-details-mode) ;; avoid break on older emacs
      (dired-hide-details-mode 1)))
  (add-hook 'dired-mode-hook #'my-setup-dired)

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
    (sqlind-minor-mode 1))

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
;; (require 'rainbow-delimiters)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
;; (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
(add-hook 'sql-mode-hook #'rainbow-delimiters-mode)
(add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)
;; (add-hook 'sly-mrepl-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;;(global-rainbow-delimiters-mode)

;;;----------------------------------------------------------------------------
;;; rainbow-mode
;;;----------------------------------------------------------------------------
;;(rainbow-mode)

;;;----------------------------------------------------------------------------
;;; Expand-region
;;; https://github.com/magnars/expand-region.el
;;;----------------------------------------------------------------------------
;;(require 'expand-region)
;; (autoload 'expand-region "expand-region" "expand region" t)
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
;; ;;(add-to-list 'load-path "~/.emacs.d/paredit")
;; (autoload 'enable-paredit-mode "paredit"
;;   "Turn on pseudo-structural editing of Lisp code." t)
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
          (add-to-list 'company-backends 'company-omnisharp))))

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
(global-set-key (kbd "M-g g") #'avy-goto-line)
(global-set-key (kbd "M-g M-g") #'avy-goto-line)
;; TODO: fix issue (maybe upstream too?) where `avy-isearch' doesn't
;; work with evil "/" command. But it does work with evil's "?".
(define-key isearch-mode-map (kbd "C-SPC") #'avy-isearch)
;; (define-key evil-normal-state-map (kbd "s") ; like vim sneak.
;;   #'avy-goto-char-2)
;; (define-key evil-motion-state-map (kbd "s") #'avy-goto-char-2)
(when my-use-evil-p
  (define-key evil-normal-state-map (kbd "SPC") #'avy-goto-word-1)
  (define-key evil-motion-state-map (kbd "SPC") #'avy-goto-word-1)
  ;; (define-key evil-normal-state-map (kbd "SPC") #'avy-goto-char-timer)
  ;; (define-key evil-motion-state-map (kbd "SPC") #'avy-goto-char-timer)
  )

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
  (setq avy-timeout-seconds 0.3) ;; shorten delay for `avy-goto-char-timer'

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
    (add-to-list 'exec-path "C:/Users/mtz/programs/LLVM/bin"))

  ;; (irony-cdb-autosetup-compile-options) ;should be in the hook
  )



;; Only needed on Windows
(when (eq system-type 'windows-nt)
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

(when (eq my-curr-computer 'wild-dog)
  (let ((lisp-file "my-proj-wild-dog"))
    (autoload #'my-proj-dive-python lisp-file nil t)
    (autoload #'my-proj-pcl lisp-file nil t)
    (autoload #'my-proj-progit2 lisp-file nil t)
    (autoload #'my-proj-progit2-dired lisp-file nil t)
    (autoload #'my-proj-ydnjs lisp-file nil t)
    (autoload #'my-proj-paip lisp-file nil t)))

(when (eq my-curr-computer 'work-laptop)
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
      (dired "C:/Users/mtz/proj/TFS/SafetyWebsite/OSHE/Development"))
    (evil-leader/set-key "4" #'my-open-dev-folder)))


(when (eq system-type 'gnu/linux)
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
(let ((file-hook (if (version< emacs-version "22.1")
                     'find-file-hooks
                   'find-file-hook))
      (vc-hook-attach (if (version< emacs-version "25.1")
                          'vc-find-file-hook
                        'vc-refresh-state)))
  ;; remove a vc source control hook that slows down opening files.
  (remove-hook file-hook vc-hook-attach))

(with-eval-after-load 'vc
  (add-to-list 'vc-directory-exclusion-list "bin")
  (add-to-list 'vc-directory-exclusion-list "obj")
  (when my-use-evil-p
    ;; use emacs bindings (not evil).
    (add-to-list 'evil-buffer-regexps '("\\*vc" . emacs))))

;; (add-hook 'vc-- (lambda () (linum-mode 0)))

;;;----------------------------------------------------------------------------
;;; Projectile
;;;----------------------------------------------------------------------------
;; (require 'projectile)
;; (projectile-global-mode)
;; (setq projectile-indexing-method 'native)
;; (setq projectile-enable-caching t)
;; (define-key projectile-mode-map (kbd "C-x C-b") 'projectile-ibuffer)

;;;--------------------
;;; icicles
;;;--------------------
(when (and (eq my-narrow-type 'icicles)
           (fboundp #'icicle-mode)) ; package installed. autoloaded fn
  (require 'icicles)
  (icicle-mode 1))

;;;----------------------------------------------------------------------------
;;; web-mode
;;;----------------------------------------------------------------------------
;;(require 'web-mode)
(autoload 'web-mode "web-mode" "web mode" t)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))

(with-eval-after-load 'web-mode
  ;; Auto-close on > and </
  (setq web-mode-auto-close-style 2)

  ;; highlights for start/closing tags and vertical line between them.
  (setq web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight  t)

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
;;(require 'vimrc-mode)
(autoload 'vimrc-mode "vimrc-mode" "vimrc mode" t)
(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))

;;;----------------------------------------------------------------------------
;;; Make dired appear in a side window
;;;----------------------------------------------------------------------------
(autoload #'my-current-file-path "my-folder-nav" nil t)
(autoload #'my-current-folder-path "my-folder-nav" nil t)
(autoload #'my-folder-nav "my-folder-nav" nil t)
(global-set-key (kbd "<f8>") #'my-folder-nav)


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
    (linum-mode 0)
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
;;; magit
;;;----------------------------------------------------------------------------
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
    (add-to-list 'evil-buffer-regexps '("\\*magit" . emacs)))

  (when my-use-ivy-p
    (setq magit-completing-read-function #'ivy-completing-read))

  ;; Special highlight on the changed words in a line. Makes it easier to see
  ;; what changed. If 'all becomes a performance problem then set it to t so
  ;; it only affects the currently highlighted diff.
  (setq magit-diff-refine-hunk 'all)

  ;; use colored graph lines. Could be a performance issue.
  (add-to-list 'magit-log-arguments "--color")

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

  (when (eq my-curr-computer 'work-laptop)
    (setq ediff-diff3-program "C:/Program Files/KDiff3/bin/diff3.exe")))


;;;----------------------------------------------------------------------------
;;; helm-w32-launcher. Microsoft Windows only?
;;;----------------------------------------------------------------------------
(when (fboundp #'helm-w32-launcher) ;;(eq system-type 'windows-nt)
  (global-set-key (kbd "C-c w") #'helm-w32-launcher))

;;;----------------------------------------------------------------------------
;;; leerzeichen. Displays symbols for tab, space, and newline.
;;;----------------------------------------------------------------------------
;; (autoload 'leerzeichen-mode "leerzeichen" nil t)
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
;; (require 'darkroom)
(autoload 'darkroom-mode "darkroom" "darkroom-mode" t)
(with-eval-after-load 'darkroom
  (setq darkroom-margins 0.15)
  ;;nil keeps margins close to the centered text.
  (setq darkroom-fringes-outside-margins nil))

;;;----------------------------------------------------------------------------
;;; vim-empty-lines-mode
;;;----------------------------------------------------------------------------
;; ;; messes up recenter-top-bottom so not using for now.
;; (global-vim-empty-lines-mode)

;;;----------------------------------------------------------------------------
;;; fill-column-indicator, fci-mode
;;;----------------------------------------------------------------------------
;; (require 'fill-column-indicator)
;; (add-hook 'prog-mode-hook (lambda ()
;;                             (fci-mode 1))) ; fci-mode is autoloaded.


(defvar-local company-fci-mode-on-p nil) ; put this in top level.
                                         ; flycheck warning.
(with-eval-after-load 'fill-column-indicator
  (setq fci-rule-column 79)
  (setq fci-rule-width 1)
  ;; use text, not bitmap to avoid increasing line spacing with fixedsys font.
  (setq fci-always-use-textual-rule t)
  (progn
    (setq fci-dash-pattern 0.5)   ;; length of the dash 0 to 1
    (setq fci-rule-use-dashes t))
  ;; (setq fci-rule-color "#4d4d4d") ;; tailored for zenburn ATM.

  (defun my-fci-refresh ()
    (interactive)
    (fci-mode 0)
    (fci-mode 1))

  (progn ;work-around issue where `fill-column-indicator' moves company
    ;; suggestion box.
    ;; TODO: handle for auto-complete too. It's on emacs.stackexchange.
    ;; (defvar-local company-fci-mode-on-p nil) ; moved to top level

    (defun company-turn-off-fci (&rest _ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))

    (defun company-maybe-turn-on-fci (&rest _ignore)
      (when company-fci-mode-on-p (fci-mode 1)))

    (add-hook 'company-completion-started-hook #'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook #'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook #'company-maybe-turn-on-fci)))


;; (progn
;;   ;; make fci compatible with emacs built-in variable
;;   ;; `show-trailing-whitespace'.
;;   ;; TODO: it doesn't seem to be working!
;;   ;; TO-DID: used "white-space-mode" instead of `show-trailing-whitespace'.
;;   (setq whitespace-style '(face trailing)))

;;;----------------------------------------------------------------------------
;;; flycheck
;;;----------------------------------------------------------------------------
(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "M-n") #'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") #'flycheck-previous-error)
  ;;(evil-define-key 'flycheck-mode-map (kbd "M-n") #'flycheck-next-error)

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
;;; hydra
;;;----------------------------------------------------------------------------
(autoload #'my-choose-hydra "my-hydras" nil t)
(autoload #'hydra-easyscroll/body "my-hydras" nil t)

(when my-use-evil-p
  (define-key evil-normal-state-map (kbd "z s") #'hydra-easyscroll/body)
  ;; (define-key evil-normal-state-map (kbd "\\") #'my-choose-hydra)
  ;; (define-key evil-motion-state-map (kbd "\\") #'my-choose-hydra)
  )

(with-eval-after-load 'hydra
  (setq hydra-is-helpful t)
  ;; don't use window for hints. It seems to lock things up.
  ;; And window switcher mode really gets messed up.
  (setq hydra-lv nil))



;;;----------------------------------------------------------------------------
;;; erc
;;;----------------------------------------------------------------------------
(with-eval-after-load 'erc
  (let ((format "%-I:%M%#p")) ;; use 12 hour format. am/pm
    (setq erc-timestamp-format       format
          erc-timestamp-format-right format))

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
        "Channel:"
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
(unkillable-scratch 1)


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
;;; swiper. ivy is (or at least was) bundled with swiper.
;;; ivy
;;; counsel -> provides extra features for completing some things.
;;;----------------------------------------------------------------------------
(when my-use-ivy-p
  (when my-use-evil-p
    ;; (define-key evil-normal-state-map (kbd "s") #'swiper)
    (evil-leader/set-key "b" #'ivy-switch-buffer))

  (when (eq my-ui-type 'emacs)
    (global-set-key (kbd "C-c C-s") #'swiper)
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
    (global-set-key (kbd "C-x C-f") #'counsel-find-file)
    (defun my-counsel-load-theme ()
      (interactive)
      (my-handle-weird-theme-setups)
      ;; (let ((ivy-height 100)) ;; taller ivy window for viewing themes.
      ;;   (call-interactively #'counsel-load-theme))
      (counsel-load-theme))
    (global-set-key (kbd "<f9>") #'my-counsel-load-theme)
    (global-set-key (kbd "C-h v") #'counsel-describe-variable)
    (global-set-key (kbd "C-h f") #'counsel-describe-function)
    ;; ;; replace keybind for `bookmark-bmenu-list'
    ;; (global-set-key (kbd "C-x r l") #'counsel-bookmark)
    (when my-use-evil-p
      (evil-leader/set-key "w" #'counsel-yank-pop)
      (evil-leader/set-key "h" #'counsel-git) ; safe on ms-windows
      )))

(with-eval-after-load 'swiper
  (define-key swiper-map (kbd "C-SPC") #'swiper-avy)
  (define-key swiper-all-map (kbd "C-SPC") #'swiper-avy))

(with-eval-after-load 'ivy

  (when my-use-evil-p
    ;; use emacs bindings (not evil) in ivy-occur buffer
    (add-to-list 'evil-buffer-regexps '("^*ivy-occur" . emacs)))

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

  (global-set-key (kbd "<f7>") #'my-cycle-ivy-match-style)

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
    (evil-leader/set-key "h" #'counsel-git) ; safe on ms-windows
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
(add-to-list 'load-path "~/.emacs.d/notElpa/mine/mor")
(autoload #'mor-mode-on-region "mode-on-region" nil t)
(autoload #'mor-prev-mode-on-region "mode-on-region" nil t)
;; Recommended keybinds for vanilla Emacs. Press "C-c m" with text highlighted.
(global-set-key (kbd "C-c m") #'mor-mode-on-region)
(global-set-key (kbd "C-c .") #'mor-prev-mode-on-region)
;; Recommended keybinds for evil users. Press "m" in visual mode.
(eval-after-load 'evil
  '(progn
     (define-key evil-visual-state-map (kbd "m") #'mor-mode-on-region)
     (define-key evil-visual-state-map (kbd ".") #'mor-prev-mode-on-region)))
;; Configure
(eval-after-load 'mode-on-region
  '(progn
     (setq mor-format-automatically-p nil)
     (setq mor-readonly-for-extra-protection-p t)
     (custom-set-faces
      `(mor-readonly-face
        ((t (:background "black" :foreground "red" :strike-through t)))))
     ;; recommended keybinds for the tmp buffer.  Both Vanilla and Evil.
     (define-key mor-tmp-buffer-mode-map (kbd "C-c b") #'mor-copy-back)
     (define-key mor-tmp-buffer-mode-map (kbd "C-c c")
       #'mor-close-tmp-buffer)))

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
  (setq lazy-highlight-initial-delay 0.2))

;;;----------------------------------------------------------------------------
;;; my-window-search.  Limit isearch to the visible buffer.
;;;----------------------------------------------------------------------------
(autoload #'my-window-search "my-window-search" nil t)
(global-set-key (kbd "C-c s") #'my-window-search)
(global-set-key (kbd "C-c C-s") #'my-window-search)

;; using this binding for swiper when `my-ui-type' is 'emacs
;; (global-set-key (kbd "C-c C-s") #'my-window-search)

;;;----------------------------------------------------------------------------
;;; lispy
;;;----------------------------------------------------------------------------
(when my-use-lispy-p
  (add-hook 'lisp-mode-hook #'lispy-mode)) ; for common lisp.

(with-eval-after-load 'lispy
  ;; To improve start up speed move hooks to eval-after-load. Otherwise the
  ;; scratch buffer which uses `lisp-interaction-mode' will load lispy. The
  ;; first lispy start will be manual, then hooks be set up. On work-laptop
  ;; this speeds start up by 3-4 seconds.
  (add-hook 'emacs-lisp-mode-hook       #'lispy-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'lispy-mode)
  (add-hook 'ielm-mode-hook             #'lispy-mode)
  (add-hook 'lisp-interaction-mode-hook #'lispy-mode)
  ;; (add-hook 'scheme-mode-hook           #'lispy-mode)
  (add-hook 'slime-repl-mode-hook       #'lispy-mode)

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
(add-to-list 'auto-mode-alist '("\\.info\\'" . my-info-mode))

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
  (define-key emacs-lisp-mode-map (kbd "C-M-i") #'counsel-el)
  (define-key lisp-interaction-mode-map (kbd "C-M-i") #'counsel-el))

;; (with-eval-after-load "lisp-mode"
;;   (add-hook 'emacs-lisp-mode-hook
;;             (lambda ()
;;               (push '("lambda" . ?f) prettify-symbols-alist))))

;;;----------------------------------------------------------------------------
;;; cider-style-overlays
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

(autoload 'pos-tip-show "pos-tip" nil t)
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
(dolist (hook '(emacs-lisp-mode-hook
                ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

(with-eval-after-load 'elisp-slime-nav

  (defun my-elisp-slime-nav-colored ()
    (interactive)
    (call-interactively #'elisp-slime-nav-describe-elisp-thing-at-point)
    (with-current-buffer (help-buffer)
      (rainbow-delimiters-mode)))

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
(with-eval-after-load 'sx-tab
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
    (add-to-list 'evil-buffer-regexps '("\\*question-list" . emacs))
    (add-to-list 'evil-buffer-regexps '("\\*sx-" . emacs))))



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
(add-to-list 'auto-mode-alist '("\\.gitignore$" . shell-script-mode))
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
;;; sallet. from fuco. saved to notElpa folder as a git submodule.
;;;----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/notElpa/sallet")
(autoload #'sallet-buffer "sallet" nil t)

;; NOTE: the sallet package is not ready yet. Just configuring to give it a
;;       test run.
(when (eq my-narrow-type 'sallet)
  ;; install packages sallet depends on.
  ;; TODO: look into a way to make this happen without code for a specific
  ;; case. (sallet is from git, not package manager). Maybe scrape out the
  ;; require symbols?
  (dolist (pkg '(dash s async flx ov f))
    (unless (package-installed-p pkg)
      (package-install pkg)))

  ;; keybinds
  (when my-use-evil-p
    (evil-leader/set-key "b" #'sallet-buffer)))

;;;----------------------------------------------------------------------------
;;; sunrise-commander. saved to notElpa folder as a git submodule.
;;;----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/notElpa/sunrise-commander")
(autoload #'sunrise-cd "sunrise-commander" nil t)

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
(winner-mode 1)

;;;----------------------------------------------------------------------------
;;; js2-highlight-vars
;;;----------------------------------------------------------------------------
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
(when nil
  ;; TODO: put in a package. look for the ideal built in functions.
  (defun my-curr-line ()
    "Like `what-line' but return an integer instead of a message."
    (interactive)
    (let ((start (point-min))
          (n (line-number-at-pos)))
      (if (= start 1)
          n
        (save-excursion
          (save-restriction
            (widen)
            (+ n (line-number-at-pos start) -1))))))

  (defun my-top-screen-line ()
    (interactive)
    (line-number-at-pos (window-start)))

  (defun my-bottom-screen-line ()
    (interactive)
    (line-number-at-pos (- (window-end) 1)))

  (when nil ;; interactive testing
    (my-top-screen-line)
    (my-bottom-screen-line)))

;;;----------------------------------------------------------------------------
;;; my-test
;;;----------------------------------------------------------------------------
(autoload 'my-deftest "my-test" nil nil 'macro)


;;;----------------------------------------------------------------------------
;;; highlight-indent-guides
;;;----------------------------------------------------------------------------
(with-eval-after-load 'highlight-indent-guides
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|))


;;;----------------------------------------------------------------------------
;;; python-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'python

  ;; use custom fn to avoid alignment issues with horizontal comments. Tabs
  ;; can't be used *after* code, only before. (otherwise alignment issues
  ;; occur at different tab widths.
  (define-key python-mode-map (kbd "M-;") #'my-comment-dwim-align-with-spaces)


  (defun my-setup-python-smart-tabs-mode ()
    ;; keeps comments closer to the code. buffer local
    (setq comment-column 1)

    ;; NOTE: sometimes I use a larger tab-width for python
    (setq tab-width my-indent-width)
    (setq indent-tabs-mode t)

    (smart-tabs-mode-enable)
    (smart-tabs-advice python-indent-line python-indent-offset)
    (smart-tabs-advice python-indent-region python-indent-offset)
    (when (featurep 'python-mode)
      (smart-tabs-advice py-indent-line py-indent-offset)
      (smart-tabs-advice py-newline-and-indent py-indent-offset)
      (smart-tabs-advice py-indent-region py-indent-offset)))
  ;; hook for smart-tabs-mode
  (add-hook 'python-mode-hook #'my-setup-python-smart-tabs-mode)

  (defun my-setup-python ()
    (yas-minor-mode 1)
    (rainbow-delimiters-mode-enable)
    (my-turn-on-electric-pair-local-mode))
  (add-hook 'python-mode-hook #'my-setup-python))

;;;----------------------------------------------------------------------------
;;; calendar
;;;----------------------------------------------------------------------------
(with-eval-after-load 'calendar
  ;; default is 8 which *should* be correct but seems I need to bump up to 9
  ;; to stop the calendar window from resizing.
  (setq calendar-minimum-window-height 9)

  (when my-use-evil-p ;; use emacs, not evil bindings in calendar.
    (add-to-list 'evil-buffer-regexps '("\\*Calendar*" . emacs))))


;;;----------------------------------------------------------------------------
;;; twelve-m-calendar.el
;;;----------------------------------------------------------------------------
;; (autoload #'year-calendar "twelve-m-calendar" nil t)

;;;----------------------------------------------------------------------------
;;; smart-tabs-mode
;;;----------------------------------------------------------------------------
;; NOTE: just setting up hooks manually in eval-after-load for specific langs.
;; (smart-tabs-insinuate 'c)

;;;----------------------------------------------------------------------------
;;; lua-mode
;;;----------------------------------------------------------------------------
;; NOTE: auto-mode-alist is already taken care of in lua-mode-autoloads.el
(with-eval-after-load 'lua-mode
  (setq lua-indent-level 2
        lua-default-application "luajit")

  (defun my-setup-lua-mode ()
    (yas-minor-mode 1)
    (rainbow-delimiters-mode 1)
    (electric-spacing-mode 1)
    (my-turn-on-electric-pair-local-mode))
  (add-hook 'lua-mode-hook #'my-setup-lua-mode))

;;;----------------------------------------------------------------------------
;;; swift-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'swift-mode
  (defun my-setup-swift-mode ()
    (rainbow-delimiters-mode 1)
    (my-turn-on-electric-pair-local-mode))
  (add-hook 'swift-mode-hook #'my-setup-swift-mode))

;;;----------------------------------------------------------------------------
;;; ggtags
;;;----------------------------------------------------------------------------
;; TODO: fix all the key bindings `ggtags-mode' clobbers. Like M-n, M-p.
(with-eval-after-load 'ggtags
  ;; doesn't work, added to windows path instead.
  ;; (when (eq my-curr-computer 'work-laptop)
  ;;   (add-to-list 'exec-path "C:/Users/mtz/programs/glo653wb/bin"))

  (when my-use-evil-p
    (evil-define-key 'normal ggtags-mode-map (kbd "M-.")
      #'ggtags-find-tag-dwim)
    ;; `evil-define-key' doesn't work here but `define-key' does?
    (define-key ggtags-mode-map (kbd "M-,") #'pop-tag-mark)))


;;;----------------------------------------------------------------------------
;;; follow-mode
;;;----------------------------------------------------------------------------
(defun my-follow-mode ()
  "Get the windows set up for `follow-mode', then turn it on."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (shrink-window-horizontally 10)
  (follow-mode 1))

;; (global-set-key (kbd "C-x f") #'my-follow-mode)

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
      (lispy-mode)))
  (add-hook 'clojure-mode-hook #'my-setup-clojure-mode))

;;;----------------------------------------------------------------------------
;;; cider
;;;----------------------------------------------------------------------------
(with-eval-after-load 'cider

  ;; when looking up docs with C-c C-c C-d, pop up the doc immediately.
  (setq cider-prompt-for-symbol nil)

  (when (eq my-curr-computer 'work-laptop)
    (add-to-list 'exec-path "C:/Users/mtz/.lein/bin")
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
      (lispy-mode)))
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
(when (>= emacs-major-version 25)
  (global-eldoc-mode 0))

;;;----------------------------------------------------------------------------
;;; scheme-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'scheme
  (defun my-setup-scheme ()
    (rainbow-delimiters-mode-enable)
    (enable-paredit-mode)
    (when my-use-lispy-p
      (lispy-mode)))
  (add-hook 'scheme-mode-hook #'my-setup-scheme))

;;;----------------------------------------------------------------------------
;;; geiser
;;;----------------------------------------------------------------------------
(with-eval-after-load 'geiser
  (when (eq my-curr-computer 'work-laptop)
    (setq geiser-default-implementation 'racket
          geiser-active-implementations '(racket))
    (add-to-list 'exec-path "C:/Users/mtz/programs/Racket")))

(with-eval-after-load 'geiser-repl
  (defun my-setup-geiser-repl ()
    (rainbow-delimiters-mode-enable)
    (enable-paredit-mode)
    (when my-use-lispy-p
      (lispy-mode)))
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
  ;; increase the timestamp ping a little.
  (setq auto-revert-interval 7)
  ;; default is t, but set anyway to guarantee
  (setq auto-revert-stop-on-user-input t)
  (setq global-auto-revert-non-file-buffers nil)
  (setq auto-revert-remote-files nil))

;; ;; reload buffer if it changes on disk outside emacs.
;; (global-auto-revert-mode t)


;;;----------------------------------------------------------------------------
;;; adoc-mode
;;;----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.asc\\'" . adoc-mode))

(with-eval-after-load 'adoc-mode
  (defun my-setup-adoc-mode ()
    ;; usually for reading books, so use word wrap.
    (visual-line-mode))
  (add-hook 'adoc-mode-hook #'my-setup-adoc-mode))

;;;----------------------------------------------------------------------------
;;; markdown-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'markdown-mode
  (defun my-setup-markdown-mode ()
    ;; usually for reading books, so use word wrap.
    (visual-line-mode))
  (add-hook 'markdown-mode-hook #'my-setup-markdown-mode))

;;;----------------------------------------------------------------------------
;;; typescript-mode
;;;----------------------------------------------------------------------------
(with-eval-after-load 'typescript-mode
  (setq typescript-indent-level my-indent-width)

  (defun my-setup-typescript-mode ()
    ;; (yas-minor-mode 1)
    (rainbow-delimiters-mode 1)
    (my-turn-on-electric-pair-local-mode))
  (add-hook 'typescript-mode-hook #'my-setup-typescript-mode))

;;;----------------------------------------------------------------------------
;;; tide
;;;----------------------------------------------------------------------------
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

  ;; for now, do not automatically turn on tide mode.
  ;; (add-hook 'typescript-mode-hook #'my-setup-tide-for-ts)
  )



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

  (defun my-line-numbers-on ()
    (interactive)
    (setq display-line-numbers t))
  (defun my-line-numbers-relative-on ()
    (interactive)
    (setq display-line-numbers 'relative))
  (defun my-line-numbers-off ()
    (interactive)
    (setq display-line-numbers nil))


  ;; line number display styles. lexically bound.
  (let ((styles '(relative absolute off))
        (curr nil)) ;; TODO: make curr buffer local.
    (defun my-line-numbers-cycle ()
      "Cycle line number display styles. relative, absolute, off.
Closure over `styles', `curr'."
      (interactive)
      (setq curr (car (or (cdr (memq curr styles))
                          styles)))
      (setq display-line-numbers (cond ((eq curr 'relative) 'relative)
                                       ((eq curr 'absolute) t)
                                       ((eq curr 'off) nil)))
      (message "line numbers: %s" curr)))

  (global-set-key (kbd "<f6>") #'my-line-numbers-cycle)

  ;; ;; Attempt at turning on relative line numbers in visual mode.
  ;; (when my-use-evil-p
  ;;   (let ((cache nil)) ; lexically bound `cache'.
  ;;     (defun my-vis-entry ()
  ;;       (setq cache display-line-numbers)
  ;;       (unless (or (eq display-line-numbers 'relative)
  ;;                   ;; don't do anything if visual is from expand-region.
  ;;                   (memq last-command
  ;;                         '(er/expand-region
  ;;                           er/contract-region
  ;;                           hydra-expand-region/er/expand-region
  ;;                           hydra-expand-region/er/contract-region)))
  ;;         (setq display-line-numbers 'relative)))

  ;;     (defun my-vis-exit ()
  ;;       (unless (eq display-line-numbers cache)
  ;;         (setq display-line-numbers cache))))

  ;;   (add-hook 'evil-visual-state-entry-hook #'my-vis-entry)
  ;;   (add-hook 'evil-visual-state-exit-hook #'my-vis-exit))
  )

;;;----------------------------------------------------------------------------
;;; powershell
;;;----------------------------------------------------------------------------
(with-eval-after-load 'powershell
  (defun my-setup-powershell-mode ()
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
(defun my-scroll-left ()
  "Scroll left."
  (interactive)
  (my-scroll-horizontal #'scroll-left))

(defun my-scroll-right ()
  "Scroll right."
  (interactive)
  (my-scroll-horizontal #'scroll-right))

(defun my-scroll-horizontal (scroll-fn)
  "Scroll 25% of the window width.
SCROLL-FN will be `my-scroll-left' or `my-scroll-right'."
  (let ((cols (floor (* 0.25 (window-total-width)))))
    (funcall scroll-fn cols)
    ;; TODO: ensure point is moved to a visible character to make subsequent
    ;; navigation/selection of visible text possible.
    ))


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
                          (name . "^\magit"))))))

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
(let ((pos nil) ; cache values for repeated M-r presses.
      (page-top nil)
      (page-bot nil)
      (page-mid nil))
  (defun my-cycle-line-position ()
    "Cycle between the mid, bot, and top positions in the buffer.
Similar to `move-to-window-line-top-bottom' but takes into account buffer sizes
smaller than the window height."
    (interactive)
    (let ((repeatp (eq this-command last-command)))
      (unless repeatp
        ;; calculate line numbers.
        (setq page-top (line-number-at-pos (window-start)))
        ;; TODO: figure out how much to subtract. 2 is needed most often.
        (setq page-bot (- (line-number-at-pos (window-end)) 2))
        (setq page-mid (+ page-top
                          (/ (1+ (- page-bot page-top)) 2))))
      (setq pos (my-next-cycle-pos (if repeatp pos nil)))
      ;; per emacs warnings, don't use `goto-line'.
      (goto-char (point-min))
      (forward-line (1- (cond ((eq pos 'top) page-top)
                              ((eq pos 'mid) page-mid)
                              ((eq pos 'bot) page-bot)))))))

(global-set-key (kbd "M-r") #'my-cycle-line-position)

;;;----------------------------------------------------------------------------
;;; tern
;;;----------------------------------------------------------------------------

;;;----------------------------------------------------------------------------
;;; company-tern
;;;----------------------------------------------------------------------------
(with-eval-after-load 'company
  (when has-nodejs-p
    (add-to-list 'company-backends 'company-tern)))

;;;----------------------------------------------------------------------------
;;; browse-kill-ring
;;;----------------------------------------------------------------------------
;; (global-set-key (kbd "M-y") #'browse-kill-ring) ; autoloaded fn


;;;----------------------------------------------------------------------------
;;; company-lsp
;;;----------------------------------------------------------------------------
(with-eval-after-load 'lsp-mode
  (require 'company-lsp)
  (push 'company-lsp company-backends))

;;;----------------------------------------------------------------------------
;;; cquery. Not an elisp project. Built separately.
;;; NOTE: put compile_commands.json in each project root (or symlink).
;;;----------------------------------------------------------------------------
(when (eq my-curr-computer 'wild-dog)
  (add-to-list 'exec-path "/home/mike/proj/cquery/build/release/bin"))

;;;----------------------------------------------------------------------------
;;; cquery. Melpa elisp package. (works with cquery binary above.)
;;;----------------------------------------------------------------------------
(with-eval-after-load 'cquery
  (when (eq my-curr-computer 'wild-dog)
    (setq cquery-executable
          "/home/mike/proj/cquery/build/release/bin/cquery")))

(when (eq my-curr-computer 'wild-dog)
  (defun my-setup-cquery ()
    ;; autoload for `lsp-cquery-enable' is broken so just require the
    ;; `cquery' library to make it available.
    (require 'cquery)
    (lsp-cquery-enable))
  ;; turn on cquery automatically.  But might go wonky if
  ;; compile_commands.json is not in the project root.
  (add-hook 'c-mode-common-hook #'my-setup-cquery))


;;;----------------------------------------------------------------------------
;;; Indium (formerly Jade). Javascript IDE.
;;;----------------------------------------------------------------------------
;; NOTE: must install server with the following command.
;;       npm install -g indium
;; NOTE: must install package dependency `websocket'. Currently getting it from
;;       melpa.
(add-to-list 'load-path "~/.emacs.d/notElpa/Indium") ; git submodule
(autoload 'indium-connect "indium-interaction" nil t)
;; (with-eval-after-load 'indium
;;   )

;;;----------------------------------------------------------------------------
;;; ispell
;;;----------------------------------------------------------------------------
(with-eval-after-load 'ispell
  ;; NOTE: instructions to set up hunspell on windows:
  ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2014-04/msg00030.html
  (when (eq my-curr-computer 'work-laptop)
    (add-to-list 'exec-path
                 "C:/Users/mtz/programs/hunspell-1.3.2-3-w32-bin/bin")
    (setq ispell-program-name (locate-file "hunspell"
                                           exec-path
                                           exec-suffixes
                                           'file-executable-p))))


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
;;; MISC options. Keep this at the bottom
;;;----------------------------------------------------------------------------
(defun my-load-everything-for-pdump ()
  "Load libraries so they will be part of the saved image from pdumper.
For faster subsequent start up."
  (interactive)
  ;; measure time spent loading libs.
  (let ((start (float-time)))
    ;; my libs
    (my-handle-weird-theme-setups)
    (load "my-hydras") ;; TODO: provide a feature so i can use `require'.
    (require 'my-rand)

    ;; 3rd party libs.
    (require 'company)
    (require 'swiper)
    (require 'ivy)
    (require 'counsel)
    (require 'flx)
    (require 'ido)
    (require 'smex)
    (require 'eshell)
    (require 'expand-region)
    (require 'ibuffer)
    (require 'org)
    (require 'org-agenda)
    (require 'calendar)
    (require 'slime)
    (require 'eww)
    (require 'js)
    (require 'js2-mode)
    (require 'js2-highlight-vars)
    (require 'json-mode)
    (require 'cc-mode)
    (require 'json-mode)
    (require 'num3-mode)
    (require 'powershell)
    (require 'leerzeichen)
    (require 'fill-column-indicator)
    (require 'hydra)
    (require 'avy)
    (require 'lispy)
    (require 'elisp-slime-nav)
    (require 'electric-spacing)
    (require 'highlight-tail)
    (require 'highlight-indent-guides)
    (require 'smart-tabs-mode)
    (require 'lua-mode)
    (require 'typescript-mode)
    (require 'erc)
    (require 'erc-hl-nicks)
    (require 'flycheck)
    (require 'unkillable-scratch)
    (require 'web-mode)
    (require 'css-mode)
    (require 'htmlize)
    (require 'rainbow-mode)
    (require 'paredit)
    (provide 'sql)
    (require 'sql-indent)
    (require 'yasnippet)
    (require 'ispell)
    (require 'grep)
    (require 'bookmark)
    (require 'mode-on-region)
    (require 'darkroom)
    (require 'ediff)
    (require 'vimrc-mode)
    (require 'vc)
    (require 'ace-window)
    (require 'ace-link)
    (require 'dired)
    (require 'nxml-mode)
    ;; (require 'magit) ; exclude magit until hook performance is resolved.
    ;; print elapsed time
    (message
     (format "Finished loading libs. elapsed seconds: %f"
             (time-to-seconds (time-subtract (float-time)
                                             start))))))

(defun my-list-holidays ()
  "List the major holidays."
  (interactive)
  (let ((year (string-to-number (format-time-string "%Y"))))
    (list-holidays year
                   year
                   (append holiday-general-holidays
                           holiday-christian-holidays))))

(when (eq system-type 'windows-nt)
  (defun my-find-file-by-name ()
    "Find files by name starting in current directory."
    (interactive)
    (let ((compile-command "dir /b/s "))
      ;; #'shell-command
      ;; #'grep
      (call-interactively #'compile)))

  (when my-use-evil-p
    (evil-leader/set-key "h" #'my-find-file-by-name)))

(defun my-indent-defun ()
  "Indent the function the cursor is inside."
  (interactive)
  (mark-defun)
  (call-interactively #'indent-region))
(define-key prog-mode-map (kbd "C-c <tab>") #'my-indent-defun)

(defun my-insert-ruler ()
  "Insert text to visually measure width in the buffer.  By columns."
  ;; TODO: make it recursive, to handle 1000's, millions, etc.
  (interactive)
  (let* ((default-cols 110)
         (cols (if (equal current-prefix-arg '(4))
                   (read-number "cols: " default-cols)
                 default-cols))
         (secs-10 (/ cols 10))
         (remainder-10 (mod cols 10))
         (secs-100 (/ cols 100)))
    ;; the 100's
    (cl-loop for s from 1 to secs-100
             do
             (cl-loop for i from 1 to 100
                      do
                      (if (< i 100)
                          (insert " ")
                        (insert (format "%d" (mod s 100))))))
    (when (> secs-100 0)
      (insert "\n"))
    ;; the 10's
    (cl-loop for s from 1 to secs-10
             do
             (cl-loop for i from 1 to 10
                      do
                      (if (< i 10)
                          (insert " ")
                        (insert (format "%d" (mod s 10))))))
    (when (> secs-10 0)
      (insert "\n"))
    ;; the 1's
    (cl-loop repeat secs-10
             do
             (insert "1234567890"))
    (cl-loop for i from 1 to remainder-10
             do
             (insert (format "%d" i)))))

(defun my-goto-line (line-num)
  "Docs say don't use `goto-line' in Lisp code.
Making my own using the recommended alternative.
LINE-NUM = line to goto."
  (goto-char (point-min))
  (forward-line (1- line-num)))

(defun my-longest-line ()
  "Find the longest line in the buffer and jump to it.
It is slow in large buffers."
  (interactive)
  (let ((end (progn
               (goto-char (point-max)) ; (end-of-buffer)
               (line-number-at-pos)))
        (start 1)
        (biggest-col 0)
        (at-line 0))
    (cl-loop for line from start to end
             do
             (my-goto-line line)
             (move-end-of-line nil)
             (let ((col (current-column)))
               (when (> col biggest-col)
                 (setq biggest-col col)
                 (setq at-line line))))
    (my-goto-line at-line)
    (message (format "biggest col: %d\n line: %d"
                     biggest-col
                     at-line))))

(when (or (> emacs-major-version 25)
          (and (= emacs-major-version 25)
               (>= emacs-minor-version 2)))
  ;; Disable the weird quote characters used in help buffers in emacs 25.
  ;; var `use-default-font-for-symbols' was introduced in emacs 25.2 to disable
  ;; the bad behavior introduced in emacs 25.
  (setq use-default-font-for-symbols nil))

(cond ((eq my-curr-computer 'work-laptop)
       (setq find-function-C-source-directory
             "c:/users/mtz/scratch/emacs/src")))

(defun my-win-count ()
  "Calculate the number of windows in the current frame."
  (length (window-list))
  ;; (cl-loop for w being the windows of (selected-frame)
  ;;          sum 1)
  )

(defun find-shell (&optional shell-only)
  ;; from jwd630. https://www.reddit.com/r/emacs/comments/48opk1/eshell_and_why
  ;; _cant_i_convert_to_you/
  "Find end of shell buffer or create one by splitting the current window.
If shell is already displayed in current frame, delete other windows
in frame.  Stop displaying shell in all other windows.
SHELL-ONLY will be documented later."
  (interactive)
  (let* ((shellbuf (get-buffer "*shell*")))
    (if (or (eq (window-buffer) shellbuf) shell-only)
        (delete-other-windows)
      (when (eq 1 (count-windows))
        (split-window-vertically))
      (unless (eq (window-buffer) shellbuf)
        (other-window 1)))
    ;; un-display shell in other windows (on other devices)
    (and shellbuf
         (> (length (get-buffer-window-list shellbuf nil t)) 0)
         (replace-buffer-in-windows shellbuf)))
  (shell)
  (goto-char (point-max))
  (recenter -2))
(define-key global-map (kbd "C-c C-z") #'find-shell) ; mimic slime repl binding

(when (and nil   ;don't start server for now.
           ;;`server-start' doesn't seem to work on MS-windows?
           (eq system-type 'gnu/linux))
  (server-start))

;; prevents warnings where you must select encoding (like in `list-packages')
(prefer-coding-system 'utf-8)


(defun what-face (pos)
  "Prints the face at point.  POS = point."
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


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


;; scroll like vim when moving 1 line off screen with j/k.
;; has some weird rules about re-centering, but 100 is supposed to
;; not recenter. I had an issue with value 1 where if i held down
;; j to scroll, it would periodically recenter.
(setq scroll-conservatively 100)
;; maintain cursor location when scrolling
(setq scroll-preserve-screen-position nil)

;; scrolling performance increase?
;; see https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-moveme
;; nt-lag/28746
(setq auto-window-vscroll nil)

(progn ;;window navigation.
  (when my-use-evil-p
    (global-set-key (kbd "M-h") #'evil-window-left)
    (global-set-key (kbd "M-j") #'evil-window-down)
    (global-set-key (kbd "M-k") #'evil-window-up)
    (global-set-key (kbd "M-l") #'evil-window-right)))

;; cycle the buffers really fast. Not doing this anymore since these are error
;; handling shortcuts in some modes.
;; (global-set-key (kbd "M-n") #'next-buffer)
;; (global-set-key (kbd "M-p") #'previous-buffer)

;; delete spaces between words
(defun my-cycle-spacing ()
  "Call `cycle-spacing', using fast mode.
This deletes the space if there is only 1 space.  Otherwise it would do nothing
on the first call."
  (interactive)
  (cycle-spacing current-prefix-arg nil 'fast))
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

;; (setq-default column-number-mode 1) ; show/hide column # in mode line.
(when nil ;skip this for now
  (setq-default line-number-mode 0)) ; show/hide line # in mode line.
                                     ; or use fn what-line
;; do not display modes in the mode-line. They take up too much space.
;; Function `describe-mode' (kbd "C-h m") is better to see active modes anyway.
(setq mode-line-modes nil)
(when nil ;skip this for now. showing % and line number again.
  ;; hide the % of the buffer you are viewing. Used to set to nil, but that
  ;; broke `nyan-mode' which manipulates this variable.
  ;; TODO: look into this more to understand the format of this variable.
  (setq mode-line-position '((size-indication-mode nil)
                             (line-number-mode nil))))



;; Don't suggest keybinds in minibuffer.  It messes up functions that use the
;; minibuffer for output.  Instead use "C-h f func" to see the current keybind
;; of a function.
(setq suggest-key-bindings nil)

(progn
  ;; replacing position info in mode line with a function called on demand.
  ;; Bound to "g a".

  (defun my-what-line ()
    (interactive)
    (let ((start (point-min))
          (n (line-number-at-pos)))
      (if (= start 1)
          (format "L%d" n)
        (save-excursion
          (save-restriction
            (widen)
            (format "L%d (narrowed L%d)"
                    (+ n (line-number-at-pos start) -1) n))))))

  (defun my-what-position (&optional _detail)
    "Your position in space and time."
    (interactive "P")
    (let* ((pos (point))
           (total (buffer-size))
           (percent (if (> total 50000)
                        ;; Avoid overflow from multiplying by 100!
                        (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
                      (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1))))
           (line (my-what-line))
           (col (+ 1 (current-column))))
      (message "%s  %d%% %s C%d     %s"
               (buffer-name)
               percent line col
               (format-time-string "%-m-%-d-%Y %a %-I:%M%#p"))))
  (defalias 'my-what-time #'my-what-position)

  (when my-use-evil-p
    ;; (evil-define-key 'normal global-map (kbd "g a") #'my-what-position)
    (define-key evil-normal-state-map "ga" #'my-what-position)))






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
    (setq display-time-format "%-m/%-d %a %-I:%M%#p"))
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
;; (setq initial-major-mode #'fundamental-mode) ;;for faster startup.



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

;; Don't echo passwords when dealing with interactive programs
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)



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
;;; ms
;;;----------------------------------------------------------------------------
;; (defvar ms-cols 30)
;; (defvar ms-rows 25)
;; (defvar ms-map ())

;; ;; u f O *

;; (defun ms-init ()
;;   (setq ms-map ()) ;; clear
;;   (let ((times (* ms-cols ms-rows)))
;;     (dotimes (i times)
;;       (setq ms-map (cons 'u ms-map)))))

;; (defun ms-get-index (r c)
;;   (+ (* r ms-cols) c))

;; (defun ms-render-map (map)
;;   (dotimes (r ms-rows)
;;     (dotimes (c ms-cols)
;;       (insert (symbol-name (nth (ms-get-index r c)
;;                                 ms-map)))
;;       (insert " "))
;;     (insert "\n")))

;; ;; interactive testing
;; (ms-render-map ms-map)
;; (ms-init)
;; (dolist (x ms-map)
;;   (insert (symbol-name x))
;;   (insert " "))

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


(progn ;; JUMPrestore. restore values is set earlier for startup time.
  (setq file-name-handler-alist file-name-handler-alist-backup)
  (setq gc-cons-threshold gc-cons-threshold-backup)
  ;; unbind junk variables. Avoid namespace pollution.
  (makunbound 'file-name-handler-alist-backup)
  (makunbound 'gc-cons-threshold-backup))


;;; init.el ends here
