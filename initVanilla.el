;;; initVanilla.el --- My emacs config. -*- lexical-binding: t -*-
(load "~/.emacs.d/initFast") ;; .el or .elc if available


;;;----------------------------------------------------------------------------
;;; Packages. NOTE: with the conversion to git submodules, most of the old
;;; package code is moved to ~/.emacs.d/notElpa/mine/my-package-stuff.el
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpa/" load-path) ; stores elisp files that are
                                       ; not "packages".
(push "~/.emacs.d/notElpa/mine/" load-path)
(setq custom-theme-directory "~/.emacs.d/notElpa/themes/") ;color themes.
;; some themes require helper files so add themes dir to load-path.
(push custom-theme-directory load-path)

(push "~/.emacs.d/notElpa/themes/replace-colorthemes/" custom-theme-load-path)


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


;;;----------------------------------------------------------------------------
;;; compat
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpaYolo/compat.el" load-path)


;;;----------------------------------------------------------------------------
;;; async
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpaYolo/emacs-async" load-path)
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
(push "~/.emacs.d/notElpaYolo/dash.el" load-path)
;; NOTE: contains dash-functional.el which is a separate pacakge on melpa.

;;;----------------------------------------------------------------------------
;; bug-hunter
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpaYolo/elisp-bug-hunter" load-path)
(autoload #'bug-hunter-file "bug-hunter" nil t)
(autoload #'bug-hunter-init-file "bug-hunter" nil t)

;;;----------------------------------------------------------------------------
;;; s
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpaYolo/s.el" load-path)
(autoload #'s-split "s" nil nil)
(autoload #'s-trim "s" nil nil)

;;;----------------------------------------------------------------------------
;;; f
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpaYolo/f.el" load-path)

;;;----------------------------------------------------------------------------
;;; num3-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpaYolo/num3-mode" load-path)
(autoload #'num3-mode "num3-mode" nil t)
(autoload #'global-num3-mode "num3-mode" nil t)

;;;----------------------------------------------------------------------------
;;; cursor
;;;----------------------------------------------------------------------------
(setq-default cursor-type '(bar . 2))
(blink-cursor-mode 1)

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
;;; rainbow-delimiters
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpaYolo/rainbow-delimiters" load-path)
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
;;; font
;;;----------------------------------------------------------------------------
;; (push
;;    ;; '(font . "-*-Menlo-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
;;    ;; '(font . "-*-JetBrains Mono NL-light-normal-normal-*-15-*-*-*-m-0-iso10646-1")
;;    ;; '(font . "-*-Ubuntu Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
;;    ;; '(font . "-*-Ubuntu Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
;;    ;; '(font . "-*-Iosevka-regular-normal-normal-*-17-*-*-*-m-0-iso10646-1")
;;    '(font . "-*-Iosevka-light-normal-normal-*-17-*-*-*-m-0-iso10646-1")
;;    default-frame-alist)

(load-theme 'charcoal t)

;;;----------------------------------------------------------------------------
;;; ido
;;;----------------------------------------------------------------------------
(ido-mode 1)

;;;----------------------------------------------------------------------------
;;; smex. used by ido, ivy
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpaYolo/smex" load-path)
(autoload #'smex "smex" nil t)
(autoload #'smex-major-mode-commands "smex" nil t)
(autoload #'smex-initialize "smex" nil t)

(with-eval-after-load 'smex
  ;; NOTE: smex is used for `counsel-M-x' too where this advice is not needed.
  ;; insert a hyphen - on space like in normal M-x
  (defadvice smex (around space-inserts-hyphen activate compile)
    (let ((ido-cannot-complete-command
           `(lambda ()
              (interactive)
              (if (string= " " (this-command-keys))
                  (insert ?-)
                (funcall ,ido-cannot-complete-command)))))
      ad-do-it)))

;; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
;;                   ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") #'smex)
(global-set-key (kbd "M-X") #'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x")
                #'execute-extended-command) ; rebind the original M-x command


;;;----------------------------------------------------------------------------
;;; Paredit
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpaYolo/paredit" load-path)
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

  ;; barf/slurp keybinds
  (define-key paredit-mode-map (kbd "C-9") #'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-0") #'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-M-9") #'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-M-0") #'paredit-backward-barf-sexp)
  ;; don't allow paredit to steal the M-r keybind.
  (define-key paredit-mode-map (kbd "M-r") #'move-to-window-line-top-bottom)
  ;; rebind `paredit-raise-sexp' to C-M-r
  (define-key paredit-mode-map (kbd "C-M-r") #'paredit-raise-sexp))


;;;----------------------------------------------------------------------------
;;; repeat
;;;----------------------------------------------------------------------------
(defvar repeat-exit-timeout) ;; silence byte computer warning

(with-eval-after-load 'repeat
  (setq repeat-exit-timeout nil)) ;; don't timeout

(when (fboundp #'repeat-mode) ;; emacs 28+
  (repeat-mode))

;;;----------------------------------------------------------------------------
;;; devil
;;;----------------------------------------------------------------------------
(autoload #'devil-mode "devil" nil t)
(autoload #'global-devil-mode "devil" nil t)

;;;----------------------------------------------------------------------------
;;; Avy
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpaYolo/avy" load-path)
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
;; C-SPC doesn't work in some terminals, so bind an alternative key.
(define-key isearch-mode-map (kbd "C-o") #'avy-isearch)
(define-key isearch-mode-map (kbd "C-'") #'avy-isearch) ; swiper convention
;; (define-key evil-normal-state-map (kbd "s") ; like vim sneak.
;;   #'avy-goto-char-2)
;; (define-key evil-motion-state-map (kbd "s") #'avy-goto-char-2)

(global-set-key (kbd "C-c a") #'avy-goto-word-1)
(global-set-key (kbd "C-c s") #'avy-goto-char-timer)


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
  )

;;;----------------------------------------------------------------------------
;;; erc
;;;----------------------------------------------------------------------------
(with-eval-after-load 'erc
  (setq erc-default-server "irc.libera.chat") ; formerly "irc.freenode.net"

  (let ((format "%-I:%M%#p")) ;; use 12 hour format. am/pm
    (setq erc-timestamp-format       format
          erc-timestamp-format-right format))

  (defvar my-erc-observe-client "/ctcp Username version")

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
(push "~/.emacs.d/notElpaYolo/erc-hl-nicks" load-path)
(autoload #'erc-hl-nicks-force-nick-face "erc-hl-nicks" nil nil)
(autoload #'erc-hl-nicks-alias-nick "erc-hl-nicks" nil nil)
(autoload #'erc-hl-nicks "erc-hl-nicks" nil nil)

(with-eval-after-load 'erc
  (require 'erc-hl-nicks)
  (add-to-list 'erc-modules 'hl-nicks t))

;;;----------------------------------------------------------------------------
;;; electric-pair
;;;----------------------------------------------------------------------------
(electric-pair-mode 1) ; global

;;;----------------------------------------------------------------------------
;;; markdown-mode
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpaYolo/markdown-mode" load-path)
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
;;; hydra
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpaYolo/hydra" load-path)
(autoload #'defhydra "hydra" nil t)

(autoload #'my-choose-hydra "my-hydras" nil t)
(autoload #'hydra-easyscroll/body "my-hydras" nil t)
(autoload #'my-hydra-smerge/body "my-hydras" nil t)

(with-eval-after-load 'hydra
  (setq hydra-is-helpful t)
  ;; don't use window for hints. It seems to lock things up.
  ;; And window switcher mode really gets messed up.
  (setq hydra-hint-display-type 'message) ;; (setq hydra-lv nil)
  )


;;;----------------------------------------------------------------------------
;;; expand-region
;;; https://github.com/magnars/expand-region.el
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpaYolo/expand-region.el" load-path)
(autoload #'er/expand-region "expand-region" nil t)
(autoload #'er/contract-region "expand-region-core" nil t)
(global-set-key (kbd "C-=") #'er/expand-region)
(global-set-key (kbd "C--") #'er/contract-region)

(autoload #'hydra-expand-region/body "my-hydras" nil t)
(global-set-key (kbd "C-c k") #'hydra-expand-region/body)


;;;----------------------------------------------------------------------------
;;; swiper. ivy is (or at least was) bundled with swiper. git submodule
;;; ivy
;;; counsel -> provides extra features for completing some things.
;;;----------------------------------------------------------------------------
(push "~/.emacs.d/notElpaYolo/swiper" load-path)
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

(global-set-key (kbd "C-c C-s") #'swiper-isearch)


(with-eval-after-load 'swiper
  ;; performance tweak to avoid visual-line-mode
  (setq swiper-use-visual-line-p #'ignore)

  (setq swiper-isearch-highlight-delay '(2 0.8))
  (define-key swiper-map (kbd "C-SPC") #'swiper-avy)
  (define-key swiper-all-map (kbd "C-SPC") #'swiper-avy))

(with-eval-after-load 'ivy
  (setq ivy-height 35)

  (when nil
    ;; an optional 3rd party sorting/filtering for ivy.
    ;; will remember remember past selections during find-file, etc.
    (ivy-prescient-mode 1)
    (prescient-persist-mode))

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
  (setq ivy-display-style 'fancy)

  ;; part of the "vertico suite" but works with ivy too.
  ;; (marginalia-mode)
  )


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
;;; misc
;;;----------------------------------------------------------------------------
