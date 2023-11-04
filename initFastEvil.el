(load "~/.emacs.d/initFast") ;; .el or .elc if available

;;;----------------------------------------------------------------------------
;;; load-path
;;;----------------------------------------------------------------------------
;; do this at the top becuase some packages like evil-leader require being
;; loaded before their parent depency
(push "~/.emacs.d/notElpa/" load-path)
(push "~/.emacs.d/notElpa/mine/" load-path)
(push "~/.emacs.d/notElpa/evil" load-path)
(push "~/.emacs.d/notElpa/evil-leader" load-path)
(push "~/.emacs.d/notElpa/rainbow-delimiters" load-path)
(push "~/.emacs.d/notElpa/paredit" load-path)
(push "~/.emacs.d/notElpa/avy" load-path)
(push "~/.emacs.d/notElpa/swiper" load-path)

;;;----------------------------------------------------------------------------
;;; Color theme stuff.
;;;----------------------------------------------------------------------------
(setq custom-theme-directory "~/.emacs.d/notElpa/themes/") ; color themes.
;; some themes require helper files so add themes dir to load-path.
(push custom-theme-directory load-path)
(push "~/.emacs.d/notElpa/themes/replace-colorthemes/" custom-theme-load-path)

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

(load-theme 'charcoal t)

;;;----------------------------------------------------------------------------
;;; rainbow-delimiters
;;;----------------------------------------------------------------------------
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
;;; ido
;;;----------------------------------------------------------------------------
(ido-mode 1)

;;;----------------------------------------------------------------------------
;;; Paredit
;;;----------------------------------------------------------------------------
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
;;; evil-leader
;;;----------------------------------------------------------------------------
;; NOTE: per docs, evil-leader must be enabled before evil mode.
(require 'evil-leader)
(global-evil-leader-mode)
;; leader keys
(evil-leader/set-leader ",")
(evil-leader/set-key "q" #'balance-windows)
(evil-leader/set-key "x" #'maximize-window)
(evil-leader/set-key "," #'delete-other-windows)
(evil-leader/set-key "d" #'delete-window)
(evil-leader/set-key "c" #'quit-window) ; buffer left alive
(evil-leader/set-key "v" #'evil-visual-block)
(evil-leader/set-key "b" #'switch-to-buffer)

;;;----------------------------------------------------------------------------
;;; evil
;;;----------------------------------------------------------------------------
(require 'evil)
(evil-mode)

;;;----------------------------------------------------------------------------
;;; key-chord
;;;----------------------------------------------------------------------------
(require 'key-chord)
(setq key-chord-two-keys-delay 0.2)
(setq key-chord-one-key-delay 0.4)
(key-chord-define evil-insert-state-map "fj" #'evil-normal-state)
(key-chord-define evil-visual-state-map "fj" #'evil-exit-visual-state)
(key-chord-mode 1)

;;;----------------------------------------------------------------------------
;;; avy
;;;----------------------------------------------------------------------------
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
(define-key evil-normal-state-map (kbd "SPC") #'avy-goto-word-1)
(define-key evil-motion-state-map (kbd "SPC") #'avy-goto-word-1)
(evil-leader/set-key "s" #'avy-goto-char-timer)

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
;;; swiper. ivy is (or at least was) bundled with swiper. git submodule
;;; ivy
;;; counsel -> provides extra features for completing some things.
;;;----------------------------------------------------------------------------
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

(when nil ;;my-use-ivy-p
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

  (when nil ;; my-use-ivy-explorer
    ;; turn on grid-style display for find-file
    (ivy-explorer-mode 1))

  (when nil
    ;; an optional 3rd party sorting/filtering for ivy.
    ;; will remember remember past selections during find-file, etc.
    (ivy-prescient-mode 1)
    (prescient-persist-mode))

  ;; use emacs bindings (not evil) in ivy-occur buffer
  (push '("^*ivy-occur" . emacs) evil-buffer-regexps)

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
  (when nil ;; my-use-ivy-p ;; GUARD. I use swiper even when using ido, so guard
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

(define-key evil-normal-state-map (kbd "s") #'swiper-isearch)