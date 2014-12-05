;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;;----------------------------------
;; Packages
;;----------------------------------
(require 'package)
;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
;; (unless package-archive-contents
;;   (package-refresh-contents))


;;--------------------------------------------------------------------
;; Evil mode
;;--------------------------------------------------------------------
(progn
  ;;prevent minibuffer spam when switching modes.
  ;;Cursor style/color is sufficient to determine mode.
  (setq evil-insert-state-message nil)
  (setq evil-emacs-state-message nil)
  (setq evil-visual-state-message nil)
  (setq evil-motion-state-message nil)
  (setq evil-normal-state-message nil)
  (setq evil-operator-state-message nil)
  (setq evil-replace-state-message nil)
  ;;Don't waste mode line space displaying mode.
  ;;Cursor style/color is sufficient to determine mode.
  (setq evil-mode-line-format nil))

(setq evil-default-cursor t)

(require 'evil)
(evil-mode 1)


;; When in visual mode: press $ to go to the end of the line minus the newline char.
(defadvice evil-end-of-line (after do-not-highlight-newline)
  "When in visual mode: press $ to go to the end of the line minus the newline char."
  (when (evil-visual-state-p)
    (evil-backward-char)))
(ad-activate 'evil-end-of-line)

;;----------------------------------
;; key-chord
;;----------------------------------
(setq key-chord-two-keys-delay 0.2) ;lower to reduce lag when pressing a key of a chord.
(setq key-chord-one-key-delay 0.4)

;; slows down movement when in visual mode and pressing "j" sine it is looking for the chord.
(require 'key-chord)
(key-chord-mode 1)
;; Define a key chord for escape so I don't have to press Esc or C-[
(let ((chord "fj"))
  ;;NOTE: fj lags downward movement with "j" in visual mode.
  ;;      If you hold down j it messes things up and the chord doesn't work.
  (key-chord-define evil-insert-state-map chord 'evil-normal-state)
  (key-chord-define evil-visual-state-map chord 'evil-exit-visual-state)
  ;; (key-chord-define evil-replace-state-map chord 'evil-normal-state)
  ;; (key-chord-define evil-operator-state-map chord func)
  ;; (key-chord-define evil-motion-state-map chord func))
  )

;(key-chord-define evil-insert-state-map "fj" 'evil-normal-state)
;(key-chord-define c++-mode-map ";;"  "\C-e;")


;;--------------------------------------------------------------------
;; Paredit
;;--------------------------------------------------------------------
;(add-to-list 'load-path "~/.emacs.d/paredit")
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'sly-mrepl-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'sql-mode-hook #'enable-paredit-mode)

;;-----------------------------------------------------------------------------
;; Misc options. Keep this at the bottom
;;-----------------------------------------------------------------------------
;; (let* ((alpha-focused 93)
;;        (alpha-unfocused 93)
;;        (alpha-lst (list alpha-focused alpha-unfocused)))
;;   (set-frame-parameter (selected-frame) 'alpha alpha-lst)
;;   (add-to-list 'default-frame-alist alpha-lst))



;; (progn ;; show time in mode line
;;   ;; disable process average display. Not sure why this is mixed in with time display.
;;   (setq display-time-default-load-average nil)
;;   (setq display-time-load-average nil)
;;   (setq display-time-load-average-threshold nil)
;;   ;;(setq-default display-time-day-and-date t)
;;   (display-time-mode 1)
;;   ;;(setq-default display-time-format "%-m/%-d %-I:%M%#p")
;;   (setq display-time-format "%-I:%M%#p"))



(setq inhibit-startup-message t)
;;(setq initial-scratch-message ";; Scratch buffer ;;\n\n\n\n")
(setq initial-scratch-message "\n\n\n\n\n")

(blink-cursor-mode 0)
(hl-line-mode 0)

(global-auto-revert-mode t) ;;reload buffer if it changes on disk outside emacs.

(setq-default line-spacing 0)
(global-linum-mode 0) ;show/hide line numbers
(setq-default column-number-mode t)
(setq-default line-number-mode t)
(setq-default indicate-empty-lines t) ;Like vim's tildes

(setq-default transient-mark-mode t)  ;show selected regions
(setq ring-bell-function 'ignore)

(progn ;;tab handling
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default indent-line-function 'insert-tab)
  )

(setq make-backup-files nil backup-inhibited t) ;No annoying backup files
(setq auto-save-default nil) ;No annoying auto-save files


;;disable annoying newline emacs automatically adds to the end of a file when saving.
(setq require-final-newline nil)
(setq mode-require-final-newline nil)