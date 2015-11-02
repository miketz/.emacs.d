;;; init.el --- My emacs config.

;;; Commentary:

;;; NOTES:
;;; Windows: registry entry for "open with" option:
;;;   Computer\HKEY_CLASSES_ROOT\Applications\runemacs.exe\shell\open\command

;;;----------------------------------
;;; Git incantations:
;;;----------------------------------
;;; Create a new github repo from an existing local repo:
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

;;; Remove all untracked files and directories.
;;;     git clean -fd

;;; Code:

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;;for compatibility with < 24.4 emacs, define `with-eval-after-load'
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
    (declare (indent 1) (debug t))
    `(eval-after-load ,file (lambda () ,@body))))

;;;--------------------------------------------------------------------
;;; Helper functions and macros
;;;--------------------------------------------------------------------
;; eval-when-compile used to prevent flycheck `cl' warning, but only works for macros?
(require 'cl)

(defmacro not-m (bool)
  "Similar to the not function.  But...
It must operate on a variable (not a value)
It mutates BOOL to the opposite value.
Useful to check a boolean state and toggle the state in 1 go."
  `(setq ,bool (not ,bool)))


(defun my-getAtIndex (i lst)
  "Return the element at I from LST."
  (cond
   ((null lst) nil)
   ((= i 0) (car lst))
   (t (my-getAtIndex (- i 1) (cdr lst)))))

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


;;;----------------------------------
;;; flags used for conditional execution
;;;----------------------------------
;; (display-graphic-p)
;; system-type
;; my-curr-computer

;; Keeping track of the various computers I use emacs on.
(defvar my-computers
  '(unknown        ; if my-curr-computer.txt does not exist
    work-laptop
    raspberry-pi
    utilite
    old-sony-vaio
    a-tower
    a-laptop-old
    a-laptop-faster
    leyna-laptop
    hp-tower-2009)
  "The computers I use Emacs on.  Specific configurations may be made for some computers.")

;; currently used computer. (manually set)
;; Used to conditionally set computer specific options, and paths.
;; NOTE: When setting up emacs on a new computer create file
;; ~/.emacs.d/my-curr-computer.txt
;; Then type the name of the symbol (see `my-computers') in the text file.
;; The file should contain 1 line and no whitespace. The text will be converted
;; to a symbol.
(defconst my-curr-computer
  (let ((curr-comp-file "~/.emacs.d/my-curr-computer.txt"))
    (if (file-exists-p curr-comp-file)
        (intern (my-get-string-from-file curr-comp-file))
      'unknown))
  "The computer running this Emacs.  Identified by a flag file.
Specific configs may be made based on the computer.")

;; (let ((curr-comp-file "~/.emacs.d/my-curr-computer.txt"))
;;   (if (file-exists-p curr-comp-file)
;;       (setq my-curr-computer (intern (my-get-string-from-file curr-comp-file)))
;;     (setq my-curr-computer 'unknown)))

;; TODO: look into a way to use auto-complete for some modes and company for
;;       others.



;;;----------------------------------
;;; globals
;;;----------------------------------
(defvar my-indent-width 4
  "An omni-variable serving 3 related purposes.
Becuase I want them to have same value.
-Preferred indent width for C-like languages.
-Number of spaces for a tab.
-How many columns to show for a 'real' tab.")

(defvar my-graphic-p (display-graphic-p)
  "Caching the result of `display-graphic-p' since it is used everywhere and won't change.")


;; TODO: look into a way to limit the values to evil, emacs, and cua. Like an enum. defcustom?
;; TODO: support cua.
(defvar my-ui-type 'evil
  "The user interface type I'm currently using.
Choices: evil emacs cua")

(defvar my-use-evil-p (eq my-ui-type 'evil)
  "Whether i'm using evil at the moment or not.
Just a convenience to avoid checks agaisnt `my-ui-type'.")


(defvar my-narrow-type 'ivy
  "The package I'm currenlty using for narrowing completions.
Choices: ivy ido helm")

;;TODO: make ivy pop-up it's window on the linux tty.
(defvar my-use-ivy-p (eq my-narrow-type 'ivy)
  "If I'm using ivy completion at the moment.
Just a convenience to avoid checks against `my-narrow-type'.")

(defvar my-use-helm-p (eq my-narrow-type 'helm)
  "Whether i'm using helm at the momnet or not.
Just a convenience to avoid checks against `my-narrow-type'.")
(defvar my-load-helm-on-init-p t
  "Whether to load helm during start up, or postpone till first attempted use.")

(defvar my-use-ido-p (eq my-narrow-type 'ido)
  "If I'm using ido at the moment.
Just a convenience to avoid checks against `my-narrow-type'.")

(defvar my-swoop-fn (if my-use-helm-p
                        #'helm-swoop
                      #'swiper)
  "Function for searching with an overview.")

;;;----------------------------------
;;; Packages
;;;----------------------------------
(add-to-list 'load-path "~/.emacs.d/notElpa/") ;stores elisp files that are not "packages".
(add-to-list 'load-path "~/.emacs.d/notElpa/mine/")
(setq custom-theme-directory "~/.emacs.d/notElpa/themes/") ;color themes.

;; TODO: specify if it should use elpa or melpa version of a package.
(defvar my-packages
  `(evil
    evil-leader
    ;;evil-escape
    ;;evil-matchit
    ;;evil-snipe
    ;;evil-god-state
    ;;evil-surround
    key-chord
    slime
    ;;sly
    paredit
    ;;paxedit
    ;;smartparens
    ;;redshank
    ;;auto-complete
    ;;ac-slime
    company
    company-web
    ;;company-quickhelp
    slime-company
    ;;ace-jump-mode
    ace-window
    ;;ace-jump-zap
    csharp-mode
    js2-mode
    js2-highlight-vars
    skewer-mode
    ;;ac-js2
    web-beautify
    helm
    helm-cmd-t
    helm-swoop
    ;;helm-git-grep ;search text of files.
    ;;helm-ls-git ;search for files. Similar to helm-cmd-t but with git.
    ;;icicles
    ;;projectile
    clippy
    yasnippet
    rainbow-delimiters
    rainbow-mode
    expand-region
    ;;multiple-cursors
    ;;omnisharp
    zenburn-theme
    hc-zenburn-theme
    niflheim-theme
    gruvbox-theme
    badger-theme
    monokai-theme
    tronesque-theme
    gandalf-theme
    color-theme-sanityinc-tomorrow
    grandshell-theme
    tommyh-theme
    ;;sublimity
    nyan-mode
    nyan-prompt
    ;;powerline
    ;;dired-details ;default feature in emacs 24.4+
    web-mode
    htmlize
    magit
    vimrc-mode
    sicp
    neotree
    num3-mode
    powershell
    irony
    company-irony
    flycheck-irony
    rtags
    aggressive-indent
    helm-w32-launcher
    sx
    leerzeichen
    sql-indent
    darkroom
    ;;vim-empty-lines-mode
    fill-column-indicator
    flycheck
    hydra
    ;;linum-relative
    guide-key
    unkillable-scratch
    speed-type
    bug-hunter
    swiper
    counsel
    color-identifiers-mode
    svg-mode-line-themes ;; only works on gnu/linux
    smex
    avy
    helm-flycheck
    lispy
    ;;helm-descbinds
    worf
    elisp-slime-nav
    electric-spacing
    ;;w3
    ;;w3m
    flymake-jslint
    nlinum
    ido-vertical-mode
    ido-ubiquitous
    flx-ido
    ov
    highlight-tail
    function-args)
  "Packages I use from elpa/melpa.")


(when (eq my-curr-computer 'work-laptop)
  (add-to-list 'my-packages 'omnisharp))


(require 'package)
(add-to-list 'package-archives
             ;;'("melpa" . "http://melpa.milkbox.net/packages/")
             '("melpa" . "http://melpa.org/packages/")
             t)
;; (cl-mapc 'add-to-list
;;          '(package-archives package-archives)
;;          '(("melpa" . "http://melpa.milkbox.net/packages/")
;;            ("marmalade" . "http://marmalade-repo.org/packages/"))
;;          '(t t))

;; (setq package-archives
;;       '(("gnu" . "http://elpa.gnu.org/packages/")
;;         ("melpa" . "http://melpa.milkbox.net/packages/")
;;         ("marmalade" . "http://marmalade-repo.org/packages/")))


;; (setq package-enable-at-startup nil)
(package-initialize) ;; activate all the packages (in particular autoloads)


(defun my-install-packages ()
  "Call this function on a new Emacs installation to install packages.
Installs packages in the list `my-packages'."
  (interactive)
  ;; fetch the list of packages available
  (unless package-archive-contents
    (package-refresh-contents))

  ;; install the missing packages
  (dolist (pkg my-packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(my-install-packages)

(defun my-upgrade-packages ()
  "Upgrade installed packages.
Code taken from http://oremacs.com/2015/03/20/managing-emacs-packages/"
  (interactive)
  (save-window-excursion
    (package-list-packages) ;; (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute t)))

(defun package-list-unaccounted-packages ()
  "Display unaccounted packages.
Like `package-list-packages', but only show packages that are installed and not
in `my-packages'.  Useful for cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x)
                    (and (not (memq x my-packages))
                         (not (package-built-in-p x))
                         (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))



;;;--------------------------------------------------------------------
;;; w32-send-sys codes. Operating system commands. MS Windows only.
;;;--------------------------------------------------------------------
(when (eq system-type 'windows-nt)
  (defvar my-w32-actions
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
      (hotkey . 61776)))
  (defun my-w32-get-code (action)
    "Get the numeric code from the action symbol."
    (cdr (assoc action my-w32-actions)))
  (defun my-w32-get-action (code)
    "Get the action symbol from the numeric code."
    (car (cl-rassoc code my-w32-actions)))
  (defun my-w32-run (action)
    "Executes a w32 action."
    (let ((code (my-w32-get-code action)))
      (w32-send-sys-command code))))

;;;----------------------------------
;;; key-chord
;;;----------------------------------
;; NOTE: "fj" chord slows down movement when in visual mode when pressing "j"
;;       since it is looking for the chord.

(when my-use-evil-p

  (key-chord-mode 1)                    ; autoloaded function

  (with-eval-after-load "key-chord"
    (setq key-chord-two-keys-delay 0.2) ;lower to reduce lag when pressing a key of a chord.
    (setq key-chord-one-key-delay 0.4))

  (with-eval-after-load "helm"
    ;; must be in eval-after-load so `helm-map' is defined
    (key-chord-define helm-map "fj" #'helm-keyboard-quit))

  (with-eval-after-load "ivy"
    (key-chord-define ivy-mode-map "fj" #'keyboard-escape-quit))

  (with-eval-after-load "evil"
    ;; must be in eval-after-load so key maps are defined.
    (key-chord-define evil-insert-state-map "fj" #'evil-normal-state)
    (key-chord-define evil-visual-state-map "fj" #'evil-exit-visual-state))

  (with-eval-after-load "lisp-mode"
    (when nil ;; trying lispy
      ;; TODO: make sure `hydra-paredit/body' still works after autoload changes.
      (key-chord-define lisp-mode-shared-map "df" #'hydra-paredit/body)))

;; (with-eval-after-load "smartparens"
;;   (load "~/.emacs.d/notElpa/mine/my-hydras.el")
;;   (key-chord-define smartparens-mode-map "df" #'hydra-smartparens/body))
  )


;;;----------------------------------
;;; cursor
;;;----------------------------------
(setq-default cursor-type '(bar . 2))
(custom-set-faces
 '(cursor ((t (:background "cyan")))))

(when my-use-evil-p
  (cl-defun my-cursor-stuff (&optional &key (color-emacs nil)
                                       (color-evil nil)
                                       (color-motion nil)) ;(color-motion "red")
    (interactive)
    (let ((args-emacs '())
          (args-evil '())
          (args-evil-motion '())) ;use same color throughout evil-mode, except for "motion" state.
      (when color-emacs (setq args-emacs (cons color-emacs args-emacs)))
      (when color-evil (setq args-evil (cons color-evil args-evil)))
      (when color-motion (setq args-evil-motion (cons color-motion args-evil-motion)))
      ;;bar hollow box hbar
      ;; (setq-default cursor-type (cons 'bar args-emacs)) ;;commenting this allows vim command mode : to have a bar cursor.
      (setq evil-emacs-state-cursor (cons 'bar args-emacs))
      (setq evil-normal-state-cursor (cons 'hollow args-evil))
      (setq evil-insert-state-cursor (cons 'bar args-evil))
      (setq evil-visual-state-cursor (cons 'hollow args-evil))
      (setq evil-operator-state-cursor (cons 'hollow args-evil))
      (setq evil-replace-state-cursor (cons 'hbar args-evil))
      ;;motion state is when some of evil is disabled (like in the function help and C-h-i pages).
      ;;give special color I know when it is not full-evil bindings.
      (setq evil-motion-state-cursor (cons 'box args-evil-motion))))

  (defun my-cursor-stuff-darkBg ()
    (interactive)
    (my-cursor-stuff :color-emacs "cyan" :color-evil "spring green"))

  (defun my-cursor-stuff-lightBg ()
    (interactive)
    (my-cursor-stuff :color-emacs "black" :color-evil "blue"))

  (when my-graphic-p
    (my-cursor-stuff)) ;set the default cursor style. colors not specified yet.
  )

;;;--------------------------------------------------------------------
;;; evil
;;;--------------------------------------------------------------------

;; keeping evil turned off by default now.
;; Enable evil explicitly for certain modes or file types.
;; (add-hook 'prog-mode-hook #'evil-local-mode)


(when my-use-evil-p
  (evil-mode 1)) ;; enable globally

(with-eval-after-load "evil"
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

    (when my-graphic-p
      ;;Don't waste mode line space displaying Evil-states.
      ;;Cursor style/color is sufficient to determine mode.
      ;;but if in a terminal without cursor styles then allow it to exist.
      (setq evil-mode-line-format nil)))


  (setq evil-default-cursor t)

  ;;(add-to-list 'load-path "~/.emacs.d/evil") ; only without ELPA/el-get
  ;; (require 'evil)
  (require 'evil-leader)
  (global-evil-leader-mode)
  ;; (evil-mode 1)


  ;;(define-key <keymap> key 'function)


  ;; Make j/k movement keys go up/down accross wrapped lines.
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>")
    'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>")
    'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>")
    'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>")
    'evil-previous-visual-line)
  ;;(setq-default evil-cross-lines t) ;; Make horizontal movement cross lines


  ;; make C-k kill to the end of hte line in insert mode.
  (define-key evil-insert-state-map (kbd "C-k") #'kill-line)


  ;; Map for Esc.
  ;; The key-chord package causes lag when a key of the chord is pressed.
  ;; So using the the built-in control chords which are fast. Better than the
  ;; awkward C-[ default.
  ;; (evil-define-key 'insert global-map (kbd "C-n") 'evil-normal-state)
  ;; (evil-define-key 'visual global-map (kbd "C-n") 'evil-exit-visual-state)


  ;; For visual mode: press $ to go to the end of the line minus the newline char.
  (defadvice evil-end-of-line (after do-not-highlight-newline)
    "When in visual mode, press $ to go to the end of the line minus the newline char."
    (when (evil-visual-state-p)
      (evil-backward-char)))
  (ad-activate 'evil-end-of-line)


  ;;leader keys
  (evil-leader/set-leader ",")
  (evil-leader/set-key "w" 'other-window)
  (evil-leader/set-key "q" 'balance-windows)
  (evil-leader/set-key "x" 'maximize-window)
  (evil-leader/set-key "," 'delete-other-windows)
  (evil-leader/set-key "d" 'delete-window)
  (evil-leader/set-key "k" 'kill-this-buffer)

  (evil-leader/set-key "<" (lambda ()   ;shrink window a little
                             (interactive)
                             (shrink-window-horizontally 15)))
  (evil-leader/set-key ">" (lambda ()   ;widen window a little
                             (interactive)
                             (enlarge-window-horizontally 15)))
  ;; (evil-leader/set-key "j" (lambda ()
  ;;                            (interactive)
  ;;                            (shrink-window 10)))
  ;; (evil-leader/set-key "k" (lambda ()
  ;;                            (interactive)
  ;;                            (enlarge-window 10)))

  ;;TODO: look into equivalent resizing for non-Windows machines.
  (when (eq system-type 'windows-nt)
    ;;`isFrameMax-my' can get out of sync. Hit <Leader>f a 2cd time to re-sync.
    (defvar isFrameMax-my nil)
    (evil-leader/set-key "f" (lambda ()
                               (interactive)
                               (let ((action (if (not-m isFrameMax-my)
                                                 'max
                                               'restore-curr-frame)))
                                 (my-w32-run action)))))

  ;;evalate lisp expression. Insert result on a new line.
  ;;(evil-leader/set-key "l" "a\C-j\C-u\C-x\C-e")

  (defun my-eval-last-sexp ()
    (interactive)
    (let ((val (eval (eval-sexp-add-defvars (preceding-sexp)) lexical-binding)))
      (prin1-to-string val)))

  (autoload 'pos-tip-show "pos-tip" nil t)
  (if my-graphic-p
      (progn
        ;;(require 'pos-tip)
        (evil-leader/set-key "e" (lambda ()
                                   (interactive)
                                   ;;(clippy-say (my-eval-last-sexp))
                                   (pos-tip-show (my-eval-last-sexp)))))
    (progn
      (evil-leader/set-key "e"
        (lambda ()
          (interactive)
          (save-excursion
            (evil-append 1)
            (default-indent-new-line)
            (eval-last-sexp t)         ; t to insert result in buffer.
            (evil-normal-state))))))

  (evil-leader/set-key "a" 'slime-eval-print-last-expression)
  (evil-leader/set-key "p" (lambda ()
                             (interactive)
                             (save-excursion ;don't move the point
                               (evil-append 1)
                               (slime-pprint-eval-last-expression)
                               (evil-normal-state)))))


;;;----------------------------------
;;; evil-snipe
;;;----------------------------------
;; (setq evil-snipe-enable-highlight nil)
;; (setq evil-snipe-enable-incremental-highlight nil)
;; (setq evil-snipe-scope 'visible)
;; (setq evil-snipe-repeat-scope 'visible)

;; (require 'evil-snipe)
;; (global-evil-snipe-mode 1)


;;;----------------------------------
;;; evil-god-state
;;;----------------------------------
;; (evil-define-key 'normal global-map (kbd "\\") 'evil-execute-in-god-state)
;; (evil-define-key 'motion global-map (kbd "\\") 'evil-execute-in-god-state)
;; (evil-define-key 'god global-map [escape] 'evil-god-state-bail)
;; ;;(evil-leader/set-key "," 'evil-execute-in-god-state)
;; ;;(global-set-key (kbd "C-c i") 'my-insert-date-string)

;; ;; (add-hook 'evil-god-start-hook (lambda () (diminish 'god-local-mode)))
;; ;; (add-hook 'evil-god-stop-hook (lambda () (diminish-undo 'god-local-mode)))

;;;----------------------------------
;;; evil-surround
;;;----------------------------------
;; (require 'evil-surround)
;; (global-evil-surround-mode 1)


;;;----------------------------------
;;; font
;;;----------------------------------
(when (or (eq my-curr-computer 'work-laptop)
          (eq my-curr-computer 'leyna-laptop))
  ;; configure default settings for fonts.
  (defvar my-default-font 'consolas)
  (defvar my-good-fonts '((inconsolata "Inconsolata" 135 normal) ;looks OK. fits a good number of lines on screen. flaky on bold. no itallic?
                          (consolas "Consolas" 125 normal) ; consolas is the best looking but fits fewer lines on screen.
                          (dejavu "DejaVu Sans Mono for Powerline" 120 normal) ;good, but looks a bit "tall"
                          (fixedsys "FixedSys" 120 normal)))

  (cl-defun my-set-font (&optional &key
                                   (sym nil) (height nil) (weight nil) (resize-window nil))
    "Sets the font.
If sym is not specified it uses the configured default set in `my-default-font'.
If height or weight are not specified then it uses the configured defaults in `my-good-fonts'.
Resize-window = t will adjust the window so the modeline fits on screen, etc."
    (unless sym (setq sym my-default-font))
    (let ((the-font (assoc sym my-good-fonts)))
      (unless height (setq height (third the-font)))
      (unless weight (setq weight (fourth the-font)))
      (let ((font-str (second the-font)))
        (custom-set-faces
         `(default ((t (:family ,font-str
                                :foundry "outline"
                                :slant normal
                                :weight ,weight
                                :height ,height
                                :width normal)))))))
    (when resize-window
      (my-w32-run 'restore-curr-frame)
      (my-w32-run 'max))))


(progn ;; inc/dec font size

  (defvar my-curr-font-size nil
    "Starts out unknown")

  (defun my-change-font-size (bigger-p)
    (interactive)
    (let* ((curr-size (if my-curr-font-size ;; use cached value if it's set
                          my-curr-font-size
                        (face-attribute 'default :height (selected-frame))))
           (step (if bigger-p 1 -1)) ;; TODO: calculate "threshold" step increment.
           (new-size (+ curr-size step)))

      (custom-set-faces
       `(default
          ((t (:height ,new-size)))))

      ;; must cache the new value becuase :height does not acutally inc until a threshold is breached.
      (setq my-curr-font-size new-size)

      (when (fboundp 'my-w32-run) ;; TODO: make it work on non-Windows machines.
        (my-w32-run 'restore-curr-frame)
        (my-w32-run 'max))

      (message (int-to-string new-size))))

  (global-set-key (kbd "M-=") (lambda ()
                                (interactive)
                                (my-change-font-size t)))
  (global-set-key (kbd "M--") (lambda ()
                                (interactive)
                                (my-change-font-size nil))))

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
     ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
   '(font-use-system-font t)
   '(tool-bar-mode nil))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "Fixed" :foundry "Misc"
                          :slant normal :weight normal :height 150 :width normal))))))



;;;------------------------------------------------------
;;; Color theme stuff.
;;;------------------------------------------------------
;;TODO: implement a way to undo color settings made outside the theme definition.
;;      use custom-theme-set-faces to set the colors/styles so they are rolled back
;;      when switching/disabling themes.
(defadvice load-theme (before disable-before-load)
  "Disable any loaded themes before enabling a new theme.
This prevents overlapping themes; something I would rarely want."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))
(ad-activate 'load-theme)


;; vim charcoal: hi Normal guifg=#ADC299 guibg=#35352B "*
(defvar mayan-smoke "#F4F4E8" "Background color from the Vim theme.")
(defvar my-charcoal "#35352B" "Expirimental dark background color.")



;;programmatically call a fucntion as if a prefix arg C-u was used.
;; (let ((current-prefix-arg '(4)))
;;   (call-interactively #'next-line))


(progn ; theme changing stuff.
  (autoload #'my-load-theme "my-load-theme" nil t)
  (autoload #'color "my-load-theme" nil t) ;trying to duplciate vim's :color interface
  (autoload #'my-cycle-theme "my-load-theme" nil t)
  (autoload #'my-cycle-light-bg "my-load-theme" nil t)

  (unless my-use-ivy-p
    (global-set-key (kbd "<f9>")
                    (lambda ()
                      (interactive)
                      ;;nil for no candidate limit. I want to scroll through all the themes.
                      (let ((helm-candidate-number-limit nil))
                        (call-interactively #'my-load-theme)))))
  (global-set-key (kbd "<f10>") #'my-cycle-theme)
  (global-set-key (kbd "<f12>") #'my-cycle-light-bg))

(defun my-load-theme-make-bold-like-zenburn (&optional theme)
  (interactive)
  (let ((zen-bold-faces '())
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
    (dolist (f (face-list))
      (when (eq (face-attribute f :weight frame)
                'bold)
        (add-to-list 'zen-bold-faces f)))
    ;; load theme and use zenburn's bolding.
    (load-theme theme t)
    (dolist (f zen-bold-faces)
      (set-face-attribute f nil :weight 'bold))))


(autoload #'my-rainbow-parens-dark-bg "my-bg-specific-colors" nil t)
(autoload #'my-rainbow-parens-dark-bg-bold "my-bg-specific-colors" nil t)
(autoload #'my-rainbow-parens-light-bg "my-bg-specific-colors" nil t)
(autoload #'my-rainbow-parens-light-bg2 "my-bg-specific-colors" nil t)


(autoload #'my-color-grandshell "my-color-grandshell" nil t)
(autoload #'my-color-zenburn "my-color-zenburn" nil t)
(autoload #'my-color-github "my-color-github" nil t)
(autoload #'my-color-badger "my-color-badger" nil t)
(autoload #'my-color-gruvbox "my-color-gruvbox" nil t)
(autoload #'my-color-monokai "my-color-monokai" nil t)
(autoload #'my-color-tommyh "my-color-tommyh" nil t)
(autoload #'my-color-default "my-color-default" nil t)
(autoload #'my-color-default-fancy "my-color-default-fancy" nil t)
(autoload #'my-color-gandalf "my-color-gandalf" nil t)
(autoload #'my-color-leuven "my-color-leuven" nil t)
(autoload #'my-color-dichromacy "my-color-dichromacy" nil t)
(autoload #'my-color-firebelly "my-color-firebelly" nil t)
(autoload #'my-color-molokai "my-color-molokai" nil t)

(when my-graphic-p ;; transparency stuff
  ;; TODO: auto load the transparency stuff

  ;; TODO: get this value on-the-fly rather than caching to avoid a
  ;; transparency "jump" if alpha var gets out of sync.
  (defvar my-curr-alpha 100
    "Starts out 100")

  ;; using `cl-defun' to allow `return-from'
  (cl-defun my-change-alpha (solider-p)
    (interactive)
    ;; exit early if can't increase or decrease further
    (when (or (and (= my-curr-alpha 100) solider-p)
              (and (= my-curr-alpha 0) (not solider-p)))
      (message (int-to-string my-curr-alpha))
      (return-from my-change-alpha))

    (let* ((step (if solider-p 1 -1)))
      (incf my-curr-alpha step)
      (set-frame-parameter (selected-frame) 'alpha `(,my-curr-alpha ,my-curr-alpha))
      (message (int-to-string my-curr-alpha))))

  (global-set-key (kbd "C-M-=") (lambda ()
                                (interactive)
                                (my-change-alpha t)))
  (global-set-key (kbd "C-M--") (lambda ()
                                (interactive)
                                (my-change-alpha nil))))

;;theme of the week and corresponding settings. This may change often.
(progn
  (cond
   ((or (eq my-curr-computer 'work-laptop)
        (eq my-curr-computer 'leyna-laptop)
        (eq my-curr-computer 'a-laptop-old))
    ;; (set-frame-font "-raster-Terminal-normal-normal-normal-mono-18-*-*-*-c-*-ms-oemlatin")
    (my-set-font :sym 'consolas
                 :height 125            ;'90 105 115 120 125
                 :weight 'normal)
    (when my-graphic-p
      (my-color-zenburn)))

   ((eq my-curr-computer 'raspberry-pi)
    (when my-graphic-p
      (my-color-zenburn)))

   ((eq my-curr-computer 'hp-tower-2009)
    (when my-graphic-p
      (custom-set-faces
       '(default ((t (:family "Droid Sans Mono" :foundry "unknown" :slant normal :weight normal :height 140 :width normal)))))
      (my-color-zenburn)))

   ((eq my-curr-computer 'a-laptop-faster)
    (custom-set-faces
     '(default ((t (:family "Source Code Pro"
                            :foundry "adobe"
                            :slant normal
                            :weight semi-bold
                            :height 120
                            :width normal))))
     ;; '(default ((t (:family "Inconsolata"
     ;;                        :foundry "unknown"
     ;;                        :slant normal
     ;;                        :weight normal
     ;;                        :height 155
     ;;                        :width normal))))
     )
    (when my-graphic-p ;this doens't return true for emacs daemon!
      (my-color-zenburn))))


  ;; (let ((a 92)) ;92
  ;;   (set-frame-parameter (selected-frame) 'alpha `(,a ,a)))

  ;; (require 'highlight-tail)
  ;; (setq highlight-tail-colors '(("dark cyan" . 0)
  ;;                               ("black" . 40)))
  ;; (setq highlight-tail-steps 40 ;80
  ;;       highlight-tail-timer 0.04;0.04
  ;;       )
  ;; (setq highlight-tail-posterior-type t) ;(setq highlight-tail-posterior-type 'const)
  ;; (highlight-tail-mode)
  ;; ;;(highlight-tail-reload)
  )


;;;---------------------------------------------
;;; Recursively byte-compile every .el file
;;;---------------------------------------------
;;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)



;;;---------------------------------------------
;;; sly
;;;---------------------------------------------
;; (setq my-use-sly nil)

;; (when my-use-sly
;;   (when (eq my-curr-computer 'work-laptop)
;;     (setq inferior-lisp-program "C:\\Users\\mtz\\programs\\ccl-1.10-windowsx86\\ccl\\wx86cl64")))

;;;---------------------------------------------
;;; SLIME
;;;---------------------------------------------
(require 'slime-autoloads)
(with-eval-after-load "slime"
  (slime-setup '(slime-fancy
                 slime-company
                 slime-banner
                 slime-indentation))
  (setq slime-complete-symbol*-fancy t)
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

  (when my-use-evil-p
    ;;(define-key slime-mode-map (kbd "M-.") 'slime-edit-definition) ;override evil's binding of M-. when using slime
    (evil-define-key 'normal slime-mode-map (kbd "M-.") 'slime-edit-definition) ;override evil's binding of M-. when using slime
    (evil-define-key 'normal slime-repl-mode-map (kbd "M-.") 'slime-edit-definition))

  ;;disable the banner header line in repl. TODO: get rid of the date string that replaces it too.
  (setq slime-header-line-p nil)
  ;; (require 's)
  ;; (setq slime-words-of-encouragement (let ((words '())) ;;hidden
  ;;                                      (dolist (w slime-words-of-encouragement)
  ;;                                        (when (s-contains? "REPL" w)
  ;;                                          (setq words (cons w words))))
  ;;                                      words))


  ;; redefine `slime-startup-message' to work how I want
  ;; (defun slime-startup-message ()
  ;;   (when (zerop (buffer-size))
  ;;     (let ((welcome (concat ";; "
  ;;                            (nth 5 slime-words-of-encouragement))))
  ;;       (if slime-startup-animation
  ;;           (animate-string welcome 0 0)
  ;;         (insert welcome)))))

  (progn
    (when (eq my-curr-computer 'work-laptop)
      (setq slime-default-lisp 'ccl
            slime-lisp-implementations '((ccl ("C:\\Users\\mtz\\programs\\ccl-1.10-windowsx86\\ccl\\wx86cl64"))
                                         (sbcl ("C:/Program Files/Steel Bank Common Lisp/1.2.15/sbcl.exe"))
                                         (clisp ("~/path/to/clisp-2.49/clisp" "-modern")))));clisp is just a fake example for now.
    (when (eq my-curr-computer 'utilite)
      (setq slime-default-lisp 'ccl
            slime-lisp-implementations '((ccl ("armcl")))))

    (when (eq my-curr-computer 'hp-tower-2009)
      (setq slime-default-lisp 'sbcl
            slime-lisp-implementations '((ccl ("~/software/ccl/lx86cl64"))
                                         (sbcl ("/usr/bin/sbcl")))))

    (when (eq my-curr-computer 'a-laptop-faster)
      (setq slime-default-lisp 'ccl
            slime-lisp-implementations '((ccl ("~/Downloads/ccl/lx86cl"))
                                         (sbcl ("/usr/bin/sbcl")))))

    ;; when on a computer with SLIME set up
    (when (or (eq my-curr-computer 'work-laptop)
              (eq my-curr-computer 'utilite)
              (eq my-curr-computer 'a-laptop-faster))
      ;; connect lisp buffers to SLIME automatically.
      (add-hook 'slime-mode-hook ;not sure why this works, since it's a hook on slime-mode which I thought would need to be hooked on lisp-mode-hook???
                (lambda ()
                  (unless (slime-connected-p)
                    (save-excursion (slime)))))))

  ;; (add-hook 'slime-mode-hook #'lispy-mode)
  ;; (add-hook 'slime-repl-mode-hook #'lispy-mode)
  (add-hook 'slime-repl-mode-hook
            (lambda ()
              ;;turn off line numbers in the repl
              (linum-mode 0)
              ;;there's always a trailing space at repl prompt. Don't highlight it.
              (setq show-trailing-whitespace nil)
              ;;aggressive-indent moves SLIME's comments in the REPL. Turn it off.
              (when (fboundp 'aggressive-indent-mode)
                (aggressive-indent-mode 0))))

  ;;(define-key slime-mode-map (kbd "<tab>") #'slime-indent-and-complete-symbol)
  (when my-use-evil-p
    (evil-define-key 'insert slime-mode-map (kbd "<tab>") #'slime-indent-and-complete-symbol)
    (evil-leader/set-key "r" (lambda ()
                               (interactive)
                               (save-excursion
                                 (evil-append 1)
                                 (let ((string (slime-last-expression)))
                                   (evil-normal-state)
                                   (slime-eval-async
                                    `(swank:eval-and-grab-output ,string)
                                    (lambda (result)
                                      (cl-destructuring-bind (output value) result
                                        (pos-tip-show value)
                                        ;;(push-mark)
                                        ;;(insert output value)
                                        ))))))))

  (when (eq my-curr-computer 'work-laptop)
    ;; use local hyperspec
    (setq common-lisp-hyperspec-root "file:///C:/users/mtz/AppData/Roaming/CommonLispHyperSpec/HyperSpec/"))

  (defun my-view-hyperspec ()
    (interactive)
    (let ((browse-url-browser-function #'eww-browse-url))
      (slime-documentation-lookup)))
  (define-key slime-mode-map (kbd "C-c C-d h") #'my-view-hyperspec)
  (when my-use-ivy-p
    (define-key slime-mode-map (kbd "C-M-i") #'counsel-cl)))


;;;---------------------------------------------
;;; redshank
;;;---------------------------------------------
;; (require 'redshank-loader)
;; (eval-after-load "redshank-loader"
;;   `(redshank-setup '(lisp-mode-hook
;;                      slime-repl-mode-hook) t))
;; ;;   (eval-after-load "redshank"
;; ;;     '(progn ...redefine keys, etc....))


;;;---------------------------------------------
;;; company
;;;---------------------------------------------
;;company mode is breaking emacs 24.3. Works OK in 24.4
;; (require 'company)
(add-hook 'after-init-hook 'global-company-mode) ;all buffers

(with-eval-after-load "company"
  (when my-use-evil-p
    (define-key company-mode-map (kbd "C-SPC") 'company-complete)) ;C-Space like Visual Studio
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "<tab>") 'company-complete) ;expands till -. Completes after that.
  (define-key company-active-map (kbd "C-v") #'company-next-page) ;would be default, but my other keymap killed this
  (define-key company-active-map (kbd "M-v") #'company-previous-page) ;default, but set just in case.
  (define-key company-active-map (kbd "M-<") ;go to first candidate
    (lambda ()
      (interactive)
      (let ((company-selection-wrap-around nil))
        (company-set-selection 0))))
  (define-key company-active-map (kbd "M->") ;go to last candidate
    (lambda ()
      (interactive)
      (let ((company-selection-wrap-around nil))
        (company-set-selection company-candidates-length))))

  ;;(setq company-tooltip-minimum-width 60) ;avoids changing width as visislbe candidates change.
  ;; (add-hook 'company-completion-started-hook
  ;;           (lambda ()
  ;;             (interactive)
  ;;             (setq company-tooltip-minimum-width
  ;;                   (apply #'max
  ;;                          (mapcar #'length
  ;;                                  company-candidates)))))

  (setq company-idle-delay nil)         ;disable automatic completion
  (setq company-minimum-prefix-length 3) ;but if automatic is on, don't fire until 3 chars.
  (setq company-tooltip-limit 20)        ;popup more suggestions.

  (when nil
    (progn ;work-around issue where `fill-column-indicator' moves suggestion box.
      ;;TODO: handle for auto-complete too. It's on emacs.stackexchange.
      (defvar-local company-fci-mode-on-p nil)

      (defun company-turn-off-fci (&rest ignore)
        (when (boundp 'fci-mode)
          (setq company-fci-mode-on-p fci-mode)
          (when fci-mode (fci-mode -1))))

      (defun company-maybe-turn-on-fci (&rest ignore)
        (when company-fci-mode-on-p (fci-mode 1)))

      (add-hook 'company-completion-started-hook 'company-turn-off-fci)
      (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
      (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))))



;;;---------------------------------------------
;;; company-web
;;;---------------------------------------------
(with-eval-after-load "web-mode"
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim)

  (add-hook 'web-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '(company-web-html company-files))))

  (define-key web-mode-map (kbd "C-SPC") 'company-web-html))


;;;---------------------------------------------
;;; company-quickhelp. not using messes up keybinds
;;;---------------------------------------------
;; (setq company-quickhelp-idle-delay 0.5)
;; (company-quickhelp-mode 1)

;;;---------------------------------------------
;;; slime-company
;;;---------------------------------------------
;; this is set in the slime section

;;;---------------------------------------------
;;; Auto-complete
;;;---------------------------------------------
;; ;;use auto-complete in emacs 24.3 and below
;; (when t
;;   ;; (and (<= emacs-major-version 24)
;;   ;;      (<= emacs-minor-version 3))
;;   (require 'auto-complete-config)
;;   (ac-config-default)

;;   (define-key ac-mode-map (kbd "C-SPC") 'auto-complete) ;C-Space like Visual Studio
;;   (setq ac-auto-start nil) ;don't automatically pop up completions. Use C-Space

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

;; ;;;------------------------------------------------
;; ;;; ac-slime. integrates auto-complete with slime.
;; ;;;------------------------------------------------
;; (when t
;;   (require 'ac-slime)
;;   ;;(add-hook 'slime-mode-hook 'set-up-slime-ac)
;;   ;;(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;;   (add-hook 'slime-mode-hook (lambda ()
;;                                (interactive)
;;                                (set-up-slime-ac t))) ;t for fuzzy matching
;;   (add-hook 'slime-repl-mode-hook (lambda ()
;;                                     (interactive)
;;                                     (set-up-slime-ac t))) ;t for fuzzy matching
;;   (eval-after-load "auto-complete"
;;     '(add-to-list 'ac-modes 'slime-repl-mode))
;;   )

;;;--------------------------------------------------
;;; turn on lisp-mode when editing file .stumpwmrc
;;;--------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.stumpwmrc\\'" . lisp-mode))

;;;---------------------------------------------
;;; Org mode
;;;---------------------------------------------
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(with-eval-after-load "org"
  (setq org-startup-indented t)
  (setq org-log-done t) ;make timestamp when flagging something done with C-c C-t
  ;; new keyword for tasks put on hold
  (add-to-list 'org-todo-keywords '(sequence "HOLD") t)

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)  ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  (when (eq my-curr-computer 'work-laptop)
    (setq org-agenda-files '("C:\\Users\\mtz\\TODO.org")))

  ;; org mode steals M-h keybind. reclaim it. TODO: rebind org fn to a key.
  (when my-use-evil-p
   (define-key org-mode-map (kbd "M-h") 'evil-window-left)))

;;;-----------------------------------------
;;; worf. key shortcuts for org-mode
;;;-----------------------------------------


;;;---------------------------------------------
;;; csharp-mode
;;;---------------------------------------------
;;(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


;; `electric-pair-mode' is in `c-mode-common-hook' already.
;; (add-hook 'csharp-mode-hook #'electric-pair-mode)

(add-hook #'csharp-mode-hook (lambda () (yas-minor-mode 1)))


;;;-------------------------
;;; js2-hightlight-vars.el
;;;-------------------------
;; (require 'js2-highlight-vars)

;;;------------------------------------
;;; align-let.el in ~/.emacs.d/notElpa
;;;------------------------------------
;; (autoload 'align-let "align-let" nil t)
;; (let ((key (kbd "C-c C-a")))
;;   (define-key lisp-mode-map key #'align-let)
;;   (define-key emacs-lisp-mode-map key #'align-let)
;;   (define-key lisp-interaction-mode-map key #'align-let))

(with-eval-after-load "lisp-mode"
  (autoload 'align-let "align-let" nil t)
  (define-key lisp-mode-shared-map (kbd "C-c C-a") #'align-let))

;; (let ((abadf 333)
;;       (x     222)
;;       (yy    44)))

;; (setq aaaaaaaaaaaaa 2
;;       b             3
;;       cc            433
;;       d             "hello there")

;;;---------------------------------------------
;;; js2-mode
;;;---------------------------------------------
;;(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(with-eval-after-load "js2-mode"
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
  (setq js2-basic-offset my-indent-width) ;; default is 4, but set explicilty anyway.

  (setq js2-mode-show-strict-warnings t)

  ;; (setq js2-include-jslint-globals t) ;; recognize vars in the global comment for jslint. Doesn't work?

  ;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
  ;; add any symbols to a buffer-local var of acceptable global vars
  ;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
  ;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
  ;; you can;t have a symbol called "someName:false"
  ;; (add-hook 'js2-post-parse-callbacks
  ;;           (lambda ()
  ;;             (when (> (buffer-size) 0)
  ;;               (let ((btext (replace-regexp-in-string
  ;;                             ": *true" " "
  ;;                             (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
  ;;                 (mapc (apply-partially 'add-to-list 'js2-additional-externs)
  ;;                       (split-string
  ;;                        (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
  ;;                        " *, *" t))))))

  ;;js2 steals M-j keybinding by default. Reclaim it.
  (when my-use-evil-p
    (define-key js2-mode-map (kbd "M-j") #'evil-window-down)
    (evil-define-key 'normal js2-mode-map (kbd "M-n") 'js2-next-error)
    (evil-define-key 'normal js2-mode-map (kbd "M-p") (lambda ()
                                                        (interactive)
                                                        (js2-next-error -1))))
  (define-key js2-mode-map (kbd "C-c e") #'js2-display-error-list)

  ;; (defhydra hydra-js2-flycheck ()
  ;;   "js2 flycheck"
  ;;   ("n" flycheck-next-error)
  ;;   ("p" flycheck-previous-error)
  ;;   ("q" nil))
  ;; (evil-define-key 'normal js2-mode-map (kbd "C-c l") #'hydra-js2-flycheck/body)

  (defhydra my-hydra-js2-flymake (:color amaranth)
    "jslint:flymake: "
    ("n" flymake-goto-next-error)
    ("p" flymake-goto-prev-error)
    ("v" flymake-popup-current-error-menu)
    ("C-g" nil nil)
    ("q" nil))
  (when my-use-evil-p
   (evil-define-key 'normal js2-mode-map (kbd "C-c l") #'my-hydra-js2-flymake/body))

  ;; (evil-define-key 'normal js2-mode-map (kbd "C-c h") #'my-hydra-hs/body)

  ;; Add parsing of jslint output in compilation mode
  ;; (add-to-list 'compilation-error-regexp-alist-alist '(jslint "^\\(.*\\): line \\([0-9]+\\), col \\([0-9]+\\), " 1 2 3))
  ;; (add-to-list 'compilation-error-regexp-alist 'jslint)

  (add-hook 'js2-mode-hook
            (lambda ()
              (js2-highlight-unused-variables-mode t)
              ;; replace ambiguous name "Javascript-IDE" with "js2"
              (setq mode-name "js2")
              ;; (setq-default js2-global-externs "jQuery $")
              ;; (setq-default js2-indent-on-enter-key t)
              ;; (add-to-list 'js2-ecma-262-externs "setTimeout")

              ;; (when (featurep 'js2-highlight-vars)
              ;;   (js2-highlight-vars-mode))

              ;;(js2-imenu-extras-mode)

              (electric-pair-mode 1)
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
                ;; ;; bind M-n, M-p to use flymake functions istead of js2 functions
                ;; (evil-define-key 'normal js2-mode-map (kbd "M-n") #'flymake-goto-next-error)
                ;; (evil-define-key 'normal js2-mode-map (kbd "M-p") #'flymake-goto-prev-error)
                ;; (evil-define-key 'normal js2-mode-map (kbd "C-c m") #'flymake-popup-current-error-menu)
                )
              ;; regex so M-x complile can parse jslint output.
              ;; (set (make-local-variable 'compilation-error-regexp-alist)
              ;;      '(("^[ \t]*\\([A-Za-z.0-9_: \\-]+\\)(\\([0-9]+\\)[,]\\( *[0-9]+\\))\\( Microsoft JScript runtime error\\| JSLINT\\): \\(.+\\)$" 1 2 3)))
              ;; show a greek lambda for function
              (setq prettify-symbols-alist '(("function" . 955)))
              ;; collapse/show sections of code
              (hs-minor-mode 1))))

;;;--------------------------------------
;;; web-beautify
;;;--------------------------------------
;; Depends on external programs: nodejs, js-beatify
;; So only use on computers with the depedencies set up.
(when (eq my-curr-computer 'work-laptop)
  ;; (with-eval-after-load "web-beautify"
  ;;   (add-to-list 'web-beautify-args "3")
  ;;   (add-to-list 'web-beautify-args "-m")
  ;;   )
  (with-eval-after-load "js2-mode"
    (define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
  ;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
  (with-eval-after-load "js"
    (define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

  (with-eval-after-load "json-mode"
    (define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

  ;; (with-eval-after-load "sgml-mode"
  ;;   (define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

  ;; (with-eval-after-load "css-mode"
  ;;   (define-key css-mode-map (kbd "C-c b") 'web-beautify-css))
  )
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

;;;--------------------
;;; ac-js2
;;;--------------------
;; (when nil
;;   (add-hook 'js2-mode-hook 'ac-js2-mode)
;;   (setq ac-js2-evaluate-calls t);requires connection to browser with (run-skewer)
;;   ;;(add-to-list 'ac-js2-external-libraries "path/to/lib/library.js") ;external lib example
;;   )

;;;--------------------
;;; nxml
;;;--------------------
(with-eval-after-load "nxml-mode"
  (setq nxml-slash-auto-complete-flag t) ;auto-insert when typing </
  (when my-use-evil-p
    ;; reclaim key M-h which nxml stole for`nxml-mark-paragraph'
    (define-key nxml-mode-map (kbd "M-h") #'evil-window-left))

  ;; TODO make a minor mode for elmah logs. Inherit from nxml-mode.
  (defun my-elmah-format-xml ()
    "Insert newlines at the escape codes in elmah's XML error message.
To make it human readable."
    (interactive)
    (save-excursion
      (beginning-of-buffer) ;; go to beggining of buffer
      (let ((pos 0))
        (while (not (null pos))
          (setq pos (search-forward "&#xD;&#xA;" nil t))
          (when (not (null pos))
            (newline)))))))

;;;--------------------
;;; Helm
;;;--------------------
;;(add-to-list 'load-path "~/.emacs.d/helm")

(when (and my-use-helm-p
           (not (eq my-curr-computer 'raspberry-pi)) ;helm is a little slow on a raspberry pi.
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
    ;; TODO: use `helm-dabbrev', once i figure out what's preventing it from finding candidates.
    ;; the standard emacs `dabbrev-expand' works fine. `hippie-expand' works too.
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
    (setq helm-display-header-line nil) ;save 1 line for rarely used header.
    (set-face-attribute 'helm-source-header nil :height 1.0) ;don't make source seperators bigger than needed
    ;; (progn
    ;;   ;;helm-autoresize-mode hides other windows, and dynamically adjusts the
    ;;   ;;helm window size as you type.
    ;;   (helm-autoresize-mode 1)
    ;;   ;;disable the dynamic size adjustment.
    ;;   (setq helm-autoresize-max-height 35)
    ;;   (setq helm-autoresize-min-height 35))
    ;; ;;prevents the windown hiding from `helm-autoresize-mode'. And when there are
    ;; ;;lots of split windows, keep the popup at the current window.
    ;; (setq helm-split-window-in-side-p t)
    )


  ;; (progn ;;Trick from tuhdo. Move helm input to top of helm buffer, hide in echo area.
  ;;   (setq helm-echo-input-in-header-line t)
  ;;   (setq helm-split-window-in-side-p t) ;;optoinally put helm buffer inside current buffer.

  ;;   (defun helm-hide-minibuffer-maybe ()
  ;;     (when (with-helm-buffer helm-echo-input-in-header-line)
  ;;       (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
  ;;         (overlay-put ov 'window (selected-window))
  ;;         (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
  ;;                                 `(:background ,bg-color :foreground ,bg-color)))
  ;;         (setq-local cursor-type nil))))

  ;;   (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe))


  ;; (helm-mode 1) ;helm-selection everywhere like when using M-x. putting this in eval-after-load to decrease start up time a bit.

  ;;(global-set-key (kbd "C-x c!")   #'helm-calcul-expression)
  ;;(global-set-key (kbd "C-x c:")   #'helm-eval-expression-with-eldoc)
  ;;(define-key helm-map (kbd "M-o") #'helm-previous-source)

  ;;(global-set-key (kbd "M-s s")   #'helm-ag)

  ) ;;end helm eval-after-load


;;;------------------------------------------------------------------------------
;;; helm-descbinds
;;;------------------------------------------------------------------------------
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


;;;----------------------------------
;;; helm-company
;;;----------------------------------
;; (eval-after-load 'company
;;   '(progn
;;      (define-key company-mode-map (kbd "C-SPC") 'helm-company)
;;      (define-key company-active-map (kbd "C-SPC") 'helm-company)))

;;;----------------------------------
;;; evil-escape
;;;----------------------------------
;;(evil-escape-mode 1)



;;;--------------------
;;; helm-git-grep (makes emacs crash on windows)
;;;--------------------
;; (when my-run-sys-specific
;;   (defadvice helm-git-grep (after turn-off-activeupdate)
;;     "Turn off active update in MS-windows. It can't handle grep processes spawning on each keystroke."
;;     (helm-toggle-suspend-update))
;;   (ad-activate 'helm-git-grep))

;; (require 'helm-git-grep)
;; (define-key helm-git-grep-mode-map (kbd "C-u") 'helm-toggle-suspend-update)
;; (evil-leader/set-key "g" 'helm-git-grep)


;;;--------------------
;;; vc-git-grep. This is better for ms-windows since it can't handle helm-git-grep's many processes.
;;; Also grepping is a pretty heavy weight opperation so I prefer to set up the search inputs first,
;;; select the top folder, etc instead of searching in real-time for each key press.
;;;--------------------
;; defined in ~/emacs.d/notElpa/mine/my-vc-git-grep.el
(autoload 'my-vc-git-grep "my-vc-git-grep" nil t)
(when my-use-evil-p
 (evil-leader/set-key "g" #'my-vc-git-grep))

;;;--------------------
;;; helm-swoop
;;;--------------------
;; (autoload 'helm-swoop "helm-swoop" nil t)

;; invoke with M-x for now. binding avy to the "s" key
(when my-use-helm-p
  ;; helm needs to be initalized or else helm-swoop won't work. (it doens't `require' everything it needs)
  (when my-use-evil-p
   (define-key evil-normal-state-map (kbd "s") #'helm-swoop)))

;; (global-set-key (kbd "C-c s") 'helm-swoop)
;; (global-set-key (kbd "C-c C-s") 'helm-swoop)
;;(evil-leader/set-key "s" 'helm-multi-swoop-all)

(with-eval-after-load "helm-swoop"
  ;;Prevent swoop from grabbing the text under the cursor. I rarely want that.
  (setq helm-swoop-pre-input-function
        (lambda () ""))

  ;; Change keybinds to whatever you like :)
  ;; (global-set-key (kbd "M-i") 'helm-swoop)
  ;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
  ;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
  ;; (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

  ;; When doing isearch, hand the word over to helm-swoop
  ;; (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

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


;;;---------------------
;;; sublimity
;;;---------------------
;; (require 'sublimity)
;; (require 'sublimity-scroll)
;;map is annoying;;(require 'sublimity-map)

;;(setq sublimity-scroll-drift-length 1) ;(sublimity-scroll-weight 4)

;;(sublimity-global-mode)


;;;---------------------
;;; Clippy. pop-up help
;;;---------------------
(when my-use-evil-p
  (evil-leader/set-key "c" 'clippy-describe-function)
  (evil-leader/set-key "v" 'clippy-describe-variable))
;; (evil-leader/set-key "n"
;;   (lambda ()
;;     (interactive)
;;     (clippy-say "It looks like you want to know more about the [search function]. Please use the [search function] to find out more about the [search function]. The [search function] provides access to more a more detailed description than can be provided in tool-tip help." t)))

;; (evil-leader/set-key "n"
;;   (lambda (variable)
;;     (interactive (list (variable-at-point)))
;;     (if (not (null variable))
;;         (clippy-say (format "It looks like you want to know more about [%s]. Please use the [search function] to find out more about [%s]. The [search function] provides access to more a more detailed description than can be provided in tool-tip help."
;;                             variable
;;                             variable)
;;                     t))))



;;;-----------------------------------------------------------
;;; ido
;;; ido-veritical-mode
;;; ido-ubiquitous
;;; flx-ido
;;; smex (built on ido)
;;;-----------------------------------------------------------
(when my-use-ido-p
  ;;use swiper on "s" even when using ido.
  (when my-use-evil-p
   (define-key evil-normal-state-map (kbd "s") #'swiper))

  (setq ido-everywhere t)
  (ido-mode t) ;;autoloaded function. turn on ido.

  (when my-use-evil-p
   (evil-leader/set-key "b" #'ido-switch-buffer))

  ;; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
  ;;                   ; when Smex is auto-initialized on its first run.
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c M-x") 'execute-extended-command) ; rebind the original M-x command

  ;; moving ido-ubiquitous out of (with-eval-after-load "ido") becuase some
  ;; other modes load ido, inadvertently turning on ido-ubiquitous even
  ;; when i'm not using ido.
  (ido-ubiquitous-mode 1))

(with-eval-after-load "ido"
  ;; 3rd party extension to ido. Display vertically like swiper.
  (ido-vertical-mode 1) ;; invokes with-eval-after-load "ido-vertical-mode"
  ;; (flx-ido-mode 1) ;; invokes (with-eval-after-load "flx-ido")

  ;; insert a hypen - on space like in normal M-x
  (defadvice ido-switch-buffer (around space-inserts-hyphen activate compile)
    (let ((ido-cannot-complete-command
           `(lambda ()
              (interactive)
              (if (string= " " (this-command-keys))
                  (insert ?-)
                (funcall ,ido-cannot-complete-command)))))
      ad-do-it)))

(with-eval-after-load "flx-ido"
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(with-eval-after-load "ido-vertical-mode"
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-vertical-show-count t))

(with-eval-after-load "smex"
  ;; insert a hypen - on space like in normal M-x
  (defadvice smex (around space-inserts-hyphen activate compile)
    (let ((ido-cannot-complete-command `(lambda ()
                                          (interactive)
                                          (if (string= " " (this-command-keys))
                                              (insert ?-)
                                            (funcall ,ido-cannot-complete-command)))))
      ad-do-it)))

;;;--------------------
;;; Yasnippet
;;;--------------------
;; ;;(add-to-list 'load-path "~/.emacs.d/yasnippet")

;;(require 'yasnippet)
;;(yas-global-mode 0)
;; (autoload 'yasnippet "yasnippet" "yasnippet mode" t)
(autoload #'snippet-mode "yasnippet" "A mode for editing yasnippets" t)

(with-eval-after-load "yasnippet"
  ;; so custom snippets are not overwritten when updating from melpa.
  (yas-load-directory "~/.emacs.d/snippets")

  ;; (setq yas-snippet-dirs
  ;;     `("~/.emacs.d/snippets"                 ;; personal snippets
  ;;       ;; "/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
  ;;       ;; "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
  ;;       ,yas-installed-snippets-dir              ;; the default collection
  ;;       ))
  (setq yas/triggers-in-field nil) ;Enable/disable trigger of a sub-snippet while in a snippet.
  (defun my-yas-handle-param (param-str
                              sep-char
                              fn-deco
                              fn-fix-first
                              fn-fix-last)
    "Does something special for each paramter in a snippet."
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


;;;--------------------
;;; cc-mode
;;;--------------------
(with-eval-after-load "cc-mode"
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
  (setq-default c-basic-offset my-indent-width) ;tab width
  (setq-default c-electric-flag t)

  ;; `which-function-mode' is OK, but it turns on the mode globally for all buffers which is annoying.
  ;; And if you the functions fit on screen then it's just wasted modeline space.
  ;; As an alternative use `beginning-of-defun' C-M-a to jump to the function name.
  ;; Then `evil-jump-backward' C-o to jump back to where you were.
  ;;(eval-after-load 'cc-mode 'which-function-mode)

  (add-hook 'c-mode-common-hook
            (lambda ()
              (yas-minor-mode 1)
              ;;(which-function-mode);;displays function at cursor in the mode-line. But can be annoying.
              (electric-pair-mode 1)

              ;; highlight escapes in the printf format string.
              (highlight-regexp "%[[:alpha:]]\\|\\\\[[:alpha:]]")

              ;;(flycheck-mode 1)
              ;; (electric-spacing-mode 1)
              ))


  (add-hook 'c-initialization-hook
            (lambda ()
              ;;TODO: fill this up
              ;; hook that runs 1 time.
              ;; equivalent to using eval-after-load???
              ))

  ;; (defun my-make-CR-do-indent ()
  ;;   (define-key c-mode-base-map "\C-m" 'c-context-line-break))
  ;; (add-hook 'c-initialization-hook 'my-make-CR-do-indent)
  )


;;;------------------
;;; Dired
;;;------------------
(with-eval-after-load "dired" ; dired -> dired.el in `load-path'
  (setq-default dired-isearch-filenames t) ;search file names only in Dired.
  (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))
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
;;   ;; use the same buffer when navigating folders in dired. And use the same buffer when opening a file in dired.
;;   (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
;;   (define-key dired-mode-map (kbd "^")
;;     (lambda ()
;;       (interactive)
;;       (find-alternate-file "..")))) ;; was dired-up-directory

;;;---------------------
;;; dired-details
;;;---------------------
;; ;;allows collapsing the file details with "(" and ")" in emacs <= 24.3
;; (require 'dired-details)
;; (dired-details-install)



;;;--------------------------------------------------------------------
;;; sql-mode
;;;--------------------------------------------------------------------
(with-eval-after-load "sql"
  (autoload #'s-trim "my-misc" nil nil) ; used by snippet "ins"
  (add-hook 'sql-mode-hook #'electric-pair-mode)
  (add-hook #'sql-mode-hook
            (lambda ()
              (yas-minor-mode 1)
              ;;electric-indent doesn't work very well with T-sql.
              ;;use C-j for newline and indent.
              (when (fboundp 'electric-indent-local-mode)
                (electric-indent-local-mode -1))

              ;; turn off indent when you press "o" in evil. Buffer local
              (when my-use-evil-p
                (setq evil-auto-indent nil))))

  ;; ;;experiment to handle annoying indents.
  ;; (when nil
  ;;   (defun my-delete-region (start end)
  ;;     (interactive "r")
  ;;     (delete-region)
  ;;     (deactivate-mark))
  ;;   ;; augment the backspace to handle the annoying indentation sql-mode gives.
  ;;   (evil-define-key 'insert sql-mode-map (kbd "<backspace>")
  ;;     (lambda ()
  ;;       (interactive)
  ;;       (set-mark-command nil)
  ;;       (evil-backward-word-begin)
  ;;       (evil-forward-word-end)
  ;;       (evil-forward-char)
  ;;       (call-interactively #'my-delete-region))))
  )


;;;--------------------------------------------------------------------
;;; rainbow-delimiters
;;;--------------------------------------------------------------------
;; (require 'rainbow-delimiters)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
(add-hook 'sql-mode-hook #'rainbow-delimiters-mode)
(add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)
;;(add-hook 'sly-mrepl-mode-hook #'rainbow-delimiters-mode) ;(lambda () (rainbow-delimiters-mode-turn-on)))
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;;(global-rainbow-delimiters-mode)

;;;--------------------------------------------------------------------
;;; rainbow-mode
;;;--------------------------------------------------------------------
;;(rainbow-mode)

;;;--------------------------------------------------------------------
;;; Expand-region
;;; https://github.com/magnars/expand-region.el
;;;--------------------------------------------------------------------
;;(require 'expand-region)
;; (autoload 'expand-region "expand-region" "expand region" t)
(global-set-key (kbd "C-=") #'er/expand-region)
(global-set-key (kbd "C--") #'er/contract-region)

;;;--------------------------------------------------------------------
;;; mulitple-cursors
;;; https://github.com/magnars/multiple-cursors.el
;;;--------------------------------------------------------------------
;;(require 'multiple-cursors)
;;(global-set-key (kbd "C--") 'mc/edit-lines)

;;;--------------------------------------------------------------------
;;; Paredit
;;;--------------------------------------------------------------------
;;(add-to-list 'load-path "~/.emacs.d/paredit")
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
;;(add-hook 'sly-mrepl-mode-hook (lambda () (paredit-mode +1)))
;;(add-hook 'sql-mode-hook #'enable-paredit-mode)


(with-eval-after-load "paredit"
  ;; Stop SLIME's REPL from grabbing DEL,
  ;; which is annoying when backspacing over a '('
  (defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

  (when (eq my-ui-type 'emacs)
    ;; reclaim the M-r binding to move cursor to middle, high, low
    (define-key paredit-mode-map (kbd "M-r") #'move-to-window-line-top-bottom)
    ;; rebind paredits raise function
    (define-key paredit-mode-map (kbd "C-c M-r") #'paredit-raise-sexp)))

;; ;;key maps
;; (global-set-key (kbd "C-9") 'paredit-backward-slurp-sexp)
;; (global-set-key (kbd "C-0") 'paredit-forward-slurp-sexp)
;; (global-set-key (kbd "C-M-9") 'paredit-backward-barf-sexp)
;; (global-set-key (kbd "C-M-0") 'paredit-forward-barf-sexp)


;;;--------------------------
;;; smartparens
;;;--------------------------
;;(require 'smartparens-config)


;;;--------------------------
;;; Omnisharp
;;;--------------------------
(when (and t ;nil ;; turn off omnisharp for the moment.
           (eq my-curr-computer 'work-laptop))

  (add-hook 'csharp-mode-hook 'omnisharp-mode) ;;turn on automatically for C# files.

  (with-eval-after-load "omnisharp"

    ;; TODO; figure out why the M-. keybind is not setting. M-, seems to work.
    (define-key omnisharp-mode-map (kbd "M-.") #'omnisharp-go-to-definition)
    (define-key omnisharp-mode-map (kbd "M-,") #'pop-tag-mark)

    (when my-use-evil-p
      ;; Example evil-mode config

      ;; (evil-define-key 'insert omnisharp-mode-map
      ;;   (kbd "C-SPC") 'omnisharp-auto-complete);C-Space like Visual Studio
      ;;(define-key omnisharp-mode-map (kbd "C-SPC") 'omnisharp-auto-complete) ;C-Space like Visual Studio

      (evil-define-key 'normal omnisharp-mode-map
        (kbd "g u") 'omnisharp-find-usages)
      (evil-define-key 'normal omnisharp-mode-map
        (kbd "g o") 'omnisharp-go-to-definition)
      (evil-define-key 'normal omnisharp-mode-map
        (kbd "g i") 'omnisharp-find-implementations)
      (evil-define-key 'normal omnisharp-mode-map
        (kbd "g r") 'omnisharp-run-code-action-refactoring)
      (evil-define-key 'normal omnisharp-mode-map
        (kbd "g f") 'omnisharp-fix-code-issue-at-point)
      (evil-define-key 'normal omnisharp-mode-map
        (kbd "g R") 'omnisharp-rename)

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

    (setq omnisharp--curl-executable-path "C:\\Users\\mtz\\programs\\curl-7.37.0-win64\\bin\\curl.exe")
    (setq omnisharp-server-executable-path "C:\\Users\\mtz\\programs\\OmniSharpServer\\OmniSharp\\bin\\Debug\\OmniSharp.exe")
    (setq omnisharp--windows-curl-tmp-file-path "C:\\Users\\mtz\\omnisharp-curl-tmp.cs") ;windows doesn't like the C:\ root folder
    (setq omnisharp-host "http://localhost:2000/") ;(setq omnisharp-host "http://localhost:2000/")
                                        ;(setq omnisharp-curl "curl.exe")
                                        ;`(:command ,omnisharp--curl-executable-path)

    (let ((i-am-using-omnisharp t))
      (when i-am-using-omnisharp
        (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-omnisharp))))

    (setq omnisharp-company-do-template-completion nil) ;tab completion of paramters. acts weird
    (setq omnisharp-company-ignore-case t)

    (defun my-start-omnisharp-server (sln)
      "Starts omnisharp server with the correct cmd line string."
      (interactive)
      (start-process-shell-command
       "Omni-Server"
       (get-buffer-create "*Omni-Server*")
       (concat omnisharp-server-executable-path " -p 2000 -s " sln)))))


;;;--------------------
;;; nyan-mode
;;;--------------------
;;(nyan-mode)
;;(setq nyan-wavy-trail nil)
;;(nyan-start-animation)

;;;--------------------
;;; nyan-prompt
;;;--------------------
;;(add-hook 'eshell-load-hook 'nyan-prompt-enable)

;;;--------------------
;;; powerline  NOTE: powerline has an error on start up in emacs 24.4.50.1, even when all code is commented out. Deleting the elpa folder for now.
;;;--------------------
;;(powerline-default-theme)
;;(powerline-center-theme)


;;;--------------------
;;; Avy
;;;--------------------
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
;; (define-key evil-normal-state-map (kbd "s") 'avy-goto-char-2) ;like vim sneak.
;; (define-key evil-motion-state-map (kbd "s") 'avy-goto-char-2)
(when my-use-evil-p
  (define-key evil-normal-state-map (kbd "SPC") 'avy-goto-word-1)
  (define-key evil-motion-state-map (kbd "SPC") 'avy-goto-word-1))

(with-eval-after-load "avy"
  ;; make keys like ace-jump. Lots of letters means more likey to need only 1 overlay char.
  (setq avy-keys (nconc (loop for i from ?a to ?z collect i)
                        (loop for i from ?A to ?Z collect i)))
  (setq avy-style 'at-full) ;;options (pre at at-full post)
  (setq avy-background nil) ;eye is already focused on the jump point so no need to gray background.
  (setq avy-all-windows t)  ;allow jumps between windows.
  (setq avy-case-fold-search t)         ;case insenstive

  ;; (defun my-avy-goto-line ()
  ;;   (interactive)
  ;;   ;; use the default keys for line jumps
  ;;   (let ((avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
  ;;     (avy-goto-line)))
  ;; (global-set-key (kbd "M-g g") #'my-avy-goto-line)
  ;; (global-set-key (kbd "M-g M-g") #'my-avy-goto-line)

  (defun my-avy-goto-char-3 (char1 char2 char3 &optional arg)
    "Copied `avy-goto-char-2' but reading 3 chars. Feels like too many."
    (interactive (list (read-char "char 1: ")
                       (read-char "char 2: ")
                       (read-char "char 3: ")
                       current-prefix-arg))
    (avy--with-avy-keys avy-goto-char-3
      (avy--generic-jump
       (regexp-quote (string char1 char2 char3))
       arg
       avy-style))))


;;;--------------------
;;; ace-link
;;;--------------------
(when (or (eq my-ui-type 'emacs)
          (eq my-ui-type 'cua))
  ;; TODO: put this in the eval-after-loads. This line slows start up by
  ;; forcing a load of file `ace-link'
  (ace-link-setup-default))

(when my-use-evil-p
  (with-eval-after-load "info"
    (define-key Info-mode-map (kbd "o") #'ace-link-info))
  (with-eval-after-load "compile"
    (define-key compilation-mode-map (kbd "o") #'ace-link-compilation))
  (with-eval-after-load "help-mode"
    (define-key help-mode-map (kbd "o") #'ace-link-help))
  (with-eval-after-load "woman"
    ;; TODO: test this binding
    (define-key woman-mode-map (kbd "o") #'ace-link-woman))
  (with-eval-after-load "eww"
    (evil-define-key 'normal eww-link-keymap (kbd "o") 'ace-link-eww)
    (evil-define-key 'normal eww-mode-map (kbd "o") 'ace-link-eww))
  (with-eval-after-load 'cus-edit
    ;; TODO: test this binding
    (evil-define-key 'normal custom-mode-map (kbd "o") 'ace-link-custom)))

;;;--------------------
;;; Ace jump mode
;;;--------------------
;; ;; (add-to-list 'load-path "/full/path/where/ace-jump-mode.el/in/")

;; ;; your eye is already focused on the jump point so no need to gray background.
;; (setq ace-jump-mode-gray-background nil)
;; (autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
;; (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

;;;--------------------
;;; ace-window
;;;--------------------
(unless (eq my-ui-type 'emacs)
  ;; this is the emacs "copy" keybind, so only don't steal it when using emacs
  ;; bindings.
  (global-set-key (kbd "M-w") 'ace-window))

(with-eval-after-load "ace-window"
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ;;home row
  (setq aw-background nil) ;; don't dim the background
  )

;;;--------------------
;;; ace-jump-zap
;;;--------------------
;; (global-set-key (kbd "M-z") 'ace-jump-zap-to-char)

;;;--------------------
;;; clang-format
;;;--------------------
;; rarely use `clang-format', so commenting it out for now.
;; (when (eq my-curr-computer 'work-laptop)
;;   (load "C:\\Users\\mtz\\programs\\LLVM\\share\\clang\\clang-format.el")
;;   ;;(global-set-key [C-M-tab] 'clang-format-region)
;;   (global-set-key (kbd "C-c f") 'clang-format-region)
;;   (global-set-key (kbd "C-c b") 'clang-format-buffer))

;;;--------------------
;;; irony
;;;--------------------
(when (or (eq my-curr-computer 'work-laptop)
          (eq my-curr-computer 'hp-tower-2009)) ;TODO: set up on more machines.
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

;;;-------------------------------
;;; company-irony
;;;-------------------------------
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))

;; ;; (optional) adds CC special commands to `company-begin-commands' in order to
;; ;; trigger completion at interesting places, such as after scope operator
;; ;;     std::|
;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;;-------------------------------
;;; Load projects
;;;-------------------------------
(when (eq my-curr-computer 'work-laptop)
  (let ((lisp-file "my-proj-work-laptop" ))
    (autoload #'proj-safetyweb lisp-file nil t)
    (autoload #'proj-rsims lisp-file nil t)
    (autoload #'proj-daily-diff lisp-file nil t)
    (autoload #'proj-db-safety lisp-file nil t)
    (autoload #'proj-trighist lisp-file nil t)
    (autoload #'proj-safetyweb lisp-file nil t)
    (autoload #'proj-emacs lisp-file nil t)
    (autoload #'proj-cl lisp-file nil t)
    (autoload #'proj-imgtag lisp-file nil t)
    (autoload #'proj-cpp lisp-file nil t)
    (autoload #'proj-pcl lisp-file nil t))

  ;;quick load of c:\users\mtz
  (when my-use-evil-p
    (evil-leader/set-key "1" (lambda ()
                               (interactive)
                               (dired "C:\\Users\\mtz")))

    ;;quick load of c:\users\mtz\proj\ecp\dev\db
    (evil-leader/set-key "2" (lambda ()
                               (interactive)
                               (dired "c:\\users\\mtz\\proj\\ecp\\dev\\db")))

    ;;quick load of TFS \Main\SqlScripts
    (evil-leader/set-key "3" (lambda ()
                               (interactive)
                               (dired "C:\\Users\\mtz\\proj\\TFS\\SafetyWebsite\\OSHE\\Main\\DbScripts")))

    ;;quick load of c:\users\mtz\TODO\TODO.org
    (evil-leader/set-key "t" (lambda ()
                               (interactive)
                               (find-file-existing "C:\\Users\\mtz\\TODO\\TODO.org")))))


(when (eq system-type 'gnu/linux)
  (when my-use-evil-p
   (evil-leader/set-key "1" (lambda ()
                              (interactive)
                              (dired "~")))))


;;; quick load of the .emacs (or init.el) file.
(defun my-load-init ()
  (interactive)
  (find-file-existing "~/.emacs.d/init.el"))

(when my-use-evil-p
  (evil-leader/set-key "`" #'my-load-init)
  ;; the above key is hard to type on a 60% poker so making an alternative.
  (evil-leader/set-key "8" #'my-load-init))


;;;-----------------------------------------------------------------------------
;;; VC version control
;;;-----------------------------------------------------------------------------
;; (defadvice vc-dir (before ensure-excluded-dirs)
;;   "add to the excluded dir list. It's not working if I add in init.el"
;;   (add-to-list 'vc-directory-exclusion-list "bin")
;;   (add-to-list 'vc-directory-exclusion-list "obj"))
;; (ad-activate 'vc-dir)

(with-eval-after-load "vc"
  (add-to-list 'vc-directory-exclusion-list "bin")
  (add-to-list 'vc-directory-exclusion-list "obj"))

;; (add-hook 'vc-- (lambda () (linum-mode 0)))

;;;-----------------------------------------------------------------------------
;;; Projectile
;;;-----------------------------------------------------------------------------
;; (require 'projectile)
;; (projectile-global-mode)
;; (setq projectile-indexing-method 'native)
;; (setq projectile-enable-caching t)
;; (define-key projectile-mode-map (kbd "C-x C-b") 'projectile-ibuffer)

;;;--------------------
;;; icicles
;;;--------------------
;; (require 'icicles) ; Load this library.
;; (icicle-mode 1)    ; Turn on Icicle mode.

;;;-----------------------------------------------------------------------------
;;; web-mode
;;;-----------------------------------------------------------------------------
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


;;;-----------------------------------------------------------------------------
;;; vimrc-mode
;;;-----------------------------------------------------------------------------
;;(require 'vimrc-mode)
(autoload 'vimrc-mode "vimrc-mode" "vimrc mode" t)
(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))

;;;-----------------------------------------------------------------------------
;;; Make dired appear in a side window
;;;-----------------------------------------------------------------------------
(autoload #'my-current-file-path "my-folder-nav" nil t)
(autoload #'my-current-folder-path "my-folder-nav" nil t)
(autoload #'my-folder-nav "my-folder-nav" nil t)
(global-set-key (kbd "<f8>") #'my-folder-nav)


;;;-----------------------------------------------------------------------------
;;; skewer-mode
;;;-----------------------------------------------------------------------------
;;(skewer-setup)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)
(add-hook 'web-mode-hook 'skewer-html-mode)

(with-eval-after-load "skewer-mode"
;;   (defun my-skewer-repl-clear-buffer ()
;;     "Deletes the contents of the skewer-reple buffer.
;; Depends on evil mode."
;;     (interactive)
;;     (evil-goto-line) ;bottom
;;     (evil-previous-visual-line) ;up 1
;;     (evil-end-of-line)
;;     (delete-region 1 (+ (point) 2))
;;     (evil-end-of-line))
;;   (define-key skewer-repl-mode-map (kbd "C-c M-o") #'my-skewer-repl-clear-buffer)

  (add-hook
   'skewer-repl-mode-hook
   (lambda ()
     ;;turn off line numbers in the repl
     (linum-mode 0)
     ;;there's always a trailing space at repl prompt. Don't highlight it. 
     (setq show-trailing-whitespace nil)))

  ;;(require 'simple-httpd)
  ;; (defun my-skewer-html ()
  ;;   "Wire up the html file you're editing with skewer."
  ;;   (interactive)
  ;;   ;;(skewer-html-mode) ; this is set in a hook, don't need it here.
  ;;   ;;(setq httpd-root "c:\\users\\mtz\\scratch\\testwebsite")
  ;;   (setq httpd-root (my-current-folder-path))
  ;;   (httpd-start)
  ;;   (browse-url-of-file (concat "http://localhost:8080/"
  ;;                               (file-name-nondirectory buffer-file-name)))
  ;;   (run-skewer)
  ;;   (message "put this in the <head>: <script src=\"http://localhost:8080/skewer\"></script> --- switch to tab http://localhost:8080/FileOrRouteName, then start evaling html"))
  )

;;;-----------------------------------------------------------------------------
;;; eshell
;;;-----------------------------------------------------------------------------
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

;;   ;;set up custome keybindings when the mode loads.
;;   (add-hook 'eshell-mode-hook ; `eshell-mode-map' not recognized unless set in the hook. Eval-after-load doesn't work.
;;             (lambda ()
;;               ;;Use the same keybinding to clear eshell as the SLIME repl
;;               (define-key eshell-mode-map (kbd "C-c M-o") 'my-eshell-clear-buffer)
;;               ;;make evil's dd compatible with the read-only prompt of hte current line.
;;               ;;(define-key evil-normal-state-map (kbd "<remap> <evil-delete-whole-line>") 'my-eshell-clear-line)
;;               ;;(evil-define-key 'normal eshell-mode-map (kbd "d d") 'my-eshell-clear-line)
;;               )))

;;;-----------------------------------------------------------------------------
;;; highlight-tail
;;;-----------------------------------------------------------------------------
;; (require 'highlight-tail)
;; (setq highlight-tail-colors '(("green yellow" . 0)
;;                               ("lemon chiffon". 40)))
;; (setq highlight-tail-colors '(("dark cyan" . 0)
;;                               ("black" . 40)))
;; (setq highlight-tail-steps 40 ;80
;;       highlight-tail-timer 0.04;0.04
;;       )
;; (setq highlight-tail-posterior-type t) ;(setq highlight-tail-posterior-type 'const)
;; (highlight-tail-mode)
;; ;;(highlight-tail-reload)

;;;-----------------------------------------------------------------------------
;;; eww web-browser
;;;-----------------------------------------------------------------------------
;;(setq browse-url-browser-function 'eww-browse-url) ;;make default for opening links.

(with-eval-after-load "eww"
  (when (eq my-curr-computer 'work-laptop)
    (setq eww-download-directory "C:\\Users\\mtz\\Downloads"))

  (define-key eww-mode-map (kbd "C-c h") #'eww-back-url)
  (define-key eww-mode-map (kbd "C-c l") #'eww-forward-url)
  (define-key eww-mode-map (kbd "C-c r") #'eww-reload)
  (define-key eww-mode-map (kbd "C-c g") #'eww)
  (define-key eww-mode-map (kbd "C-c o") #'ace-link-eww)
  (define-key eww-mode-map (kbd "<tab>") #'shr-next-link)
  ;; conkeror bindings. TODO: doesn't seem to work when in evil mode, fix it.
  (define-key eww-mode-map (kbd "B") #'eww-back-url)
  (define-key eww-mode-map (kbd "F") #'eww-forward-url)

  (progn ;; clearing the ^M
    (defun my-clear-wierd-m ()
      "Clear the weird ^M character that shows in some eww buffers."
      (interactive
       ;; (let ((args (query-replace-read-args "Replace" t)))
       ;;   (setcdr (cdr args) nil) ; remove third value returned from query---args
       ;;   args)
       )
      ;; TODO clear in the header line too.
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "" nil t)
          (replace-match ""))))

    (defadvice eww-render (after ad-clear-wierd-m)
      "Clear the weird ^M character that shows in some eww buffers."
      (my-clear-wierd-m))
    (ad-activate 'eww-render))

  (add-hook 'eww-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil))))

;;;-----------------------------------------------------------------------------
;;; w3
;;;-----------------------------------------------------------------------------
(with-eval-after-load "w3"
  (defun my-w3-goto (url)
    ;; "file:///C:/users/mtz/AppData/Roaming/CommonLispHyperSpec/HyperSpec/Body/m_w_slts.htm"
    (let ((w3--args nil) ;; prevents errors when loading CL hyperspec pages on local disk
          (w3-default-homepage url))
      (w3))))

;;;-----------------------------------------------------------------------------
;;; cedet
;;;-----------------------------------------------------------------------------
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

;;;-----------------------------------------------------------------------------
;;; cider
;;;-----------------------------------------------------------------------------
;;"C:\Program Files (x86)\Java\jre7\bin\java" -cp clojure-1.6.0.jar clojure.main

;;;-----------------------------------------------------------------------------
;;; aggressive-indent. Turning off for now since lispy makes it easy to keep
;;; things indented and aggressive-ident causes a noticeable lag when barfing/
;;; slurping in larger, deeply nested expressions.
;;;-----------------------------------------------------------------------------
;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'css-mode-hook #'aggressive-indent-mode)
;; (add-hook 'js2-mode-hook #'aggressive-indent-mode)

;;(add-hook 'slime-repl-mode-hook #'aggressive-indent-mode)
;;(global-aggressive-indent-mode 1)
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
;; (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)


;;;-----------------------------------------------------------------------------
;;; magit
;;;-----------------------------------------------------------------------------
;; prevent warning message.
;; Doesn't work when set in eval-after-load ???
;; (setq magit-last-seen-setup-instructions "1.4.0")

(when my-use-evil-p
  (evil-leader/set-key "m" #'magit-status)) ; autoloaded

(with-eval-after-load "magit"
  ;; Magit stole my M-h binding, take it back.
  ;; TODO: rebind magit-show-only-files, which was on M-h
  (when my-use-evil-p
    (define-key magit-mode-map (kbd "M-h") #'evil-window-left)
    ;; use emacs bindings (not evil). the new v2.1.0 magit uses evil for some buffers.
    (add-to-list 'evil-buffer-regexps '("\\*magit" . emacs))))


;;;-----------------------------------------------------------------------------
;;; ediff
;;;-----------------------------------------------------------------------------
(with-eval-after-load "ediff"
  (setq ediff-split-window-function 'split-window-horizontally)
  ;; don't use the popup window
  (setq ediff-window-setup-function 'ediff-setup-windows-plain) ;'ediff-setup-windows-multiframe
  )

;;;-----------------------------------------------------------------------------
;;; helm-w32-launcher. Microsoft Windows only?
;;;-----------------------------------------------------------------------------
(when (eq system-type 'windows-nt)
  (global-set-key (kbd "C-c w") 'helm-w32-launcher))

;;;-----------------------------------------------------------------------------
;;; leerzeichen. Displays symbols for tab, space, and newline.
;;;-----------------------------------------------------------------------------
(autoload 'leerzeichen-mode "leerzeichen" nil t)
;;(leerzeichen-mode)
;; (custom-set-faces
;;  '(leerzeichen ((t (:foreground "black";"#A8A800"
;;                                 :background "white";"#D4D4C8"
;;                                 :italic nil
;;                                 :bold nil
;;                                 ;;:box t
;;                                 )))))

;;;-----------------------------------------------------------------------------
;;; sql-indent
;;;-----------------------------------------------------------------------------
;; (eval-after-load "sql"
;;   '(load-library "sql-indent"))

;;;-----------------------------------------------------------------------------
;;; darkroom
;;;-----------------------------------------------------------------------------
;; (require 'darkroom)
(autoload 'darkroom-mode "darkroom" "darkroom-mode" t)
(with-eval-after-load "darkroom"
  (setq darkroom-margins 0.15)
  ;;nil keeps margins close to the centered text.
  (setq darkroom-fringes-outside-margins nil))

;;;-----------------------------------------------------------------------------
;;; vim-empty-lines-mode
;;;-----------------------------------------------------------------------------
;;(global-vim-empty-lines-mode) ; messes up recenter-top-bottom so not using for now.

;;;-----------------------------------------------------------------------------
;;; fill-column-indicator
;;;-----------------------------------------------------------------------------
;; (require 'fill-column-indicator)
;; (add-hook 'prog-mode-hook (lambda ()
;;                             (fci-mode 1))) ; fci-mode is autloaded.

(with-eval-after-load "fill-column-indicator"
  (setq fci-rule-column 80)
  (setq fci-rule-width 1)
  (progn
    (setq fci-dash-pattern 0.5)   ;length of the dash 0 to 1
    (setq fci-rule-use-dashes t))
  (setq fci-rule-color "#555555") ;tailored for zenburn ATM.

  (defun my-fci-refresh ()
    (interactive)
    (fci-mode 0)
    (fci-mode 1)))

;; ;;make fci compatible with emacs built-in variable `show-trailing-whitespace'
;; ;;TODO: it doesn't seem to be working!
;; ;;TODID: used "white-space-mode" instead of `show-trailing-whitespace'.
;; (setq whitespace-style '(face trailing))   

;;;-----------------------------------------------------------------------------
;;; flycheck
;;;-----------------------------------------------------------------------------
(with-eval-after-load "flycheck"
  (define-key flycheck-mode-map (kbd "M-n") #'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") #'flycheck-previous-error)
  ;;(evil-define-key 'flycheck-mode-map (kbd "M-n") #'flycheck-next-error)

;;;----------------------------------
;;; helm-flycheck
;;;----------------------------------
  (when my-use-helm-p
    (defun my-helm-flycheck ()
      (interactive)
      ;;nil for no candidate limit. I want to scroll through all the warnings.
      (let ((helm-candidate-number-limit nil))
        (call-interactively #'helm-flycheck)))
    ;;(evil-define-key 'normal flycheck-mode-map (kbd "C-c f") #'my-helm-flycheck)
    (define-key flycheck-mode-map (kbd "C-c f") #'my-helm-flycheck)
    ;;(define-key flycheck-mode-map (kbd "C-c ! h") #'helm-flycheck)
    ))

;;;-----------------------------------------------------------------------------
;;; hydra
;;;-----------------------------------------------------------------------------
(autoload #'my-choose-hydra "my-hydras" nil t)
(when my-use-evil-p
  ;; (define-key evil-normal-state-map (kbd "\\") #'my-choose-hydra)
  ;; (define-key evil-motion-state-map (kbd "\\") #'my-choose-hydra)
  )

(with-eval-after-load "hydra"
  (setq hydra-is-helpful t)
  ;; don't use window for hints. It seems to lock things up.
  ;; And window switcher mode really gets messed up.
  (setq hydra-lv nil))



;;;-----------------------------------------------------------------------------
;;; erc
;;;-----------------------------------------------------------------------------
(with-eval-after-load "erc"
  (progn
    ;;from finster on irc #emacs. switch erc buffers
    ;; TODO make it neutral to ido so i can use helm, swiper, default, etc.
    (defvar x/chatbuffer-types '(erc-mode
                                 circe-channel-mode
                                 circe-query-mode))
    (defun x/ido-chat-buffer ()
      "Switch to erc/circe buffer, completed by ido."
      (interactive)
      (switch-to-buffer
       (ido-completing-read "Channel:"
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

  (add-hook 'erc-mode-hook (lambda ()
                             (setq show-trailing-whitespace nil))))

;;;-----------------------------------------------------------------------------
;;; linum-relative
;;;-----------------------------------------------------------------------------
;; ;; (require 'linum-relative) ;linum-mode's behavior is changed by the linum-relative package.
;; (autoload 'linum-relative-toggle "linum-relative" "linum-relative" t)

;; ;; don't turn on by default. Makes the screen blink when line # changes.
;; ;;(linum-relative-toggle) ;;toggle between realtive and straight.
;; (with-eval-after-load "linum-relative"
;;   (setq linum-relative-format "%2s") ;rel numbers should never exceed 2 digits.
;;   (setq linum-relative-current-symbol "0"))


;;;-----------------------------------------------------------------------------
;;; guide-key
;;;-----------------------------------------------------------------------------
;; (require 'guide-key)

(with-eval-after-load "guide-key"
  ;; (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
  (setq guide-key/idle-delay 1.0)       ;default
  (setq guide-key/guide-key-sequence '("C-x" "C-c"))
  (setq guide-key/recursive-key-sequence-flag t)
  ;;(setq guide-key/popup-window-position 'bottom)
  )
;; (guide-key-mode 1)



;;;-----------------------------------------------------------------------------
;;; unkillable-scratch
;;;-----------------------------------------------------------------------------
(unkillable-scratch 1)


;;;-----------------------------------------------------------------------------
;;; bookmarks
;;;-----------------------------------------------------------------------------
;; (setq my-bookmarks
;;       '((google "www.google.com" (search email maps))
;;         (yahoo "www.yahoo.com" (search email news video))
;;         (msdn "www.msdn.com" (prog c-sharp visual-studio))))

;; (defun my-member-of (vals lst)
;;   (let ((has-a-tag-p nil))
;;     (cl-block check
;;       (dolist (v vals)
;;         (when (member v lst)
;;           (setq has-a-tag-p t)
;;           (return-from check))))
;;     has-a-tag-p))


;; (defun my-search-bookmarks (bookmarks tags)
;;   (cond
;;    ((null bookmarks) nil)
;;    ((my-member-of tags (third (first bookmarks)))
;;     (cons (first bookmarks)
;;           (my-search-bookmarks (rest bookmarks) tags)))
;;    (t (my-search-bookmarks (rest bookmarks) tags))))

;;;-----------------------------------------------------------------------------
;;; swiper. ivy is bundled with swiper.
;;; ivy
;;; counsel -> provides extra features for completing some things.
;;;-----------------------------------------------------------------------------
(when my-use-ivy-p
  (when my-use-evil-p
    (define-key evil-normal-state-map (kbd "s") #'swiper)
    (evil-leader/set-key "b" #'ivy-switch-buffer))

  (when (eq my-ui-type 'emacs)
    (global-set-key (kbd "C-c C-s") #'swiper)
    (global-set-key (kbd "C-c C-b") #'ivy-switch-buffer))

  (progn ;; counsel completion augmentation
    (global-set-key (kbd "M-x") #'counsel-M-x)
    (global-set-key (kbd "C-x C-f") #'counsel-find-file)
    ;; TODO: disable warning like i did for the other f9 binding for colors
    (global-set-key (kbd "<f9>")
                    (lambda ()
                      (interactive)
                      ;; make ivy window taller for viewing themes.
                      (let ((ivy-height 25))
                        (call-interactively #'counsel-load-theme))))
    (global-set-key (kbd "C-h v") #'counsel-describe-variable)
    (global-set-key (kbd "C-h f") #'counsel-describe-function))

  )

(with-eval-after-load "ivy"
  ;; turn on ivy completion. turned on when an autoloaded fn is used with a keybind
  ;; to slightly improve emacs init time. (discovered with profile-dotemacs.el)
  (ivy-mode 1)

  ;; (autoload 'ivy--regex-ignore-order "ivy" nil t) ;;shouldn't need this, but out of order matching is not working.
  ;; ;; allow out of order matching.
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  ;; use fancy highlights in the popup window
  (setq ivy-display-style 'fancy))

(with-eval-after-load "counsel"
  ;; remove the default ^ prefix used by `counsel-M-x'
  (when (and (eq system-type 'windows-nt)
             (not (eq my-curr-computer 'leyna-laptop)))
    ;;TODO fix this for linux and leyna-laptop. set-alist is not found?
    (set-alist 'ivy-initial-inputs-alist 'counsel-M-x ""))

  ;; redefine `counsel--load-theme-action' to not require confirmation
  ;; TODO: find an alternative to redefine so I don't have to manually sync
  ;;       with latest version of the sync on package updates.
  (defun counsel--load-theme-action (x)
    "Disable current themes and load theme X."
    (condition-case nil
        (progn
          (mapc #'disable-theme custom-enabled-themes)
          (load-theme (intern x) t)
          (when (fboundp 'powerline-reset)
            (powerline-reset)))
      (error "Problem loading theme %s" x))))


;;;-----------------------------------------------------------------------------
;;; color-identifiers-mode
;;;-----------------------------------------------------------------------------
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


;;;-----------------------------------------------------------------------------
;;; Integrate narrow-to-region with indirect buffers. To allow multiple
;;; major modes operatiing on 1 file.
;;;-----------------------------------------------------------------------------
(defun my-narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly.
Region defined by START and END is automaticallyl detected by (interactive \"r\")."
  (interactive "r")
  (deactivate-mark)
  (let ((buf-clone (clone-indirect-buffer nil nil))
        (mode      (intern (completing-read
                            "Mode: "
                            (mapcar (lambda (e)
                                      (list (symbol-name e)))
                                    (apropos-internal "-mode$" 'commandp))
                            nil t))))
    (with-current-buffer buf-clone
      (narrow-to-region start end)
      (funcall mode))
    (switch-to-buffer buf-clone)
    ;;refresh syntax highlighting by toggling `font-lock-mode'
    (dotimes (i 2)
      (font-lock-mode))))

(defun my-narrow-to-region (start end)
  (interactive "r")
  (deactivate-mark)
  (let ((mode (intern (completing-read
                       "Mode: "
                       (mapcar (lambda (e)
                                 (list (symbol-name e)))
                               (apropos-internal "-mode$" 'commandp))
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

;;;-----------------------------------------------------------------------------
;;; Focus javascript
;;;-----------------------------------------------------------------------------
;; delay execution of this code until `web-mode' is turned on.
(with-eval-after-load "web-mode"
  ;;needed to bind a key for `js2-mode-map'.
  ;;(require 'cl) is also needed but occurs higher up in this file.
  (require 'js2-mode)

  (defun my-js2-mode-on-region (start end)
    "Narrow on the active region, then turn on js2-mode."
    (interactive "r")
    (deactivate-mark)
    (narrow-to-region start end)
    (js2-mode))

  (cl-defun my-focus-javascript () ;using `cl-defun' to allow `return-from'
    "Automatcially narrow between <script> tags, then turn on js2-mode."
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
          (return-from my-focus-javascript nil))
        ;;start is found, move to the closing bracket >
        (let ((end-of-start (search-forward ">" nil t)))
          (when (null end-of-start)
            (message "start tag not found")
            (return-from my-focus-javascript nil)))
        ;; start highlighitng
        ;; (next-line)
        ;; (move-beginning-of-line nil)
        (set-mark-command nil)           ;(evil-visual-line)
        ;; jump to end tag. always search forward
        (setq end (search-forward end-tag-name nil t))
        (when (null end)
          (deactivate-mark)
          (message "end tag not found")
          (return-from my-focus-javascript nil))
        (let ((start-of-end (search-backward "<" nil t)))
          (when (null start-of-end)
            (message "end tag not found")
            (return-from my-focus-javascript nil)))
        ;;end tag is found.
        ;; (previous-line)
        ;; (move-end-of-line nil)
        ;; turn on js2-mode for this region. (and narrow)
        (call-interactively #'my-js2-mode-on-region))))

  (defun my-unfocus-javascript ()
    "Undo the effects of `my-focus-javascript'."
    (interactive)
    (widen)
    (web-mode))

  ;; key bindings
  (define-key web-mode-map (kbd "C-c j") #'my-focus-javascript)
  ;; TODO: Use a different technique for this keybind. If we didn't enter
  ;; `js2-mode' from `web-mode' then we don't want `my-unfocus-javascript' to
  ;; turn on web-mode.
  (define-key js2-mode-map (kbd "C-c C-c j") #'my-unfocus-javascript))



;;;-----------------------------------------------------------------------------
;;; svg-mode-line-themes
;;;-----------------------------------------------------------------------------
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

;;;-----------------------------------------------------------------------------
;;; isearch
;;;-----------------------------------------------------------------------------
(with-eval-after-load "isearch"
  ;; start highlighting a little faster than the default 0.25
  (setq lazy-highlight-initial-delay 0.2))

;;;-----------------------------------------------------------------------------
;;; my-window-search.  Limit isearch to the visible buffer.
;;;-----------------------------------------------------------------------------
(autoload #'my-window-search "my-window-search" nil t)
(global-set-key (kbd "C-c s") #'my-window-search)

;; using this binding for swiper when `my-ui-type' is 'emacs
;; (global-set-key (kbd "C-c C-s") #'my-window-search)

;;;-----------------------------------------------------------------------------
;;; lispy
;;;-----------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook       #'lispy-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'lispy-mode)
(add-hook 'ielm-mode-hook             #'lispy-mode)
(add-hook 'lisp-mode-hook             #'lispy-mode)
(add-hook 'lisp-interaction-mode-hook #'lispy-mode)
(add-hook 'scheme-mode-hook           #'lispy-mode)
(add-hook 'slime-repl-mode-hook #'lispy-mode)

(with-eval-after-load "lispy"
  (lispy-set-key-theme '(special)) ;helps when using paredit with lispy.
  ;; (lispy-set-key-theme '(special paredit c-digits))

  (setq lispy-avy-style-char 'pre)
  (setq lispy-avy-style-paren 'at) ;not at-full becuase parents are 1 char
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

  ;; make functions so "<" will alwoas go left. ">" will alwyas go right.
  ;; whether that's acheieved via a barf or slurp.
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
  (lispy-define-key lispy-mode-map-special (kbd "<") #'my-lispy-go-left-barf-or-slurp)
  (lispy-define-key lispy-mode-map-special (kbd ">") #'my-lispy-go-right-barf-or-slurp)

  ;; don't evaluate/insert on C-j. Use the plain way like paredit.
  (define-key lispy-mode-map (kbd "C-j") #'lispy-newline-and-indent-plain)
  ;; fn `kill-line' was bound to evil-insert C-k earlier. Override it for lispy.
  (when my-use-evil-p
    (evil-define-key 'insert lispy-mode-map (kbd "C-k") #'lispy-kill)))




;;;-----------------------------------------------------------------------------
;;; Info-mode
;;;-----------------------------------------------------------------------------
(with-eval-after-load "info"
  ;; rebind keys for vim friendliness.
  ;; orginial bindings. TODO: bind them to something else. Or just use M-x
  '((n Info-next)
    (p Info-prev)
    (f Info-follow-reference)
    (H describe-mode)
    (L Info-history)
    (w Info-copy-current-node-name)
    (e end-of-buffer)
    (b beginning-of-buffer)
    (g Info-goto-node)
    (s Info-search))
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
    (define-key Info-mode-map (kbd "s") my-swoop-fn))
  ;;TODO: figure out how to bind gg for top.
  )

;;;-----------------------------------------------------------------------------
;;; help mode
;;;-----------------------------------------------------------------------------
(with-eval-after-load "help-mode"
  (when my-use-evil-p
    (define-key help-mode-map (kbd "s") my-swoop-fn)))

;;;-----------------------------------------------------------------------------
;;; elisp emacs lisp
;;;-----------------------------------------------------------------------------
(when my-use-ivy-p
  ;; two different modes (and maps) for elisp:
  (define-key emacs-lisp-mode-map (kbd "C-M-i") #'counsel-el)
  (define-key lisp-interaction-mode-map (kbd "C-M-i") #'counsel-el))


;; (with-eval-after-load "lisp-mode"
;;   (add-hook 'emacs-lisp-mode-hook
;;             (lambda ()
;;               (push '("lambda" . ?f) prettify-symbols-alist))))

;;;-----------------------------------------------------------------------------
;;; elisp-slime-nav
;;; TODO: look into lisy's navigation. Maybe remove this section.
;;;-----------------------------------------------------------------------------
(dolist (hook '(emacs-lisp-mode-hook
                ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

(with-eval-after-load "elisp-slime-nav"

  (defun my-elisp-slime-nav-colored ()
    (interactive)
    (call-interactively #'elisp-slime-nav-describe-elisp-thing-at-point)
    (with-current-buffer (help-buffer)
      (rainbow-delimiters-mode)))

  ;; evil-mode stole the keybinds! take them back.
  (when my-use-evil-p
    (evil-define-key 'normal elisp-slime-nav-mode-map (kbd "M-.") #'elisp-slime-nav-find-elisp-thing-at-point)
    (evil-define-key 'normal elisp-slime-nav-mode-map (kbd "M-,") #'pop-tag-mark)
    (evil-define-key 'normal elisp-slime-nav-mode-map (kbd "C-c C-d d") #'my-elisp-slime-nav-colored)
    (evil-define-key 'normal elisp-slime-nav-mode-map (kbd "C-c C-d C-d") #'my-elisp-slime-nav-colored)))

;;;-----------------------------------------------------------------------------
;;; maximumize screen real-estate. TODO: complete this.
;;;-----------------------------------------------------------------------------
(autoload #'my-real-estate-max "my-screen-real-estate" nil t)
;; see cooresponding function `my-real-estate-restore'

;;;-----------------------------------------------------------------------------
;;; electric-spacing
;;;-----------------------------------------------------------------------------
;; originally called smart-operator-mode.
;; `electric-spacing-mode' is autoloaded.

;;;-----------------------------------------------------------------------------
;;; flymake-jslint
;;;-----------------------------------------------------------------------------
(with-eval-after-load "flymake-jslint"
  (setq flymake-jslint-command "jslint")
  (setq flymake-jslint-args nil))

;;;-----------------------------------------------------------------------------
;;; nlinum
;;;-----------------------------------------------------------------------------
;; (with-eval-after-load "nlinum"
;;   )


;;;-----------------------------------------------------------------------------
;;; sx
;;;-----------------------------------------------------------------------------
(with-eval-after-load "sx-tab"
  ;; TODO: this is not removing the 100 max limit. make it work.
  ;; using 'around' advice on `sx-tab-newest'
  (defadvice sx-tab-newest (around no-helm-limit)
    ;; temporarily remove the helm candiate limit. (via dynamic binding).
    (let ((helm-candidate-number-limit nil))
      ad-do-it))
  (ad-activate 'sx-tab-newest))



;;;-----------------------------------------------------------------------------
;;; my-date-stuff.el
;;;-----------------------------------------------------------------------------
(autoload #'my-insert-date-string "my-date-stuff" nil t)
(global-set-key (kbd "C-c i") #'my-insert-date-string)


;;;-----------------------------------------------------------------------------
;;; universal vim escape. Without key-chord dependence
;;;-----------------------------------------------------------------------------
;; rebind iedit-mode to another key. (it used C-; by default)
(global-set-key (kbd "C-c ;") #'iedit-mode)

(global-set-key (kbd "C-;") #'keyboard-escape-quit)
;; NOTE: can't wrap eval-after-loads in a let becuase it doesn't evaluate
;; the key, and then key doesn't exist by the time the file loads. Just
;; duplicate the `kbd' for now
(with-eval-after-load "evil"
  (let ((esc (kbd "C-;")))
    (define-key evil-insert-state-map esc #'evil-normal-state)
    (define-key evil-visual-state-map esc #'evil-exit-visual-state)
    (define-key evil-normal-state-map esc #'evil-force-normal-state)
    (define-key evil-ex-completion-map esc 'abort-recursive-edit)
    (define-key evil-read-key-map esc #'keyboard-quit)
    (define-key evil-replace-state-map esc 'evil-normal-state)))
(with-eval-after-load "helm"
  (define-key helm-map (kbd "C-;") #'helm-keyboard-quit))
(with-eval-after-load "ivy"
  (define-key ivy-mode-map (kbd "C-;") #'keyboard-escape-quit))

;;;-----------------------------------------------------------------------------
;;; list-processes
;;;-----------------------------------------------------------------------------
(with-eval-after-load "simple"

  (defun my-delete-process-at-point ()
    (interactive)
    (let ((process (get-text-property (point) 'tabulated-list-id)))
      (cond ((and process
                  (processp process))
             (delete-process process)
             (revert-buffer))
            (t
             (error "no process at point!")))))

  (define-key process-menu-mode-map (kbd "C-k") #'my-delete-process-at-point))

;;;-----------------------------------------------------------------------------
;;; shell-script-mode. (alias for sh-mode)
;;;-----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.gitignore$" . shell-script-mode))



;;;-----------------------------------------------------------------------------
;;; whitespace
;;;-----------------------------------------------------------------------------
;;show trailing whitespace.
;; (add-hook 'prog-mode-hook (lambda ()
;;                             (setq show-trailing-whitespace t)))

;; (defun my-toggle-show-trailing-whitespace ()
;;   (interactive)
;;   (not-m show-trailing-whitespace)
;;   ;;visual state makes the dipslay refresh.
;;   (evil-visual-char)
;;   (evil-exit-visual-state))
;; (global-set-key (kbd "C-c t") #'my-toggle-show-trailing-whitespace)
;; (global-set-key (kbd "C-c C-t") #'my-toggle-show-trailing-whitespace)

;;******** whitespace-mode *******
;; (require 'whitespace)
(with-eval-after-load "whitespace"
  (setq-default whitespace-line-column 80)
  ;;(setq whitespace-style '(face lines-tail))
  (setq-default whitespace-style '(face trailing)))

(with-eval-after-load "prog-mode"
  (add-hook 'prog-mode-hook (lambda ()
                              (whitespace-mode 1))))
;;(global-whitespace-mode 1)

;;;-----------------------------------------------------------------------------
;;; sallet. from fuco. saved to notElpa folder as a git submodule.
;;;-----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/notElpa/sallet")
(autoload #'sallet-buffer "sallet" nil t)

;;;-----------------------------------------------------------------------------
;;; sunrise-commander. saved to notElpa folder as a git submodule.
;;;-----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/notElpa/sunrise-commander")
(autoload #'sunrise-cd "sunrise-commander" nil t)

;;;-----------------------------------------------------------------------------
;;; winner-mode
;;;-----------------------------------------------------------------------------
(setq winner-dont-bind-my-keys t) ; doesn't work when set in eval-after-load.
(winner-mode 1)
(with-eval-after-load "winner"
  ;; reducing size from 200. Just need to facilitate a few quick undos.
  (setq winner-ring-size 20)
  (define-key winner-mode-map (kbd "C-c u") #'winner-undo)
  ;; NOTE: `winner-redo' only works if invoked immediatley after `winner-undo'.
  ;; TODO: find a way to make this keybind exist temporarily after the undo.
  (define-key winner-mode-map (kbd "C-c r") #'winner-redo))

;;;-----------------------------------------------------------------------------
;;; js2-highlight-vars
;;;-----------------------------------------------------------------------------
;; commenting out for now. It overwrites the modeline constantly making it
;; hard to read js2 error messages in the mode line.
;; (with-eval-after-load "js2-highlight-vars-autoloads"
;;   (add-hook 'js2-mode-hook (lambda () (js2-highlight-vars-mode))))


;;;-----------------------------------------------------------------------------
;;; bufftodo. ~/.emacs.d/notElpa/mine/bufftodo.el
;;;-----------------------------------------------------------------------------
;; (autoload #'bufftodo-ui "bufftodo" nil t)

;; (when my-use-evil-p
;;   (define-key evil-normal-state-map (kbd "\\") #'bufftodo-ui))

;; (with-eval-after-load 'bufftodo
;;   (setq bufftodo-open-new-window-p nil))

;;;-----------------------------------------------------------------------------
;;; function-args
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; my-relative-num
;;;-----------------------------------------------------------------------------
(when nil
  ;; TODO: put in a package. look for the ideal build in functions.
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
    (my-bottom-screen-line)

    )
  )

;;;-----------------------------------------------------------------------------
;;; Misc options. Keep this at the bottom
;;;-----------------------------------------------------------------------------
(when (and nil ;don't start server for now.
           ;;`server-start' doesn't seemt to work on MS-windows?
           (eq system-type 'gnu/linux))
  (server-start))

;; prevents warnings where you must select endcoding (like in `list-packages')
(prefer-coding-system 'utf-8)


(defun what-face (pos)
  "Prints the face at point.  POS = point???"
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
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


(progn ;;use the default emacs scroll bingding for C-v
  (when my-use-evil-p
    (define-key evil-normal-state-map (kbd "C-v") #'scroll-up-command)
    (define-key evil-motion-state-map (kbd "C-v") #'scroll-up-command)))

;; scroll like vim when moving 1 line off screen with j/k.
;; has some wierd rules about recentering, but 100 is supposed to
;; not recenter. I had an issue with value 1 where if i held down
;; j to scroll, it would perioditcally recenter.
(setq scroll-conservatively 100)
;; maintain cursor location when scrolling
(setq scroll-preserve-screen-position nil)

(progn ;;window navigation.
  (when my-use-evil-p
    (global-set-key (kbd "M-h") #'evil-window-left)
    (global-set-key (kbd "M-j") #'evil-window-down)
    (global-set-key (kbd "M-k") #'evil-window-up)
    (global-set-key (kbd "M-l") #'evil-window-right)))

;; cycle the buffers really fast. Not doing this anymore since these are error handling shortcuts in some modes.
;; (global-set-key (kbd "M-n") #'next-buffer)
;; (global-set-key (kbd "M-p") #'previous-buffer)


(cond
 ((eq my-curr-computer 'work-laptop)
  (setq ;;browse-url-generic-program "C:\\Program Files (x86)\\conkeror\\conkeror.exe"
   browse-url-generic-program "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe"
   browse-url-browser-function 'browse-url-generic))

 ((eq my-curr-computer 'hp-tower-2009)
  (setq browse-url-generic-program "conkeror"
        browse-url-browser-function 'browse-url-generic))

 ((eq my-curr-computer 'a-laptop-faster)
  (setq browse-url-generic-program "conkeror"
        browse-url-browser-function 'browse-url-generic))

 ((or (eq my-curr-computer 'raspberry-pi)
      (eq my-curr-computer 'utilite))
  (setq browse-url-generic-program "surf"
        browse-url-browser-function 'browse-url-generic)))


;; (defun my-insert-img ()
;;   (interactive)
;;   (let ((i 0))
;;     (while (< i 10)
;;       (insert-image nyan-prompt-nyan-cat-image)
;;       (incf i)))
;;   (insert "\n\n")
;;   (let ((i 0))
;;     (while (< i 10)
;;       (insert-image nyan-prompt-nyan-cat-image)
;;       (incf i))))


;;transparent
;;(set-frame-parameter (selected-frame) 'alpha '(100 93))

;; (let* ((alpha-focused 93)
;;        (alpha-unfocused 93)
;;        (alpha-lst (list alpha-focused alpha-unfocused)))
;;   (set-frame-parameter (selected-frame) 'alpha alpha-lst)
;;   (add-to-list 'default-frame-alist alpha-lst))



;; Only browse interesting buffers. Not *scratch*, *messages*, etc.
;;(global-set-key "\C-x\C-b" 'bs-show)


;; if we havn't bound leader-b to buffer switching yet, then default to ibuffer.
(when (or (eq my-curr-computer 'leyna-laptop)
          (eq my-curr-computer 'raspberry-pi)
          (and (not my-use-helm-p)
               (not my-use-ido-p)
               (not my-use-ivy-p)))
  (when my-use-evil-p
    (evil-leader/set-key "b" #'ibuffer))
  ;; (evil-leader/set-key "b" #'ido-switch-buffer)
  ;; (global-set-key (kbd "M-/") #'hippie-expand)
  ;; (evil-leader/set-key "b" #'ivy-switch-buffer)
  ;; (evil-leader/set-key "b" #'ibuffer)
  )

;;ibuffer. the way C-x C-b should be.
(global-set-key (kbd "C-x C-b") 'ibuffer)



;;(global-linum-mode 0) ;show/hide line numbers in margin

(setq-default column-number-mode 1) ;show/hide column # in mode line.
;;show/hide line # in mode line. Use fn what-line
(setq-default line-number-mode 1)
;; do not display modes in the mode-line. They take up too much space.
;; Function `describe-mode' (kbd "C-h m") is better to see active modes anyway.
(setq mode-line-modes nil)
;;(setq mode-line-position nil) ;hide the % of the buffer you are viewing.



(progn ;; show time in mode line
  ;; disable process average display. Not sure why this is mixed in with time
  ;; display.
  (setq display-time-default-load-average nil)
  (setq display-time-load-average nil)
  (setq display-time-load-average-threshold nil)
  ;;(setq-default display-time-day-and-date t)
  (display-time-mode 1)
  ;;(setq-default display-time-format "%-m/%-d %-I:%M%#p")
  (setq display-time-format "%-I:%M%#p"))

;;show lambdas with the greek symbol
(when (and (>= emacs-major-version 24)
           (>= emacs-minor-version 4))
  (unless (eq my-curr-computer 'raspberry-pi)
    (global-prettify-symbols-mode 1)))

;;indent keyword args properly. Use common lisp-style for (if) indendation too?
;;(setq lisp-indent-function 'common-lisp-indent-function)

(setq inhibit-startup-message t)
;;(setq initial-scratch-message ";; Scratch buffer ;;\n\n\n\n")
(setq initial-scratch-message "\n\n\n\n\n")
;; (setq initial-buffer-choice (lambda () (get-buffer-create "foo")))
;; (setq initial-major-mode #'fundamental-mode) ;;for faster startup.


(blink-cursor-mode 0)

(progn ;; hightlight current line stuff
  (with-eval-after-load "hl-line"
    (setq hl-line-sticky-flag nil)
    (setq global-hl-line-sticky-flag nil))
  ;; (global-hl-line-mode 1)
  ;; (hl-line-mode 1) ; highlight the current line
  )

(global-auto-revert-mode t) ;;reload buffer if it changes on disk outside emacs.

(setq-default line-spacing nil)


;; use tilde's as the fringe graphic for empty lines. Like Vim.
(progn
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
  (setq-default indent-line-function 'insert-tab))

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



;;;--------------------------------------------------------------------
;;; my-square-one
;;;--------------------------------------------------------------------
(autoload #'my-square-one "my-square-one" nil t)
(when my-use-evil-p
  (evil-leader/set-key "0" #'my-square-one))



;;;--------------------------------------------------------------------
;;; Turn on disabled functions
;;;--------------------------------------------------------------------
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

;;;---------------------------------------------------
;;; rand
;;;---------------------------------------------------
(autoload 'rand "my-rand" nil t)

;; (setq msgDb '("hi"))

;; (setq msgIndex 0)

;; (defun msg ();Use random once database is large enough to rarely get a dupe.
;;   "Display a message."
;;   (interactive)
;;   (let* ((max (- (length msgDb) 1))
;;          (msg (my-getAtIndex msgIndex msgDb)))
;;     (setq msgIndex (+ 1 msgIndex))
;;     (when (> msgIndex max)
;;       (setq msgIndex 0))
;;     (clippy-say msg)))

;; ;; (defun msg ()
;; ;;   "Display a random message."
;; ;;   (interactive)
;; ;;   (let* ((max (- (length msgDb) 1))
;; ;;          (i (rand 0 max))
;; ;;          (msg (my-getAtIndex i msgDb)))
;; ;;     ;(message msg)
;; ;;     (clippy-say msg)
;; ;;     ;(clippy-say (yow))
;; ;;     ))

;; (evil-leader/set-key "m" 'msg)

;;;---------------------------------------------------
;;; touch typing
;;;---------------------------------------------------
;; defined in ~/.emacs.d/notElpa/mine/my-type-tutor.el
(autoload 'my-type-tutor "my-type-tutor" nil t)

;;;---------------------------------------------------
;;; ms
;;;---------------------------------------------------
;; (defvar ms-width 30)
;; (defvar ms-height 25)
;; (defvar ms-map ())

;; ;; u f O *

;; (defun ms-init ()
;;   (setq ms-map ()) ;; clear
;;   (let ((times (* ms-width ms-height)))
;;     (dotimes (i times)
;;       (setq ms-map (cons 'u ms-map)))))

;; (defun ms-get-index (r c)
;;   (+ (* r ms-width) c))

;; (defun ms-render-map (map)
;;   (dotimes (r ms-height)
;;     (dotimes (c ms-width)
;;       (insert (symbol-name (nth (ms-get-index r c)
;;                                 ms-map)))
;;       (insert " "))
;;     (insert "\n")))

;; (ms-render-map ms-map)
;; (ms-init)
;; (dolist (x ms-map)
;;   (insert (symbol-name x))
;;   (insert " "))

;;;---------------------------------------------------
;;; hour format conversion. 12 -> 24
;;;---------------------------------------------------
;; (defun to24 (hour amPm)
;;   (cond
;;    ((and (equal amPm "AM")
;;          (= hour 12))
;;     (- hour 12))

;;    ((or (and (equal amPm "AM")
;;              (>= hour 1)
;;              (<= hour 11))
;;         (and (equal amPm "PM")
;;              (= hour 12)))
;;     hour)

;;    (t (+ hour 12))))

;; (defun to12 (hour)
;;   (cond
;;    ((= hour 0)
;;     '(12 . "AM"))
;;    ((and (> hour 0) (< hour 12))
;;     `(,hour . "AM"))
;;    ((= hour 12)
;;     '(12 . "PM"))
;;    (t `(,(- hour 12) . "PM"))))

;; (progn ;;test it
;;   (assert (= 0 (to24 12 "AM")))
;;   (assert (= 1 (to24 1 "AM")))
;;   (assert (= 2 (to24 2 "AM")))
;;   (assert (= 3 (to24 3 "AM")))
;;   (assert (= 4 (to24 4 "AM")))
;;   (assert (= 5 (to24 5 "AM")))
;;   (assert (= 6 (to24 6 "AM")))
;;   (assert (= 7 (to24 7 "AM")))
;;   (assert (= 8 (to24 8 "AM")))
;;   (assert (= 9 (to24 9 "AM")))
;;   (assert (= 10 (to24 10 "AM")))
;;   (assert (= 11 (to24 11 "AM")))
;;   (assert (= 12 ( to24 12 "PM")))
;;   (assert (= 13 (to24 1 "PM")))
;;   (assert (= 14 (to24 2 "PM")))
;;   (assert (= 15 (to24 3 "PM")))
;;   (assert (= 16 (to24 4 "PM")))
;;   (assert (= 17 (to24 5 "PM")))
;;   (assert (= 18 (to24 6 "PM")))
;;   (assert (= 19 (to24 7 "PM")))
;;   (assert (= 20 (to24 8 "PM")))
;;   (assert (= 21 (to24 9 "PM")))
;;   (assert (= 22 (to24 10 "PM")))
;;   (assert (= 23 (to24 11 "PM"))))


;;;------------------------------------
;;; interact with Microsoft SQL Server
;;;------------------------------------
;;sqlcmd -S 127.0.0.1,42000\OSHE
;;sqlcmd -S 127.0.0.1,42000\OSHE -q "SELECT 'hello';"

;;;------------------------------------
;;; remove all bold face attributes
;;;------------------------------------
;; (defun my-remove-bold ()
;;   "Remove all bold face attributes."
;;   (interactive)
;;   (mapc
;;    (lambda (face)
;;      (set-face-attribute face
;;                          nil ;; all frames
;;                          :weight 'normal))
;;    (face-list)))

;;; init.el ends here
