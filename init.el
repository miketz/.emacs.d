;;; package --- Summary
;;; Commentary:
;;; My emacs config.

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
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;;--------------------------------------------------------------------
;; Helper functions and macros
;;--------------------------------------------------------------------
(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (s-trim-left (s-trim-right s)))

(defmacro not-m (bool)
  "Similar to the not function.  But...
It must operate on a variable (not a value)
It mutates BOOL to the opposite value.
Useful to check a boolean state and toggle the state in 1 go."
  `(setq ,bool (not ,bool)))

(defmacro my/letify-alist (alist &rest body)
  ;; destructuring macro found by inspecting Bozhidar Batsov's zenburn color
  ;; theme.
  ;; example usage:
  ;; (my/letify-alist ((red . "#FF0000")
  ;;                   (green . "#00FF00")
  ;;                   (blue . "#0000FF"))
  ;;   (print red)
  ;;   (print green)
  ;;   (print blue))
  "Convert the keys of ALIST to variables."
  (declare (indent defun))
  `(let (,@(mapcar (lambda (cell)
                     (list (car cell) (cdr cell)))
                   alist)) ;TODO make alist work for a variable passed in.
     ,@body))


(defun my/alst-get-keys (lst)
  (mapcar 'car lst))
(defun my/alst-get-values (lst)
  (mapcar 'cdr lst))

(defun my/alst-print-keys (lst)
  (mapc #'(lambda (key)
            (insert (symbol-name key)) ;key must be a symbol
            (insert "\n"))
        (my/alst-get-keys lst)))

(defun my/getAtIndex (i lst)
  (cond
   ((null lst) nil)
   ((= i 0) (car lst))
   (t (my/getAtIndex (- i 1) (cdr lst)))))

(defun my/str-ends-with-p (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

(defun my/str-starts-with-p (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(defun my/str-replace (what with in)
  (replace-regexp-in-string (regexp-quote what) with in))

(defun my/get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun my/read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

;;--------------------------------------------------------------------
;; helper functions to draw a list in N columns. 2-d diplay from a
;; 1-d list.
;;--------------------------------------------------------------------
(when nil ;these are special-use functions. Don't create them.

  (defun my/get-longest-str (lst)
    "returns length of the longest str"
    (apply #'max
           (mapcar #'length
                   lst)))

  (defun my/get-longest-sym (lst) ;PASS
    "returns length of the longest symbol name"
    (my/get-longest-str (mapcar #'symbol-name
                                lst)))

  (defun my/index-1d (r c num-cols) ;PASS, but not used.
    "returns a 1-D index. Using the 2d indexs r and c.
And the number of columns (and vertical layout)."
    (+ (* c num-cols)
       r))

  (defun my/index-column (i num-cols) ;PASS, but not used.
    "returns the column the 1-D index falls under for N cols
(and vertical layout)"
    (floor (/ i num-cols)))

  (defun my/index-row (i num-cols) ;PASS, but not used.
    "returns the row the 1-D index falls under for N cols (and vertical layout)"
    (let* ((c (floor (/ i num-cols)))
           (r (- i (* c num-cols))))
      r))

  (defun my/get-columns (lst num-cols) ;PASS
    "returns a list-of-lists. A list for each column from the 1-D lst.
Assums a vertically stacked display of the list.
(my/get-columns '(a b c d e f g) 3)
=>
((a b c) (d e f) (g))"
    ;;STRANGE: for some reason if I align docstring to the left without white
    ;;space it messes up paredit's ability to match parens in the code
    ;;following this fucntion.
    (let ((len            (length lst))
          (lst-of-columns nil) ;the goal
          (num-rows (+ (floor (/ len num-cols))
                       (if (> (mod len num-cols) 0) 1 0)))
          (i 0)
          (c              0))
      (while (< c num-cols)
        (let ((column nil)
              (r 0))
          (dotimes (r num-rows)
            (let ((val (nth i lst)))
              (when (not (null val));last col may have empty slots to be skipped
                (setq column (append column (list val)))))
            (incf i))
          (setq lst-of-columns (cons column lst-of-columns)))
        (incf c))
      (reverse lst-of-columns)))

  (defun my/get-longest-forEachCol (lst num-cols) ;;PASS
    "Gets the longest length for each column in LST, assuming NUM-COLS.
'(lenCol1 lenCol2... lenColN)."
    (mapcar #'(lambda (column)
                (my/get-longest-str column))
            (my/get-columns lst num-cols)))

  (defun my/render-list (lst num-cols min-col-spaces)
    (let* ((data (mapcar #'symbol-name lst))
           (len (length data))
           (num-rows (+ (floor (/ len num-cols))
                        (if (> (mod len num-cols) 0) 1 0)))
           (col-lengths (my/get-longest-forEachCol data num-cols))
           (columns (my/get-columns data num-cols)))
      (insert "(:i ") ; add junk item to circumvent elisp indentation rules.
      (dotimes (r num-rows)
        (dotimes (c num-cols)
          (let* ((col (nth c columns))
                 (val (nth r col))
                 (curr-col-len (nth c col-lengths))
                 (pad-size (- curr-col-len (length val)))
                 (is-last-col (= c (- num-cols 1))))
            (when (not (null val))
              (insert val)
              (unless is-last-col
                (dotimes (s min-col-spaces) (insert " "))
                (dotimes (p pad-size) (insert " ")))))
          )
        (unless (= r (- num-rows 1)) ;unless last row
          (insert "\n")))
      (insert ")")))

  );end when, render list functions

;;----------------------------------
;; flags used for conditional execution
;;----------------------------------
;; (display-graphic-p)
;; system-type
;; my/curr-computer

;; Keeping track of the various computers I use emacs on.
(setq my/computers
      '(unknown ;if my-curr-computer.txt does not exist
        work-laptop
        raspberry-pi
        utilite
        old-sony-vaio
        a-tower
        a-laptop-old
        a-laptop-faster
        leyna-laptop))
;; currently used computer. (manually set)
;; Used to conditionally set computer specific options, and paths.
;; NOTE: When setting up emacs on a new computer create file
;; ~/.emacs.d/my-curr-computer.txt
;; Then type the name of the symbol (see `my/computers') in the text file.
;; The file should contain 1 line and no whitespace. The text will be converted
;; to a symbol.
(let ((curr-comp-file "~/.emacs.d/my-curr-computer.txt"))
  (if (file-exists-p curr-comp-file)
      (setq my/curr-computer (intern (my/get-string-from-file curr-comp-file)))
    (setq my/curr-computer 'unknown)))

;; TODO: look into a way to use auto-complete for some modes and company for
;;       others.

(require 'cl)
;;----------------------------------
;; Packages
;;----------------------------------
(setq my/packages
      '(evil
        evil-leader
        evil-escape
        evil-matchit
        evil-snipe
        ;;evil-god-state
        ;;evil-surround
        key-chord
        slime
        ;;sly
        paredit
        ;;smartparens
        redshank
        auto-complete
        ac-slime
        company
        slime-company
        ace-jump-mode
        ace-window
        ace-jump-zap
        csharp-mode
        js2-mode
        skewer-mode
        ac-js2
        helm
        helm-cmd-t
        helm-swoop
        ;;helm-git-grep ;search text of files.
        ;;helm-ls-git ;search for files. Similar to helm-cmd-t but with git.
        icicles
        ;;projectile
        clippy
        yasnippet
        rainbow-delimiters
        ;;rainbow-mode
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
        linum-relative
        guide-key
        unkillable-scratch
        speed-type))

(when (eq my/curr-computer 'work-laptop)
  (add-to-list 'my/packages 'omnisharp))



(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (cl-mapc 'add-to-list
;;          '(package-archives package-archives)
;;          '(("melpa" . "http://melpa.milkbox.net/packages/")
;;            ("marmalade" . "http://marmalade-repo.org/packages/"))
;;          '(t t))

;; (setq package-archives
;;       '(("gnu" . "http://elpa.gnu.org/packages/")
;;         ("melpa" . "http://melpa.milkbox.net/packages/")
;;         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (pkg my/packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(defun package-list-unaccounted-packages ()
  "Like `package-list-packages', but show only the packages that
are installed and are not in `my/packages'.  Useful for
cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x)
                    (and (not (memq x my/packages))
                         (not (package-built-in-p x))
                         (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))



;;--------------------------------------------------------------------
;; w32-send-sys codes. Operating system commands. MS Windows only.
;;--------------------------------------------------------------------
(when (eq system-type 'windows-nt)
  (setq my/w32-actions
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
  (defun my/w32-get-code (action)
    "Get the numeric code from the action symbol."
    (cdr (assoc action my/w32-actions)))
  (defun my/w32-get-action (code)
    "Get the action symbol from the numeric code."
    (car (cl-rassoc code my/w32-actions)))
  (defun my/w32-run (action)
    "Executes a w32 action."
    (let ((code (my/w32-get-code action)))
      (w32-send-sys-command code))))
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
  (setq evil-replace-state-message nil))



(setq evil-default-cursor t)

;;(add-to-list 'load-path "~/.emacs.d/evil") ; only without ELPA/el-get
(require 'evil)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-mode 1)

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

;; Map for Esc.
;; The key-chord package causes lag when a key of the chord is pressed.
;; So using the the built-in control chords which are fast. Better than the
;; awkward C-[ default.
;; (evil-define-key 'insert global-map (kbd "C-n") 'evil-normal-state)
;; (evil-define-key 'visual global-map (kbd "C-n") 'evil-exit-visual-state)


;; For visual mode: press $ to go to the end of the line minus the newline char.
(defadvice evil-end-of-line (after do-not-highlight-newline)
  "For visual mode: press $ to go to the end of the line minus the newline
char."
  (when (evil-visual-state-p)
    (evil-backward-char)))
(ad-activate 'evil-end-of-line)

(defun copy-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))

;;leader keys
(evil-leader/set-leader ",")
(evil-leader/set-key "w" 'other-window)
(evil-leader/set-key "q" 'balance-windows)
(evil-leader/set-key "x" 'maximize-window)
(evil-leader/set-key "," 'delete-other-windows)
(evil-leader/set-key "d" 'delete-window)
(evil-leader/set-key "k" 'kill-this-buffer)

(evil-leader/set-key "<" (lambda () ;shrink window a little
                           (interactive)
                           (shrink-window-horizontally 15)))
(evil-leader/set-key ">" (lambda () ;widen window a little
                           (interactive)
                           (enlarge-window-horizontally 15)))
;; (evil-leader/set-key "j" (lambda ()
;;                            (interactive)
;;                            (shrink-window 10)))
;; (evil-leader/set-key "k" (lambda ()
;;                            (interactive)
;;                            (enlarge-window 10)))

;;`isFrameMax-my' can get out of sync. Hit <Leader>f a 2cd time to re-sync.
(setq isFrameMax-my nil)
;;TODO: look into equivalent resizing for non-Windows machines.
(when (eq system-type 'windows-nt)
  (evil-leader/set-key "f" (lambda ()
                             (interactive)
                             (let ((action (if (not-m isFrameMax-my)
                                               'max
                                             'restore-curr-frame)))
                               (my/w32-run action)))))

;;evalate lisp expression. Insert result on a new line.
;;(evil-leader/set-key "l" "a\C-j\C-u\C-x\C-e")

(defun my/eval-last-sexp ()
  (interactive)
  (let ((val (eval (eval-sexp-add-defvars (preceding-sexp)) lexical-binding)))
    (prin1-to-string val)))

(if (display-graphic-p)
    (progn
      (require 'pos-tip)
      (evil-leader/set-key "e" (lambda ()
                                 (interactive)
                                 ;;(clippy-say (my/eval-last-sexp))
                                 (pos-tip-show (my/eval-last-sexp)))))
  (progn
    (evil-leader/set-key "e"
      (lambda ()
        (interactive)
        (save-excursion
          (evil-append 1)
          (default-indent-new-line)
          (eval-last-sexp t) ; t to insert result in buffer.
          (evil-normal-state))))))

;; (evil-leader/set-key "r"
;;   (lambda ()
;;     (interactive)
;;     (save-excursion
;;       (evil-append 1)
;;       (slime-eval-last-expression) ; t to insert result in buffer.
;;       (evil-normal-state))))

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
                                    )))))))


(evil-leader/set-key "a" 'slime-eval-print-last-expression)
(evil-leader/set-key "p" (lambda ()
                           (interactive)
                           (save-excursion ;don't move the point
                             (evil-append 1)
                             (slime-pprint-eval-last-expression)
                             (evil-normal-state))))

;;----------------------------------
;; evil-snipe
;;----------------------------------
;; (setq evil-snipe-enable-highlight nil)
;; (setq evil-snipe-enable-incremental-highlight nil)
;; (setq evil-snipe-scope 'visible)
;; (setq evil-snipe-repeat-scope 'visible)

;; (require 'evil-snipe)
;; (global-evil-snipe-mode 1)


;;----------------------------------
;; evil-god-state
;;----------------------------------
;; (evil-define-key 'normal global-map (kbd "\\") 'evil-execute-in-god-state)
;; (evil-define-key 'motion global-map (kbd "\\") 'evil-execute-in-god-state)
;; (evil-define-key 'god global-map [escape] 'evil-god-state-bail)
;; ;;(evil-leader/set-key "," 'evil-execute-in-god-state)
;; ;;(global-set-key (kbd "C-c i") 'insert-date-string)

;; ;; (add-hook 'evil-god-start-hook (lambda () (diminish 'god-local-mode)))
;; ;; (add-hook 'evil-god-stop-hook (lambda () (diminish-undo 'god-local-mode)))

;;----------------------------------
;; evil-surround
;;----------------------------------
;; (require 'evil-surround)
;; (global-evil-surround-mode 1)


;;----------------------------------
;; font
;;----------------------------------
(when (or (eq my/curr-computer 'work-laptop)
          (eq my/curr-computer 'leyna-laptop))
  ;; configure default settings for fonts.
  (setq my/default-font 'consolas
        my/good-fonts '((inconsolata "Inconsolata" 135 normal) ;looks OK. fits a good number of lines on screen. flaky on bold. no itallic?
                        (consolas "Consolas" 125 normal) ; consolas is the best looking but fits fewer lines on screen.
                        (dejavu "DejaVu Sans Mono for Powerline" 120 normal) ;good, but looks a bit "tall"
                        (fixedsys "FixedSys" 120 normal)))

  (cl-defun my/set-font (&optional &key
                                   (sym nil) (height nil) (weight nil) (resize-window nil))
    "Sets the font.
If sym is not specified it uses the configured default set in `my/default-font'.
If height or weight are not specified then it uses the configured defaults in `my/good-fonts'.
Resize-window = t will adjust the window so the modeline fits on screen, etc."
    (unless sym (setq sym my/default-font))
    (let ((the-font (assoc sym my/good-fonts)))
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
      (my/w32-run 'restore-curr-frame)
      (my/w32-run 'max))))


;; (defun my/set-font-size ()
;;   "Interactive layer over my/set-font. Takes the font size as user input."
;;   (interactive)
;;   (let ((size (string-to-number (read-string "font-size: "
;;                                              nil
;;                                              'my/history))))
;;     (my/set-font :height size
;;                  :resize-window t)))
;; (defun my/set-font-weight ()
;;   "Interactive layer over my/set-font."
;;   (interactive)
;;   (let ((weight (intern (read-string "font-weight: "
;;                                              nil
;;                                              'my/history))))
;;     (my/set-font :weight weight
;;                  :resize-window t)))

(when (eq my/curr-computer 'raspberry-pi)
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


;;----------------------------------
;; cursor
;;----------------------------------
(cl-defun my/cursor-stuff (&optional &key (color-emacs nil)
                                     (color-evil nil)
                                     (color-motion nil));(color-motion "red")
  (interactive)
  (let ((args-emacs '())
        (args-evil '())
        (args-evil-motion '())) ;use same color throughout evil-mode, except for "motion" state.
    (when color-emacs (setq args-emacs (cons color-emacs args-emacs)))
    (when color-evil (setq args-evil (cons color-evil args-evil)))
    (when color-motion (setq args-evil-motion (cons color-motion args-evil-motion)))
    ;;bar hollow box hbar
    (setq-default cursor-type (cons 'bar args-emacs))
    (setq evil-emacs-state-cursor (cons 'bar args-emacs))
    (setq evil-normal-state-cursor (cons 'box args-evil))
    (setq evil-insert-state-cursor (cons 'bar args-evil))
    (setq evil-visual-state-cursor (cons 'hollow args-evil))
    (setq evil-operator-state-cursor (cons 'hollow args-evil))
    (setq evil-replace-state-cursor (cons 'hbar args-evil))
    ;;motion state is when some of evil is disabled (like in the function help and C-h-i pages).
    ;;give special color I know when it is not full-evil bindings.
    (setq evil-motion-state-cursor (cons 'box args-evil-motion))))

(my/cursor-stuff) ;set the default cursor style. colors not specified yet.

;;------------------------------------------------------
;; Color theme stuff.
;;------------------------------------------------------
;;TODO: implement a way to undo color settings made outside the theme definition.
;;      use custom-theme-set-faces to set the colors/styles so they are rolled back
;;      when switching/disabling themes.
(defadvice load-theme (before disable-before-load)
  "Disable any loaded themes before enabling a new theme.
This prevents overlapping themes; something I would rarely want."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))
(ad-activate 'load-theme)

;;custom-enabled-themes
;;custom-safe-themes
;;custom-known-themes
;;(custom-available-themes)
(setq my/c-index 0)
(defun my/cycle-theme ()
  (interactive)
  (let* ((themes (custom-available-themes))
         (thm (nth my/c-index themes)))
    (unwind-protect
        (progn
          (load-theme thm t))
      (progn
        (print thm)
        (incf my/c-index)
        (when (= my/c-index (length themes))
          (setq my/c-index 0))))))


;;programmatically call a fucntion as if a prefix arg C-u was used.
;; (let ((current-prefix-arg '(4)))
;;   (call-interactively #'next-line))

(defun my/load-theme (theme &optional no-confirm no-enable)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))
    nil nil))

  ;;disable any active themes
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))

  (let ((no-confirm t)
        (no-enable nil))
    (unless (custom-theme-name-valid-p theme)
      (error "Invalid theme name `%s'" theme))
    ;; If THEME is already enabled, re-enable it after loading, even if
    ;; NO-ENABLE is t.
    (if no-enable
        (setq no-enable (not (custom-theme-enabled-p theme))))
    ;; If reloading, clear out the old theme settings.
    (when (custom-theme-p theme)
      (disable-theme theme)
      (put theme 'theme-settings nil)
      (put theme 'theme-feature nil)
      (put theme 'theme-documentation nil))
    (let ((fn (locate-file (concat (symbol-name theme) "-theme.el")
                           (custom-theme--load-path)
                           '("" "c")))
          hash)
      (unless fn
        (error "Unable to find theme file for `%s'" theme))
      (with-temp-buffer
        (insert-file-contents fn)
        (setq hash (secure-hash 'sha256 (current-buffer)))
        ;; Check file safety with `custom-safe-themes', prompting the
        ;; user if necessary.
        (when (or no-confirm
                  (eq custom-safe-themes t)
                  (and (memq 'default custom-safe-themes)
                       (equal (file-name-directory fn)
                              (expand-file-name "themes/" data-directory)))
                  (member hash custom-safe-themes)
                  (custom-theme-load-confirm hash))
          (let ((custom--inhibit-theme-enable t)
                (buffer-file-name fn))    ;For load-history.
            (eval-buffer))
          ;; Optimization: if the theme changes the `default' face, put that
          ;; entry first.  This avoids some `frame-set-background-mode' rigmarole
          ;; by assigning the new background immediately.
          (let* ((settings (get theme 'theme-settings))
                 (tail settings)
                 found)
            (while (and tail (not found))
              (and (eq (nth 0 (car tail)) 'theme-face)
                   (eq (nth 1 (car tail)) 'default)
                   (setq found (car tail)))
              (setq tail (cdr tail)))
            (if found
                (put theme 'theme-settings (cons found (delq found settings)))))
          ;; Finally, enable the theme.
          (unless no-enable
            (enable-theme theme))
          t)))))

(defun color (theme &optional no-confirm no-enable)
  "dupliate of `my/load-theme' to simulate :color in vim."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))
    nil nil))

  ;;disable any active themes
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))

  (let ((no-confirm t)
        (no-enable nil))
    (unless (custom-theme-name-valid-p theme)
      (error "Invalid theme name `%s'" theme))
    ;; If THEME is already enabled, re-enable it after loading, even if
    ;; NO-ENABLE is t.
    (if no-enable
        (setq no-enable (not (custom-theme-enabled-p theme))))
    ;; If reloading, clear out the old theme settings.
    (when (custom-theme-p theme)
      (disable-theme theme)
      (put theme 'theme-settings nil)
      (put theme 'theme-feature nil)
      (put theme 'theme-documentation nil))
    (let ((fn (locate-file (concat (symbol-name theme) "-theme.el")
                           (custom-theme--load-path)
                           '("" "c")))
          hash)
      (unless fn
        (error "Unable to find theme file for `%s'" theme))
      (with-temp-buffer
        (insert-file-contents fn)
        (setq hash (secure-hash 'sha256 (current-buffer)))
        ;; Check file safety with `custom-safe-themes', prompting the
        ;; user if necessary.
        (when (or no-confirm
                  (eq custom-safe-themes t)
                  (and (memq 'default custom-safe-themes)
                       (equal (file-name-directory fn)
                              (expand-file-name "themes/" data-directory)))
                  (member hash custom-safe-themes)
                  (custom-theme-load-confirm hash))
          (let ((custom--inhibit-theme-enable t)
                (buffer-file-name fn))    ;For load-history.
            (eval-buffer))
          ;; Optimization: if the theme changes the `default' face, put that
          ;; entry first.  This avoids some `frame-set-background-mode' rigmarole
          ;; by assigning the new background immediately.
          (let* ((settings (get theme 'theme-settings))
                 (tail settings)
                 found)
            (while (and tail (not found))
              (and (eq (nth 0 (car tail)) 'theme-face)
                   (eq (nth 1 (car tail)) 'default)
                   (setq found (car tail)))
              (setq tail (cdr tail)))
            (if found
                (put theme 'theme-settings (cons found (delq found settings)))))
          ;; Finally, enable the theme.
          (unless no-enable
            (enable-theme theme))
          t)))))


(global-set-key (kbd "<f9>") #'my/load-theme)
(global-set-key (kbd "<f10>") #'my/cycle-theme)

(defun my/cursor-stuff-darkBg ()
  (interactive)
  ;;(my/cursor-stuff :color-emacs "cyan" :color-evil "#00DF00")
  (my/cursor-stuff :color-emacs "cyan" :color-evil "spring green")
  )

(defun my/cursor-stuff-lightBg ()
  (interactive)
  (my/cursor-stuff :color-emacs "black" :color-evil "blue"))

(defun my/rainbow-parens-dark-bg ()
  "Colors for parens that are easy to distinguish from each other when against a dark bg."
  (interactive)
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "orange red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "plum"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "lawn green"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "white"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "seagreen1"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black"))))))

(defun my/rainbow-parens-light-bg ()
  (interactive)
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "black"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#09a509"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#3388ff"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "black" :background "red"))))))

(defun my/rainbow-parens-light-bg2 ()
  "Colored parens with highlighting."
  (interactive)
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "black"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "light cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background "#faEaEa"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "lavenderblush"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "lemon chiffon"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background "#EEEEFF"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "#fff7ca"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black"))))))


(defun color-grandshell ()
  "Grandshell with a few mode-specific additoins."
  (interactive)
  (load-theme 'grandshell t)

  (custom-theme-set-faces
   'grandshell
   ;;This isn't a js2 error face. But externals tend to be an error since js2 doesn't find the them.
   '(js2-external-variable ((t :underline (:color "red" :style wave)
                               :background "black")))
   '(js2-error ((t :underline (:color "red" :style wave)
                   :background "dark red")))
   '(js2-function-call ((t :foreground "violet"))) ;;making same as font-lock-function-name-face
   '(js2-warning ((t :underline (:color "yellow" :style wave)
                     :background "navy blue")))
   ;; colors copied from grandshell-theme.el
   `(mode-line ((t (:foreground  "#eee"
                                 :background  "#331133"
                                 :box (:line-width -1 :style released-button)))))
   ;;colors copied from grandshell-theme.el
   `(mode-line-inactive ((t (:foreground  "#643"
                                          :background  "#110011"
                                          :weight light
                                          :box (:line-width -1 :style released-button)
                                          :inherit (mode-line )))))
   ))

(defun color-zenburn ()
  "Load the zenburn theme created by Bozhidar Batsov.  Make a few extra mods too."
  (interactive)
  (load-theme 'zenburn t)
  (my/cursor-stuff-darkBg) ;;TODO: move into `custom-set-faces'
  ;;wrap mods in `custom-theme-set-faces' so they can be rolled back with `disable-theme'
  (custom-theme-set-faces
   'zenburn
   ;;from VIM charcoal: hi Normal guifg=#ADC299 guibg=#35352B "*
   ;;`(default ((t (:foreground "#CFC5A9" :background "#35352B"))))
   ;;`(default ((t (:foreground "#CCCCBC" :background "#35352B"))))

   ;;'(cursor ((t (:foreground "blue" :background "red"))))
   ;; `(mode-line
   ;;   ((t (:foreground "#8FB28F"
   ;;                    :background "#032203"
   ;;                    ;;:underline "dark yellow"
   ;;                    ;;:overline "green"
   ;;                    :box (:line-width -1 :style released-button)))
   ;;    (t :inverse-video t)))
   '(hydra-face-red
     ((t (:foreground "green" :bold t))))
   '(hydra-face-blue
     ((t (:foreground "cyan" :bold t))))
   '(hydra-face-amaranth
     ((t (:foreground "green" :bold t))))
   `(font-lock-comment-face
     ((t (:foreground "#8FB28F" :slant italic))))
   `(ace-jump-face-foreground
     ((t (:foreground "spring green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   ;;highlight so i can see the slime function parameters highlight.
   '(highlight ((t (:foreground "spring green"
                                :background "black"))))

   `(region
     ((t (:background "#69685E"))));"#49483E"
   ;; '(region ((t :background "black")))
   '(isearch ((t :background "black"
                 :foreground "yellow"
                 :bold nil
                 :underline t)))
   ;;This isn't a js2 error face. But externals tend to be an error since js2 doesn't find the them.
   '(js2-external-variable ((t :underline (:color "red" :style wave)
                               :background "black")))
   '(js2-error ((t :underline (:color "red" :style wave)
                   :background "dark red")))
   '(js2-function-call ((t :foreground "#93E0E3"))) ;;making same as font-lock-function-name-face
   '(js2-warning ((t :underline (:color "yellow" :style wave)
                     :background "navy blue")))

   ;;'(js2-private-member ((t :foreground "green")))
   ;;'(js2-function-param ((t :foreground "green")))
   ;;'(js2-instance-member ((t :foreground "green")))
   ;;'(js2-private-function-call ((t :foreground "green")))

   '(lazy-highlight ((t
                      :background "black"
                      :foreground "cyan"
                      :bold nil
                      :underline t)))
   ;; '(num3-face-odd '((t)))
   ;; '(num3-face-even '((t :underline t)))
   '(leerzeichen ((t (:foreground "yellow4";"#A8A800"
                                  :background "black";"#D4D4C8"
                                  :italic nil
                                  :bold nil))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "orange red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "plum"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "lawn green"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "white"))))
   ;;'(rainbow-delimiters-depth-8-face ((t (:foreground "seagreen1"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "yellow" :background "black"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black"))))))

(defun color-github ()
  (interactive)
  (load-theme 'github t)
  (set-background-color "white")
  (custom-theme-set-faces
   'github
   `(mode-line ((t (:background "grey75"
                                :foreground "black"
                                :box (:line-width -1 :style released-button)
                                :height 1.0))))
   `(ace-jump-face-foreground
     ((t (:foreground "yellow"
                      :background "black"
                      :slant normal
                      :weight bold
                      :inverse-video nil))))
   ;; '(js2-external-variable ((t :underline (:color "red" :style wave)
   ;;                             :background "black")))
   ;; '(js2-error ((t :underline (:color "red" :style wave)
   ;;                             :background "dark red")))
   '(js2-function-call ((t :foreground "#990000"))) ;;making same as font-lock-function-name-face
   ;; '(js2-warning ((t :underline (:color "yellow" :style wave)
   ;;                   :background "navy blue")))
   ;;'(js2-private-member ((t :foreground "green")))
   ;;'(js2-function-param ((t :foreground "green")))
   ;;'(js2-instance-member ((t :foreground "green")))
   ;;'(js2-private-function-call ((t :foreground "green")))
   ))

(defun color-badger ()
  (interactive)
  (load-theme 'badger t)
  (custom-theme-set-faces
   'badger
   `(font-lock-comment-face
     ((t (:foreground "dark cyan" :slant italic))))
   '(region ((t :background "#7F073F")))
   `(mode-line
     ((t (:foreground "#00AF00"
                      :background "#150505"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "dark gray"
                      :background  "#090202";"#051515"
                      :box (:line-width -1 :style pressed-button)))))))

(defun color-gruvbox ()
  (interactive)
  (load-theme 'gruvbox t)
  ;;(my/set-font :weight 'normal)
  (my/cursor-stuff-darkBg)
  (my/rainbow-parens-dark-bg)
  ;; (set-face-foreground 'font-lock-string-face "salmon")
  ;;(set-face-foreground 'font-lock-comment-face "#66A555")
  (custom-theme-set-faces
   'gruvbox
   `(font-lock-comment-face
     ((t (:foreground "#66A555"))))
   `(fringe
     ((t (:foreground "burlywood"
                      :background "black"))))
   `(mode-line
     ((t (:foreground "#00AF00";"#A08F10"
                      :background "#150505"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "dark gray"
                      :background "#051515"
                      :box (:line-width -1 :style pressed-button)))))))

(defun color-monokai ()
  "Load the monokai theme with several adjustments."
  (interactive)
  (load-theme 'monokai t)
  (custom-theme-set-faces
   'monokai
   `(compilation-info
     ((t (:foreground "DarkOrange2"))))
   `(cursor
     ((t (:foreground "black"
                      :background "green"))))
   `(font-lock-comment-face
     ((t (:foreground "#66A555"))))
   `(fringe
     ((t (:foreground "burlywood"
                      :background "black"))))

   `(ace-jump-face-foreground
     ((t (:foreground "spring green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(mode-line
     ((t (:foreground "#00cf5f" ;"#00ff7f"
                      :background "#101010"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "#777777"
                      :background "#222222"
                      :box (:line-width -1 :style released-button)))))


   ;; '(js2-error
   ;;   ((t (:foreground "red"
   ;;                    :underline t))))
   ;;highlight so i can see the slime function parameters highlight.
   '(highlight ((t (:foreground "spring green"
                                :background "black"))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "orange red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "plum"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "lawn green"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "white"))))
   ;;'(rainbow-delimiters-depth-8-face ((t (:foreground "seagreen1"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "yellow" :background "black"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black"))))))

;; vim charcoal: hi Normal guifg=#ADC299 guibg=#35352B "*
(setq mayan-smoke "#F4F4E8"
      charcoal "#35352B")

;;; loads the default emacs theme. Makes a few mods too.
(defun color-default ()
  (interactive)
  ;;default is the lack of themes, so disable any enabled themes.
  (dolist (thm custom-enabled-themes)
    (disable-theme thm))
  ;;(set-background-color "ivory2")
  (my/cursor-stuff-lightBg)
  ;;(set-face-background hl-line-face "#EEFFEE")
  (my/rainbow-parens-light-bg)
  ;;(my/set-font :weight 'bold)

  (custom-set-faces
   `(ace-jump-face-foreground
     ((t (:foreground "black"
                      :background "cyan"
                      :slant normal
                      :weight bold
                      :inverse-video nil))))))

(defun color-gandalf ()
  (interactive)
  (load-theme 'gandalf t)
  (custom-theme-set-faces
   'gandalf
   `(region
     ((t (:background "goldenrod"))))
   `(fringe
     ((t (:foreground "black"
                      :background "gray"))))

   `(ace-jump-face-foreground
     ((t (:foreground "black"
                      :background "yellow"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(mode-line
     ((t (:foreground "black"
                      :background "gray"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "#505050"
                      :background "#e3e3e3"
                      :box (:line-width -1 :style released-button)))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "black"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "light cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background "#faEaEa"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "lavenderblush"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "lemon chiffon"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background "#EEEEFF"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "#fff7ca"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black"))))))

(defun color-leuven ()
  (interactive)
  (load-theme 'leuven t)
  (custom-theme-set-faces
   'leuven
   `(default ((t (:foreground "black" :background ,mayan-smoke))))
   ;;`(default ((t (:foreground "black" :background ,"white"))))
   `(mode-line ((t (:box (:line-width -1 :color "#1A2F54")
                         :foreground "#85CEEB" :background "#335EA8"
                         :style released-button))))
   `(mode-line-inactive ((t (:box (:line-width -1 :color "#4E4E4C")
                                  :foreground "#F0F0EF" :background "#9B9C97"
                                  :style released-button))))
   '(js2-function-call ((t :foreground "blue")))
   '(leerzeichen ((t (:foreground "black";"#A8A800"
                                  :background "white";"#D4D4C8"
                                  :italic nil
                                  :bold nil
                                  ;;:box t
                                  ))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "black"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "light cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background "#faEaEa"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "lavenderblush"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "lemon chiffon"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background "#EEEEFF"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "#fff7ca"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black"))))))

(defun color-dichromacy ()
  (interactive)
  (load-theme 'dichromacy t)
  (my/cursor-stuff :color-emacs "red" :color-evil "blue")
  (custom-theme-set-faces
   'dichromacy
   ;;`(default ((t (:foreground "black" :background ,mayan-smoke))))
   `(default ((t (:foreground "black" :background ,"white"))))
   `(mode-line
     ((t (:foreground "black"
                      :background "#CCCCCC"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "#666666"
                      :background "#EEEEEE"
                      :box (:line-width -1 :style released-button)))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "black"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "light cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background "#faEaEa"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "lavenderblush"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "lemon chiffon"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background "#EEEEFF"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "#fff7ca"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black")))))

  ;; (my/set-font :weight 'bold
  ;;              :height 140)
  ;;(set-background-color "floral white")
  )


(setq cycle-colors2 '("papaya whip" "old lace" "floral white" "ivory2"
                      "mint cream" "honeydew" "white smoke" "ghost white"
                      "snow" "alice blue" "lavender"))
(setq cycle-colors `( "old lace" "floral white" "snow" "ghost white" "white"
                      "#F3F1DE"  "#F3F2EA" ,mayan-smoke))
(setq cycle-index 0)
(defun my/cycle-light-bg ()
  (interactive)
  (if (= cycle-index (1- (length cycle-colors)))
      (setq cycle-index 0)
    (setq cycle-index (1+ cycle-index)))
  (let ((bg (my/getAtIndex cycle-index cycle-colors)))
    (set-background-color (my/getAtIndex cycle-index cycle-colors))
    (message bg)))

(global-set-key (kbd "<f12>") 'my/cycle-light-bg)

;;theme of the week and corresponding settings. This may change often.
(progn
  (cond
   ((or (eq my/curr-computer 'work-laptop)
        (eq my/curr-computer 'leyna-laptop))
    (my/set-font :sym 'consolas
                 :height 125;'90 105 115 120 125
                 :weight 'normal)
    (when (display-graphic-p)
      (color-zenburn)))

   ((eq my/curr-computer 'a-laptop-faster)
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
    (when (display-graphic-p) ;this doens't return true for emacs daemon!
      (color-monokai))))


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


;;---------------------------------------------
;; Recursively byte-compile every .el file
;;---------------------------------------------
;;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)



;;---------------------------------------------
;; sly
;;---------------------------------------------
;; (setq my/use-sly nil)

;; (when my/use-sly
;;   (when (eq my/curr-computer 'work-laptop)
;;     (setq inferior-lisp-program "C:\\Users\\mtz\\programs\\ccl-1.10-windowsx86\\ccl\\wx86cl64")))

;;---------------------------------------------
;; SLIME
;;---------------------------------------------
(require 'slime-autoloads)

(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-fancy
                    slime-company
                    slime-banner
                    slime-indentation))
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     ;;(define-key slime-mode-map (kbd "M-.") 'slime-edit-definition) ;override evil's binding of M-. when using slime
     (evil-define-key 'normal slime-mode-map (kbd "M-.") 'slime-edit-definition);override evil's binding of M-. when using slime
     ;;disable the banner header line in repl. TODO: get rid of the date string that replaces it too.
     (setq slime-header-line-p nil)

     ;; (require 's)
     ;; (setq slime-words-of-encouragement (let ((words '())) ;;hidden
     ;;                                      (dolist (w slime-words-of-encouragement)
     ;;                                        (when (s-contains? "REPL" w)
     ;;                                          (setq words (cons w words))))
     ;;                                      words))
     ))

(progn
  (when (eq my/curr-computer 'work-laptop)
    (setq slime-default-lisp 'ccl
          slime-lisp-implementations '((ccl ("C:\\Users\\mtz\\programs\\ccl-1.10-windowsx86\\ccl\\wx86cl64"))
                                       (clisp ("~/path/to/clisp-2.49/clisp" "-modern")))));clisp is just a fake example for now.
  (when (eq my/curr-computer 'utilite)
    (setq slime-default-lisp 'ccl
          slime-lisp-implementations '((ccl ("armcl")))))

  (when (eq my/curr-computer 'a-laptop-faster)
    (setq slime-default-lisp 'ccl
          slime-lisp-implementations '((ccl ("~/Downloads/ccl/lx86cl"))
                                       (sbcl ("/usr/bin/sbcl")))))

  ;; when on a computer with SLIME set up
  (when (or (eq my/curr-computer 'work-laptop)
            (eq my/curr-computer 'utilite)
            (eq my/curr-computer 'a-laptop-faster))
    ;; connect lisp buffers to SLIME automatically.
    (add-hook 'slime-mode-hook ;not sure why this works, since it's a hook on slime-mode which I thought would need to be hooked on lisp-mode-hook???
              (lambda ()
                (unless (slime-connected-p)
                  (save-excursion (slime)))))))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            ;;turn off line numbers in the repl
            (linum-mode 0)
            ;;there's always a trailing space at repl prompt. Don't highlight it.
            (setq show-trailing-whitespace nil)
            ;;aggressive-indent moves SLIME's comments in the REPL. Turn it off.
            (aggressive-indent-mode 0)))

;;(define-key slime-mode-map (kbd "<tab>") #'slime-indent-and-complete-symbol)
(evil-define-key 'insert slime-mode-map (kbd "<tab>") #'slime-indent-and-complete-symbol)

(when (eq my/curr-computer 'work-laptop)
  ;; use local hyperspec
  (setq common-lisp-hyperspec-root "file:///C:/users/mtz/AppData/Roaming/CommonLispHyperSpec/HyperSpec/"))


;;---------------------------------------------
;; redshank
;;---------------------------------------------
;; (require 'redshank-loader)
;; (eval-after-load "redshank-loader"
;;   `(redshank-setup '(lisp-mode-hook
;;                      slime-repl-mode-hook) t))
;; ;;   (eval-after-load "redshank"
;; ;;     '(progn ...redefine keys, etc....))


;;---------------------------------------------
;; company
;;---------------------------------------------
;;company mode is breaking emacs 24.3. Works OK in 24.4
(when t ;; (and (>= emacs-major-version 24)
  ;;      (>= emacs-minor-version 4))
  (require 'company)
  (add-hook 'after-init-hook 'global-company-mode) ;all buffers
  (define-key company-mode-map (kbd "C-SPC") 'company-complete) ;C-Space like Visual Studio
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "<tab>") 'company-complete) ;expands till -. Completes after that.
  (setq company-idle-delay nil) ;disable automatic completion
  (setq company-minimum-prefix-length 3) ;but if automatic is on, don't fire until 3 chars.

  (setq company-tooltip-limit 20) ;popup more suggestions.

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
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)))


;;---------------------------------------------
;; slime-company
;;---------------------------------------------
;; this is set in the slime section

;;---------------------------------------------
;; Auto-complete
;;---------------------------------------------
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

;; ;;------------------------------------------------
;; ;; ac-slime. integrates auto-complete with slime.
;; ;;------------------------------------------------
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

;;--------------------------------------------------
;; turn on lisp-mode when editing file .stumpwmrc
;;--------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.stumpwmrc\\'" . lisp-mode))

;;---------------------------------------------
;; Org mode
;;---------------------------------------------
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-startup-indented t)
(setq org-log-done t) ;make timestamp when flagging something done with C-c C-t

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(when (eq my/curr-computer 'work-laptop)
  (setq org-agenda-files '("C:\\Users\\mtz\\TODO.org")))

;;---------------------------------------------
;; csharp-mode
;;---------------------------------------------
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;;-------------------------
;; js2-hightlight-vars.el
;;-------------------------
;; (add-to-list 'load-path "~/.emacs.d/notElpa/")
;; (require 'js2-highlight-vars)

;;-------------------------
;; align-let.el
;;-------------------------
;;(load "~/.emacs.d/notElpa/align-let.el")
(add-to-list 'load-path "~/.emacs.d/notElpa/")
(autoload 'align-let "align-let" nil t)

(let ((key (kbd "C-c C-a")))
  (define-key lisp-mode-map key #'align-let)
  (define-key emacs-lisp-mode-map key #'align-let)
  (define-key lisp-interaction-mode-map key #'align-let))

;; (let ((abadf 333)
;;       (x     222)
;;       (yy    44)))

;; (setq aaaaaaaaaaaaa 2
;;       b             3
;;       cc            433
;;       d             "hello there")

;;---------------------------------------------
;; js2-mode
;;---------------------------------------------
;;(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq-default js2-global-externs '("$" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout"
                                   "clearTimeout" "setInterval" "clearInterval" "location" "__dirname"
                                   "console" "JSON" "ActiveXObject"))
(setq js2-highlight-level 3);;maximum highlighting


(add-hook 'js2-mode-hook
          (lambda ()
            ;; replace ambiguous name "Javascript-IDE" with "js2"
            (setq mode-name "js2")
            (evil-define-key 'normal js2-mode-map (kbd "M-n") 'js2-next-error)
            (evil-define-key 'normal js2-mode-map (kbd "M-p") (lambda ()
                                                                (interactive)
                                                                (js2-next-error -1)))
            ;; (setq-default js2-global-externs "jQuery $")
            ;; (setq-default js2-indent-on-enter-key t)
            ;; (add-to-list 'js2-ecma-262-externs "setTimeout")

            ;; (when (featurep 'js2-highlight-vars)
            ;;   (js2-highlight-vars-mode))

            ;;(js2-imenu-extras-mode)
            (electric-pair-mode 1)
            ))

(add-hook 'js2-mode-hook #'(lambda () (yas-minor-mode)))

;;--------------------
;; ac-js2
;;--------------------
(when nil
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  (setq ac-js2-evaluate-calls t);requires connection to browser with (run-skewer)
  ;;(add-to-list 'ac-js2-external-libraries "path/to/lib/library.js") ;external lib example
  )

;; ;;--------------------
;; ;; nxml
;; ;;--------------------
;; (setq nxml-slash-auto-complete-flag t) ;auto-insert when typing </


;;--------------------
;; imenu
;;--------------------
(evil-leader/set-key "i" 'helm-imenu)

;;--------------------
;; Helm
;;--------------------
;;(add-to-list 'load-path "~/.emacs.d/helm")

(setq helm-ff-transformer-show-only-basename nil
      ;;helm-adaptive-history-file             "~/.emacs.d/data/helm-history"
      ;;helm-yank-symbol-first                 t
      ;;helm-move-to-line-cycle-in-source      t
      helm-buffers-fuzzy-matching            t
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
;;           #'(lambda ()
;;               (define-key eshell-mode-map (kbd "<tab>") #'helm-esh-pcomplete)
;;               (define-key eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history)))

(evil-leader/set-key "b" #'helm-buffers-list)
;;(evil-leader/set-key "b" #'helm-mini) ;;use helm instead of bs-show
;;(global-set-key (kbd "C-x b")   #'helm-mini)
;;(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
(unless (eq my/curr-computer 'raspberry-pi) ;helm is a little slow on a raspberry pi.
  (global-set-key (kbd "M-x") #'helm-M-x))
;; (global-set-key (kbd "C-x C-f") #'helm-find-files)
;; (global-set-key (kbd "C-x C-r") #'helm-recentf)
;; (global-set-key (kbd "C-x r l") #'helm-filtered-bookmarks)
(global-set-key (kbd "M-y") #'helm-show-kill-ring)


(require 'helm-config)
(require 'helm-cmd-t)
;;(helm-adaptative-mode t)
(require 'helm-swoop)

(progn ;;from tuhdo. Customizing helm window size/display.
  (setq helm-display-header-line nil) ;save 1 line for rarely used header.
  (set-face-attribute 'helm-source-header nil :height 1.0);don't make source seperators bigger than needed
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

(unless (eq my/curr-computer 'raspberry-pi) ;helm is a little slow on a raspberry pi.
  (helm-mode 1) ;helm-selection everywhere like when using M-x
  )

;;(global-set-key (kbd "C-x c!")   #'helm-calcul-expression)
;;(global-set-key (kbd "C-x c:")   #'helm-eval-expression-with-eldoc)
;;(define-key helm-map (kbd "M-o") #'helm-previous-source)

;;(global-set-key (kbd "M-s s")   #'helm-ag)



;;----------------------------------
;; helm-company
;;----------------------------------
;; (eval-after-load 'company
;;   '(progn
;;      (define-key company-mode-map (kbd "C-SPC") 'helm-company)
;;      (define-key company-active-map (kbd "C-SPC") 'helm-company)))

;;----------------------------------
;; evil-escape
;;----------------------------------
;;(evil-escape-mode 1)
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
  (key-chord-define evil-insert-state-map chord #'evil-normal-state)
  (key-chord-define evil-visual-state-map chord #'evil-exit-visual-state)
  ;; (key-chord-define evil-replace-state-map chord 'evil-normal-state)
  ;; (key-chord-define evil-operator-state-map chord func)
  ;; (key-chord-define evil-motion-state-map chord func))
  (key-chord-define helm-map chord #'helm-keyboard-quit))

;;(key-chord-define evil-insert-state-map "fj" 'evil-normal-state)
;;(key-chord-define c++-mode-map ";;"  "\C-e;")

;;--------------------
;; helm-git-grep (makes emacs crash on windows)
;;--------------------
;; (when my/run-sys-specific
;;   (defadvice helm-git-grep (after turn-off-activeupdate)
;;     "Turn off active update in MS-windows. It can't handle grep processes spawning on each keystroke."
;;     (helm-toggle-suspend-update))
;;   (ad-activate 'helm-git-grep))

;; (require 'helm-git-grep)
;; (define-key helm-git-grep-mode-map (kbd "C-u") 'helm-toggle-suspend-update)
;; (evil-leader/set-key "g" 'helm-git-grep)

;;--------------------
;; vc-git-grep. This is better for ms-windows since it can't handle helm-git-grep's many processes.
;; Also grepping is a pretty heavy weight opperation so I prefer to set up the search inputs first,
;; select the top folder, etc instead of searching in real-time for each key press.
;;--------------------
(require 'vc-git)

(defun my/is-in-gitrepo ()
  "Returns t if the current directory is in a git repo."
  (interactive)
  (string= "true\n" ;there seems to be a newline char so include it
           (shell-command-to-string "git rev-parse --is-inside-work-tree")))

(defun my/git-grep-make-param (pat)
  "Make git-grep work with helm patters of ^, !, $"
  (let ((val (concat " -e \"" pat "\"")))
    (cond
     ((my/str-starts-with-p pat "!")
      (concat " --not -e \"" (substring pat 1 (length pat)) "\""))
     ((my/str-starts-with-p pat "^")
      val);we get helm-style "start with" ^ implemented for free in the default git-grep regex.
                                        ;TODO: make it work when there is leading whitspace on the line.
     ((my/str-ends-with-p pat "$")
      val);TODO: implement helm-style "ends with" $.
     (t val))))

(defun my/git-grep-make-cmd (input)
  ;;git --no-pager grep --no-index --ignore-case -n -e "preview" --and -e "print" -- *.cs
  (interactive)
  (let ((patterns (split-string input " "))
        (git-pat ""))
    (setq git-pat (my/git-grep-make-param (first patterns)))
    (dolist (p (rest patterns))
      (setq git-pat (concat git-pat " --and " (my/git-grep-make-param p))))
                                        ;(concat "git --no-pager grep --no-index --ignore-case -n " git-pat)
    (concat "git --no-pager grep "
            (unless (my/is-in-gitrepo) "--no-index --exclude-standard");--exclude-standard so it honors the .gitignore file when not in a git repo.
            " --ignore-case -n "
            git-pat)))

;; (defun my/git-grep ()
;;   (interactive)
;;   (let* ((input (read-string "search: "))
;;          (results (shell-command-to-string (my/git-grep-make-cmd input))))
;;     (insert results)))



(defun my/vc-git-grep (regexp &optional files dir)
  "Same as the normal vc-git-grep except I split the search string on spaces and pass
each value as a separate parameter to git grep. Making it work like helm filtering."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
                                   nil nil 'grep-history)
             nil))
      (t (let* ((regexp (grep-read-regexp))
                (files (grep-read-files regexp))
                (dir (read-directory-name "In directory: "
                                          nil default-directory t)))
           (list regexp files dir))))))
  (require 'grep)
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (if (null files)
          (if (string= command "git grep")
              (setq command nil))
        (setq dir (file-name-as-directory (expand-file-name dir)))
        (setq command
              (concat (my/git-grep-make-cmd regexp) " -- " files)
              ;; (grep-expand-template "git --no-pager grep -n -e <R> -- <F>"
              ;;                       regexp files)
              )
        (when command
          (if (equal current-prefix-arg '(4))
              (setq command
                    (read-from-minibuffer "Confirm: "
                                          command nil nil 'grep-history))
            (add-to-history 'grep-history command))))
      (when command
        (let ((default-directory dir)
              (compilation-environment (cons "PAGER=" compilation-environment)))
          ;; Setting process-setup-function makes exit-message-function work
          ;; even when async processes aren't supported.
          (compilation-start command 'grep-mode))
        (if (eq next-error-last-buffer (current-buffer))
            (setq default-directory dir))))))

(evil-leader/set-key "g" #'my/vc-git-grep)

;;--------------------
;; helm-swoop
;;--------------------
(define-key evil-normal-state-map (kbd "s") 'helm-swoop)
;; (global-set-key (kbd "C-c s") 'helm-swoop)
;; (global-set-key (kbd "C-c C-s") 'helm-swoop)
;;(evil-leader/set-key "s" 'helm-multi-swoop-all)

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
(setq helm-swoop-speed-or-color nil)



;;---------------------
;; sublimity
;;---------------------
;; (require 'sublimity)
;; (require 'sublimity-scroll)
;;map is annoying;;(require 'sublimity-map)

;;(setq sublimity-scroll-drift-length 1) ;(sublimity-scroll-weight 4)

;;(sublimity-global-mode)


;;---------------------
;; Clippy. pop-up help
;;---------------------
(evil-leader/set-key "c" 'clippy-describe-function)
(evil-leader/set-key "v" 'clippy-describe-variable)
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



;;--------------------
;; Ido mode
;;--------------------
;;(require 'ido)
;;(ido-mode t)


;;--------------------
;; Yasnippet
;;--------------------
;; ;;(add-to-list 'load-path "~/.emacs.d/yasnippet")

;;(require 'yasnippet)
;;(yas-global-mode 0)
(autoload 'yasnippet "yasnippet" "yasnippet mode" t)

(eval-after-load "yasnippet"
  '(progn
     (yas-load-directory "~/.emacs.d/snippets") ;so custom snippets are not overwritten when updating from melpa.
     (setq yas/triggers-in-field nil) ;Enable/disable trigger of a sub-snippet while in a snippet.
     (defun my/yas-handle-param (param-str
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
         (apply #'concat decorated)))
     ))


;; (my/yas-handle-param "first, middle1, middle2, last"
;;                      ","
;;                      #'(lambda (x)
;;                          (upcase (concat "'" x "' - ")))
;;                      #'(lambda (f)
;;                          (downcase f))
;;                      #'(lambda (l)
;;                          (concat l "|")))
;; "'first' - ' MIDDLE1' - ' MIDDLE2' - ' LAST' - |"


;;--------------------
;; cc-mode
;;--------------------
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
(setq-default c-basic-offset 4) ;tab width
(setq-default c-electric-flag t)

;; `which-function-mode' is OK, but it turns on the mode globally for all buffers which is annoying.
;; And if you the functions fit on screen then it's just wasted modeline space.
;; As an alternative use `beginning-of-defun' C-M-a to jump to the function name.
;; Then `evil-jump-backward' C-o to jump back to where you were.
;;(eval-after-load 'cc-mode 'which-function-mode)

(add-hook 'c-mode-common-hook
          (lambda ()
            ;;(which-function-mode);;displays function at cursor in the mode-line. But can be annoying.
            (electric-pair-mode 1)
            ;;(flycheck-mode 1)
            ))

(add-hook 'c-initialization-hook
          (lambda ()
            ;;TODO: fill this up
            ))

;; (defun my/make-CR-do-indent ()
;;   (define-key c-mode-base-map "\C-m" 'c-context-line-break))
;; (add-hook 'c-initialization-hook 'my/make-CR-do-indent)


;;------------------
;; Dired
;;------------------
(setq-default dired-isearch-filenames t) ;search file names only in Dired.
(add-hook 'dired-mode-hook #'(lambda () (dired-hide-details-mode 1)))


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

;;---------------------
;; dired-details
;;---------------------
;; ;;allows collapsing the file details with "(" and ")" in emacs <= 24.3
;; (require 'dired-details)
;; (dired-details-install)



;;--------------------------------------------------------------------
;; sql-mode
;;--------------------------------------------------------------------
(add-hook 'sql-mode-hook #'electric-pair-mode)
(add-hook #'sql-mode-hook #'(lambda () (yas-minor-mode)))


;;--------------------------------------------------------------------
;; rainbow-delimiters
;;--------------------------------------------------------------------
(require 'rainbow-delimiters)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
(add-hook 'sql-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'sly-mrepl-mode-hook #'rainbow-delimiters-mode) ;(lambda () (rainbow-delimiters-mode-turn-on)))
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;;(global-rainbow-delimiters-mode)

;;--------------------------------------------------------------------
;; rainbow-mode
;;--------------------------------------------------------------------
;;(rainbow-mode)

;;--------------------------------------------------------------------
;; Expand-region
;; https://github.com/magnars/expand-region.el
;;--------------------------------------------------------------------
;;(require 'expand-region)
(autoload 'expand-region "expand-region" "expand region" t)
(global-set-key (kbd "C-=") #'er/expand-region)
(global-set-key (kbd "C--") #'er/contract-region)

;;--------------------------------------------------------------------
;; mulitple-cursors
;; https://github.com/magnars/multiple-cursors.el
;;--------------------------------------------------------------------
;(require 'multiple-cursors)
;(global-set-key (kbd "C--") 'mc/edit-lines)

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
;;(add-hook 'sly-mrepl-mode-hook (lambda () (paredit-mode +1)))
;;(add-hook 'sql-mode-hook #'enable-paredit-mode)
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; ;;key maps
;; (global-set-key (kbd "C-9") 'paredit-backward-slurp-sexp)
;; (global-set-key (kbd "C-0") 'paredit-forward-slurp-sexp)
;; (global-set-key (kbd "C-M-9") 'paredit-backward-barf-sexp)
;; (global-set-key (kbd "C-M-0") 'paredit-forward-barf-sexp)

;;--------------------------
;; smartparens
;;--------------------------
;;(require 'smartparens-config)

;;--------------------------
;; Omnisharp
;;--------------------------
(when (eq my/curr-computer 'work-laptop)
  ;; Example evil-mode config

  ;; (evil-define-key 'insert omnisharp-mode-map
  ;;   (kbd "C-SPC") 'omnisharp-auto-complete);C-Space like Visual Studio
  ;;(define-key omnisharp-mode-map (kbd "C-SPC") 'omnisharp-auto-complete) ;C-Space like Visual Studio

  (when (eq my/curr-computer 'work-laptop)
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
      (kbd "g R") 'omnisharp-rename))

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

  ;; Speed up auto-complete on mono drastically. This comes with the
  ;; downside that documentation is impossible to fetch.

  (setq omnisharp-auto-complete-want-documentation nil)

  (setq omnisharp--curl-executable-path "C:\\Users\\mtz\\programs\\curl-7.37.0-win64\\bin\\curl.exe")
  (setq omnisharp-server-executable-path "C:\\Users\\mtz\\programs\\OmniSharpServer\\OmniSharp\\bin\\Debug\\OmniSharp.exe")
  (setq omnisharp--windows-curl-tmp-file-path "C:\\Users\\mtz\\omnisharp-curl-tmp.cs") ;windows doesn't like the C:\ root folder
  (setq omnisharp-host "http://localhost:2000/");(setq omnisharp-host "http://localhost:2000/")
                                        ;(setq omnisharp-curl "curl.exe")
                                        ;`(:command ,omnisharp--curl-executable-path)

  (add-hook 'csharp-mode-hook 'omnisharp-mode)

  (let ((i-am-using-omnisharp nil))
    (when i-am-using-omnisharp
      (eval-after-load 'company
        '(add-to-list 'company-backends 'company-omnisharp))))

  (setq omnisharp-company-do-template-completion nil) ;tab completion of paramters. acts weird
  (setq omnisharp-company-ignore-case t)



  (defun my/start-omnisharp-server (sln)
    "Starts omnisharp server with the correct cmd line string."
    (interactive)
    (start-process-shell-command
     "Omni-Server"
     (get-buffer-create "*Omni-Server*")
     (concat omnisharp-server-executable-path " -p 2000 -s " sln))))


;;--------------------
;; nyan-mode
;;--------------------
;;(nyan-mode)
;;(setq nyan-wavy-trail nil)
;;(nyan-start-animation)

;;--------------------
;; nyan-prompt
;;--------------------
;;(add-hook 'eshell-load-hook 'nyan-prompt-enable)

;;--------------------
;; powerline  NOTE: powerline has an error on start up in emacs 24.4.50.1, even when all code is commented out. Deleting the elpa folder for now.
;;--------------------
;;(powerline-default-theme)
;;(powerline-center-theme)

;;--------------------
;; Ace jump mode
;;--------------------
;;(add-to-list 'load-path "/full/path/where/ace-jump-mode.el/in/")

;; your eye is already focused on the jump point so no need to gray background.
(setq ace-jump-mode-gray-background nil)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

;;--------------------
;; ace-window
;;--------------------
(global-set-key (kbd "M-w") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ;;home row

;;--------------------
;; ace-jump-zap
;;--------------------
;; (global-set-key (kbd "M-z") 'ace-jump-zap-to-char)

;;--------------------
;; clang-format
;;--------------------
;; rarely use `clang-format', so commenting it out for now.
;; (when (eq my/curr-computer 'work-laptop)
;;   (load "C:\\Users\\mtz\\programs\\LLVM\\share\\clang\\clang-format.el")
;;   ;;(global-set-key [C-M-tab] 'clang-format-region)
;;   (global-set-key (kbd "C-c f") 'clang-format-region)
;;   (global-set-key (kbd "C-c b") 'clang-format-buffer))

;;--------------------
;; irony
;;--------------------
(when (eq my/curr-computer 'work-laptop) ;TODO: set up on more machines.
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's asynchronous function
  (defun my/irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my/irony-mode-hook)

  (when (eq my/curr-computer 'work-laptop)
    ;;directory to libclang.dll
    (add-to-list 'exec-path "C:/Users/mtz/programs/LLVM/bin")))

;; Only needed on Windows
(when (eq system-type 'windows-nt)
  (setq w32-pipe-read-delay 0))

;;See also:
;; - https://github.com/Sarcasm/company-irony
;; - https://github.com/Sarcasm/ac-irony

;;-------------------------------
;; company-irony
;;-------------------------------
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))

;; ;; (optional) adds CC special commands to `company-begin-commands' in order to
;; ;; trigger completion at interesting places, such as after scope operator
;; ;;     std::|
;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;-------------------------------
;; Load projects
;;-------------------------------
(when (eq my/curr-computer 'work-laptop)
  ;; (defun proj-ecp ()
  ;;   (interactive)
  ;;   (let* ((root "C:\\Users\\mtz\\proj\\TFS\\SafetyWebsite\\Main\\Source\\")
  ;;          (sln (concat root "Safety.sln"))
  ;;          (defaultFile (concat root "Safety.WebUI\\Areas\\ECP\\Controllers\\ProcedureController.cs")))
  ;;     ;; helm-cmd-t stuff
  ;;     (add-to-list 'helm-cmd-t-find-prunes "obj")
  ;;     (add-to-list 'helm-cmd-t-find-prunes "bin")
  ;;     (add-to-list 'helm-cmd-t-find-prunes ".svn")
  ;;     (add-to-list 'helm-cmd-t-find-prunes "packages")
  ;;     (add-to-list 'helm-cmd-t-find-prunes "Safety.WebUI.Tests")
  ;;     (add-to-list 'helm-cmd-t-find-prunes "TestResults")
  ;;     (setq dir_ecp (helm-cmd-t-get-create-source-dir root))
  ;;     (evil-leader/set-key "h" (lambda ()
  ;;                                (interactive)
  ;;                                (helm :sources '(helm-source-buffers-list
  ;;                                                 dir_ecp)
  ;;                                      :buffer "*ECP Project*")))
  ;;     ;;(dired root)
  ;;     (find-file-existing defaultFile)
  ;;     ;;custom start of omnisharp. The commnad line string made by (omnisharp-start-omnisharp-server sln) doesn't work on my box.
  ;;     (my/start-omnisharp-server sln)

  ;;     ;;TODO: build ctags or etags.
  ;;     ;;(start-process-shell-command "makingCtags" nil "ctags -R -e *.cs")
  ;;     ))

  (defun proj-safetyweb ()
    (interactive)
    (let* ((root "C:\\Users\\mtz\\proj\\TFS\\SafetyWebsite\\Main\\Source\\")
           (sln (concat root "Safety.sln"))
           (defaultFile (concat root "Safety.WebUI\\Areas\\ECP\\Controllers\\ProcedureController.cs")))
      ;; helm-cmd-t stuff
      (add-to-list 'helm-cmd-t-find-prunes "obj")
      (add-to-list 'helm-cmd-t-find-prunes "bin")
      (add-to-list 'helm-cmd-t-find-prunes ".svn")
      (add-to-list 'helm-cmd-t-find-prunes "packages")
      (add-to-list 'helm-cmd-t-find-prunes "Safety.WebUI.Tests")
      (add-to-list 'helm-cmd-t-find-prunes "TestResults")
      (setq dir_ecp (helm-cmd-t-get-create-source-dir root))
      (evil-leader/set-key "h" (lambda ()
                                 (interactive)
                                 (helm :sources '(helm-source-buffers-list
                                                  dir_ecp)
                                       :buffer "*Saftey Web Project*")))
      (dired root)
      ;;custom start of omnisharp. The commnad line string made by (omnisharp-start-omnisharp-server sln) doesn't work on my box.
      ;;(my/start-omnisharp-server sln)
      ))

  (defun proj-trighist ()
    (interactive)
    (let* ((root "C:\\Users\\mtz\\proj\\HistoryImp\\dev\\code\\v3_GeneralHistory\\HistoryTriggerGen\\")
           (sln (concat root "HistoryTriggerGen.sln")))
      ;; helm-cmd-t stuff
      (add-to-list 'helm-cmd-t-find-prunes "obj")
      (add-to-list 'helm-cmd-t-find-prunes "bin")
      (add-to-list 'helm-cmd-t-find-prunes ".svn")
      (add-to-list 'helm-cmd-t-find-prunes ".git")
      (add-to-list 'helm-cmd-t-find-prunes "packages")
      (setq dir_triggerhist (helm-cmd-t-get-create-source-dir root))
      (evil-leader/set-key "h" (lambda ()
                                 (interactive)
                                 (helm :sources '(helm-source-buffers-list
                                                  dir_triggerhist)
                                       :buffer "*ECP Project*")))
      (dired root)
      ;;custom start of omnisharp. The commnad line string made by (omnisharp-start-omnisharp-server sln) doesn't work on my box.
      ;;(my/start-omnisharp-server sln)
      ))

  (defun proj-emacs ()
    (interactive)
    (let* ((emacs-root "C:\\Users\\mtz\\scratch\\emacs\\"))
      ;; helm-cmd-t stuff
      (add-to-list 'helm-cmd-t-find-prunes ".git")
      (setq dir_emacs (helm-cmd-t-get-create-source-dir emacs-root))
      (evil-leader/set-key "h" (lambda ()
                                 (interactive)
                                 (helm :sources '(helm-source-buffers-list
                                                  dir_emacs)
                                       :buffer "*Emacs Project*")))
      (dired emacs-root)))

  (defun proj-cl ()
    (interactive)
    ;; helm-cmd-t stuff
    (setq root_dir_cl (helm-cmd-t-get-create-source-dir "C:\\Users\\mtz\\scratch\\lisp"))
    (evil-leader/set-key "h" (lambda ()
                               (interactive)
                               (helm :sources '(helm-source-buffers-list
                                                root_dir_cl)
                                     :buffer "*Lisp Project*")))
    ;;load project
    (find-file-existing "C:\\Users\\mtz\\scratch\\lisp\\test.lisp")
    ;;(dired "C:\\Users\\mtz\\scratch\\lisp")
    (slime))

  (defun proj-imgtag ()
    (interactive)
    (let* ((root "C:\\Users\\mtz\\scratch\\ImgDragAndDrop\\")
           (html (concat root "test.html"))
           (css (concat root "test.css"))
           (js (concat root "test.js")))
      (find-file-existing css) (split-window)
      (find-file-existing js) (split-window)
      (find-file-existing html)
      (evil-window-move-far-left)
      (shrink-window-horizontally 35)))

  (defun proj-cpp ()
    (interactive)
    (setq root_dir_cpp (helm-cmd-t-get-create-source-dir "C:\\Users\\mtz\\scratch\\cpp"))
    (evil-leader/set-key "h" (lambda ()
                               (interactive)
                               (helm :sources '(helm-source-buffers-list
                                                root_dir_cpp)
                                     :buffer "*Cpp Project*")))
    (dired "C:\\Users\\mtz\\scratch\\cpp")
    ;;(start-process-shell-command "makingCtags" nil "ctags -R -e *.cpp")
    ))

;;; quick load of the .emacs (or init.el) file.
(evil-leader/set-key "`" (lambda ()
                           (interactive)
                           (find-file-existing "~/.emacs.d/init.el")))

(when (eq my/curr-computer 'work-laptop)
  ;;quick load of c:\users\mtz
  (evil-leader/set-key "1" (lambda ()
                             (interactive)
                             (dired "C:\\Users\\mtz"))))

(when (eq my/curr-computer 'work-laptop)
  ;;quick load of c:\users\mtz\proj\ecp\dev\db
  (evil-leader/set-key "2" (lambda ()
                             (interactive)
                             (dired "c:\\users\\mtz\\proj\\ecp\\dev\\db"))))

(when (eq my/curr-computer 'work-laptop)
  ;;quick load of TFS \Main\SqlScripts
  (evil-leader/set-key "3" (lambda ()
                             (interactive)
                             (dired "C:\\Users\\mtz\\proj\\TFS\\SafetyWebsite\\Main\\DbScripts"))))


;;-----------------------------------------------------------------------------
;; VC version control
;;-----------------------------------------------------------------------------
;; (defadvice vc-dir (before ensure-excluded-dirs)
;;   "add to the excluded dir list. It's not working if I add in init.el"
;;   (add-to-list 'vc-directory-exclusion-list "bin")
;;   (add-to-list 'vc-directory-exclusion-list "obj"))
;; (ad-activate 'vc-dir)

(eval-after-load "vc"
  '(progn
     (add-to-list 'vc-directory-exclusion-list "bin")
     (add-to-list 'vc-directory-exclusion-list "obj")))

;; (add-hook 'vc-- (lambda () (linum-mode 0)))

;;-----------------------------------------------------------------------------
;; Projectile
;;-----------------------------------------------------------------------------
;; (require 'projectile)
;; (projectile-global-mode)
;; (setq projectile-indexing-method 'native)
;; (setq projectile-enable-caching t)
;; (define-key projectile-mode-map (kbd "C-x C-b") 'projectile-ibuffer)

;;--------------------
;; icicles
;;--------------------
;; (require 'icicles) ; Load this library.
;; (icicle-mode 1)    ; Turn on Icicle mode.

;;-----------------------------------------------------------------------------
;; web-mode
;;-----------------------------------------------------------------------------
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


;;-----------------------------------------------------------------------------
;; vimrc-mode
;;-----------------------------------------------------------------------------
;;(require 'vimrc-mode)
(autoload 'vimrc-mode "vimrc-mode" "vimrc mode" t)
(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))

;;-----------------------------------------------------------------------------
;; Make dired appear in a side window
;;-----------------------------------------------------------------------------
(defun my/current-file-path ()
  "Returns the full file path of the current buffer as a string"
  (interactive)
  (or load-file-name
      buffer-file-name))

(defun my/current-folder-path ()
  "Returns the folder path of the current buffer as a string"
  (interactive)
  (file-name-directory (my/current-file-path)))

(defun my/folder-nav ()
  "Opens a dired buffer. Dired does all the actual work. This just handles the visual aspects like window placement and size."
  (interactive)
  (dired-other-window (my/current-folder-path))
  (evil-window-move-far-left)
  ;;I can't find a function to set the exact window size, so collapsing the buffer then enlarging to the size I want.
  (let ((bigNumToCollapse 500)
        (width 65))
    (shrink-window-horizontally bigNumToCollapse)
    (enlarge-window-horizontally (- width
                                    (window-total-width)))))
;;bind to key
(global-set-key (kbd "<f8>") 'my/folder-nav)


;;-----------------------------------------------------------------------------
;; skewer-mode
;;-----------------------------------------------------------------------------
;;(skewer-setup)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)
(add-hook 'web-mode-hook 'skewer-html-mode)

(defun my/skewer-repl-clear-buffer ()
  "Deletes the contents of the skewer-reple buffer.
Depends on evil mode."
  (interactive)
  (evil-goto-line) ;bottom
  (evil-previous-visual-line) ;up 1
  (evil-end-of-line)
  (delete-region 1 (+ (point) 2))
  (evil-end-of-line))

(add-hook
 'skewer-repl-mode-hook
 (lambda ()
   ;;turn off line numbers in the repl
   (linum-mode 0)
   ;;there's always a trailing space at repl prompt. Don't highlight it. 
   (setq show-trailing-whitespace nil)
   (define-key skewer-repl-mode-map (kbd "C-c M-o") 'my/skewer-repl-clear-buffer)))

(require 'simple-httpd)
(defun my/skewer-html ()
  "Wire up the html file you're editing with skewer."
  (interactive)
  ;;(skewer-html-mode) ; this is set in a hook, don't need it here.
  ;;(setq httpd-root "c:\\users\\mtz\\scratch\\testwebsite")
  (setq httpd-root (my/current-folder-path))
  (httpd-start)
  (browse-url-of-file (concat "http://localhost:8080/"
                              (file-name-nondirectory buffer-file-name)))
  (run-skewer)
  (message "put this in the <head>: <script src=\"http://localhost:8080/skewer\"></script> --- switch to tab http://localhost:8080/FileOrRouteName, then start evaling html"))

;;-----------------------------------------------------------------------------
;; eshell
;;-----------------------------------------------------------------------------
(defun my/eshell-clear-buffer ()
  "Deletes the contents of eshell buffer, except the last prompt"
  (interactive)
  (save-excursion
    (goto-char eshell-last-output-end)
    (let ((lines (count-lines 1 (point)))
          (inhibit-read-only t))
      (beginning-of-line)
      (let ((pos (point)))
        (if (bobp)
            (if (interactive-p)
                (error "Buffer too short to truncate"))
          (delete-region (point-min) (point))
          (if (interactive-p)
              (message "Buffer cleared")))))))

(defun my/eshell-clear-line ()
  (interactive)
  ;;(message "") ;delete multiple lines of junk in the mini buffer.
  (eshell-bol)
  (evil-delete-line)
  ;;(message "") ;delete multiple lines of junk in the mini buffer.
  )

;;set up custome keybindings when the mode loads.
(add-hook 'eshell-mode-hook
          (lambda ()
            ;;Use the same keybinding to clear eshell as the SLIME repl
            (define-key eshell-mode-map (kbd "C-c M-o") 'my/eshell-clear-buffer)
            ;;make evil's dd compatible with the read-only prompt of hte current line.
            ;;(define-key evil-normal-state-map (kbd "<remap> <evil-delete-whole-line>") 'my/eshell-clear-line)
            ;;(evil-define-key 'normal eshell-mode-map (kbd "d d") 'my/eshell-clear-line)
            ))

;;-----------------------------------------------------------------------------
;; highlight-tail
;;-----------------------------------------------------------------------------
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

;;-----------------------------------------------------------------------------
;; eww web-browser
;;-----------------------------------------------------------------------------
;;(setq browse-url-browser-function 'eww-browse-url) ;;make default for opening links.

(when (eq my/curr-computer 'work-laptop)
  (setq eww-download-directory "C:\\Users\\mtz\\Downloads"))

(add-hook 'eww-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)
            (define-key eww-mode-map (kbd "C-c h") 'eww-back-url)
            (define-key eww-mode-map (kbd "C-c l") 'eww-forward-url)
            (define-key eww-mode-map (kbd "C-c r") 'eww-reload)
            (define-key eww-mode-map (kbd "C-c g") 'eww)
            (define-key eww-mode-map (kbd "<tab>") 'shr-next-link)))


;;-----------------------------------------------------------------------------
;; cedet
;;-----------------------------------------------------------------------------
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

;;-----------------------------------------------------------------------------
;; cider
;;-----------------------------------------------------------------------------
;;"C:\Program Files (x86)\Java\jre7\bin\java" -cp clojure-1.6.0.jar clojure.main

;;-----------------------------------------------------------------------------
;; aggressive-indent
;;-----------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'js2-mode-hook #'aggressive-indent-mode)
;;(add-hook 'slime-repl-mode-hook #'aggressive-indent-mode)
;;(global-aggressive-indent-mode 1)
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
;; (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)


;;-----------------------------------------------------------------------------
;; magit
;;-----------------------------------------------------------------------------
(evil-leader/set-key "m" #'magit-status)


;;-----------------------------------------------------------------------------
;; ediff
;;-----------------------------------------------------------------------------
(setq ediff-split-window-function 'split-window-horizontally)
;; don't use the popup window
(setq ediff-window-setup-function 'ediff-setup-windows-plain);'ediff-setup-windows-multiframe

;;-----------------------------------------------------------------------------
;; helm-w32-launcher. Microsoft Windows only?
;;-----------------------------------------------------------------------------
(when (eq system-type 'windows-nt)
  (global-set-key (kbd "C-c w") 'helm-w32-launcher))

;;-----------------------------------------------------------------------------
;; leerzeichen. Displays symbols for tab, space, and newline.
;;-----------------------------------------------------------------------------
(require 'leerzeichen)
;;(leerzeichen-mode)
;; (custom-set-faces
;;  '(leerzeichen ((t (:foreground "black";"#A8A800"
;;                                 :background "white";"#D4D4C8"
;;                                 :italic nil
;;                                 :bold nil
;;                                 ;;:box t
;;                                 )))))

;;------------------------------------------------------------------------------
;; sql-indent
;;------------------------------------------------------------------------------
;; (eval-after-load "sql"
;;   '(load-library "sql-indent"))

;;------------------------------------------------------------------------------
;; darkroom
;;------------------------------------------------------------------------------
;; (require 'darkroom)
;; (setq darkroom-margins 0.15)
;; (setq darkroom-fringes-outside-margins nil) ;;nil keeps margins close to the centered text.

;;-----------------------------------------------------------------------------
;; vim-empty-lines-mode
;;-----------------------------------------------------------------------------
;;(global-vim-empty-lines-mode) ; messes up recenter-top-bottom so not using for now.

;;------------------------------------------------------------------------------
;; fill-column-indicator
;;------------------------------------------------------------------------------
(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-width 1)
(progn
  (setq fci-dash-pattern 0.5) ;length of the dash 0 to 1
  (setq fci-rule-use-dashes t))
(setq fci-rule-color "#555555") ;;tailored for zenburn ATM.
(add-hook 'prog-mode-hook #'(lambda ()
                              (fci-mode 1)))

(defun my/fci-refresh ()
  (interactive)
  (fci-mode 0)
  (fci-mode 1))

;; ;;make fci compatible with emacs built-in variable `show-trailing-whitespace'
;; ;;TODO: it doesn't seem to be working!
;; ;;TODID: used "white-space-mode" instead of `show-trailing-whitespace'.
;; (setq whitespace-style '(face trailing))   

;;------------------------------------------------------------------------------
;; flycheck
;;------------------------------------------------------------------------------
(add-hook 'flycheck-mode-hook
          (lambda ()
            (define-key flycheck-mode-map (kbd "M-n") #'flycheck-next-error)
            (define-key flycheck-mode-map (kbd "M-p") #'flycheck-previous-error)
            ;;(evil-define-key 'flycheck-mode-map (kbd "M-n") #'flycheck-next-error)
            ))

;;------------------------------------------------------------------------------
;; hydra
;;------------------------------------------------------------------------------
(setq hydra-is-helpful t)
;; don't use window for hints. It seems to lock things up.
;; And window switcher mode really gets messed up.
(setq hydra-lv nil)

;; (defhydra hydra-zoom (global-map "<f2>")
;;   "zoom"
;;   ;; The property name ":color" is misleading.
;;   ;; :color blue makes hydra-mode exit after execution, like evil-leader.
;;   ;; :color red stays in mode.
;;   ("i" text-scale-increase "in" :color red)
;;   ("o" text-scale-decrease "out" :color blue))

;; (defhydra hydra-leader (evil-normal-state-map "\\")
;;   "cmd"
;;   ("h" backward-char)
;;   ("j" next-line)
;;   ("k" previous-line)
;;   ("l" forward-char)
;;   ("x" eval-defun "evalD")
;;   ("e" eval-last-sexp "eval"))

;; (global-set-key
;;  (kbd "C-M-o")
;;  (defhydra hydra-window ;;()
;;    (
;;     ;; :pre
;;     ;; (set-cursor-color "purple")
;;     ;; :post
;;     ;; (set-cursor-color "green")
;;     :color amaranth ;keep the hydra active when a unbound key is accidentally pressed.
;;            )
;;    "window"
;;    ("h" windmove-left)
;;    ("j" windmove-down)
;;    ("k" windmove-up)
;;    ("l" windmove-right)
;;    ("H" (lambda ()
;;           (interactive)
;;           (enlarge-window-horizontally 15)))
;;    ("J" (lambda ()
;;           (interactive)
;;           (shrink-window 10)))
;;    ("K" (lambda ()
;;           (interactive)
;;           (enlarge-window 10)))
;;    ("L" (lambda ()
;;           (interactive)
;;           (shrink-window-horizontally 15)))
;;    ("e" evil-window-split) ;keeps sizes balanced as it splits like in Vim.
;;    ("E" evil-window-vsplit);keeps sizes balanced as it splits like in Vim.
;;    ("S" (lambda ()
;;           (interactive)
;;           (split-window-right)
;;           (windmove-right))
;;     "vert")
;;    ("s" (lambda ()
;;           (interactive)
;;           (split-window-below)
;;           (windmove-down))
;;     "horz")
;;    ("d" (lambda ()
;;           (interactive)
;;           (delete-window))
;;     "del")
;;    ("o" other-window)
;;    ("n" next-buffer)
;;    ("p" previous-buffer)
;;    ("b" balance-windows)
;;    ;;("K" kill-this-buffer)
;;    ("x" maximize-window "max")
;;    ("," delete-other-windows "one")
;;    ("q" nil "cancel") ;nil for function is an automatic blue head.
;;    ))

;; horizontal scroll test 3333-=--------------------------------------------------------------------------------------------------aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBCCCCCCCCCcccccccccccccccccccccccccccccccccccccDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;;easy 1 key scrolling
(defhydra hydra-easyscroll (:color amaranth)
  "scroll"
  ("v" scroll-up-command)
  ("M-v" scroll-down-command)
  ("f" scroll-right)
  ("b" scroll-left)
  ("," beginning-of-buffer)
  ("." end-of-buffer)

  ("j" evil-scroll-line-down)
  ("k" evil-scroll-line-up)
  ("h" evil-scroll-column-left 10)
  ("l" evil-scroll-column-right 10)

  ("C-g" nil nil)
  ("q" nil))

;; avoid moving hand to arrow keys for barf/slurp
(defhydra hydra-paredit ()
  "paredit"
  ("h" paredit-forward-barf-sexp)
  ("l" paredit-forward-slurp-sexp)
  ("H" paredit-backward-slurp-sexp)
  ("L" paredit-backward-barf-sexp)
  ;; ("f" paredit-forward)
  ;; ("b" paredit-backward)
  ("\\" nil)
  ("q" nil))
(add-hook 'paredit-mode-hook
          (lambda ()
            (define-key paredit-mode-map (kbd "C-c p") #'hydra-paredit/body)))
;;(key-chord-define evil-normal-state-map "c," #'hydra-paredit/body)


(defhydra hydra-window ;;()
  (;; :pre ;;executes before each head.
   ;; (progn (message "executed pre")
   ;;        (my/cycle-light-bg))
   ;; :post ;;executes on exit from body, not exit from a head.
   ;; (progn (message "executed post")
   ;;        (my/cycle-light-bg))
   :color amaranth ;keep the hydra active when a unbound key is accidentally pressed.
          )
  "window"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("H" evil-window-move-far-left)
  ("J" evil-window-move-very-bottom)
  ("K" evil-window-move-very-top)
  ("L" evil-window-move-far-right)
  (">" (lambda ()
         (interactive)
         (enlarge-window-horizontally 1)))
  ("<" (lambda ()
         (interactive)
         (shrink-window-horizontally 1)))
  ("," (lambda ()
         (interactive)
         (shrink-window 1)))
  ("." (lambda ()
         (interactive)
         (enlarge-window 1)))
  ("e" evil-window-split) ;keeps sizes balanced as it splits like in Vim.
  ("E" evil-window-vsplit);keeps sizes balanced as it splits like in Vim.
  ("v" evil-window-new)
  ("V" evil-window-vnew)
  ("r" evil-window-rotate-upwards)
  ("R" evil-window-rotate-downwards)
  ("S" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right))
   "vert")
  ("s" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down))
   "horz")
  ("d" (lambda ()
         (interactive)
         (delete-window))
   "del")
  ("o" other-window)
  ("n" next-buffer)
  ("p" previous-buffer)
  ("b" balance-windows)
  ;;("K" kill-this-buffer)
  ("x" maximize-window "max")
  ("X" delete-other-windows "only")
  ("C-g" nil nil)
  ("\\" nil)
  ("q" nil "quit") ;nil for function is an automatic blue head.
  )
;; (define-key evil-normal-state-map (kbd "\\") 'hydra-window/body)
;; (define-key evil-motion-state-map (kbd "\\") 'hydra-window/body)
;; (eval-after-load "magit"
;;   '(progn
;;      (define-key magit-mode-map (kbd "\\") 'hydra-window/body)))
;;(evil-define-key 'emacs magit-mode-map (kbd "\\") 'hydra-window/body)

;; (defhydra helm-like-unite ()
;;   "vim movement"
;;   ("?" helm-help "help")
;;   ("<escape>" keyboard-escape-quit "exit")
;;   ("<SPC>" helm-toggle-visible-mark "mark")
;;   ("a" helm-toggle-all-marks "(un)mark all")
;;   ;; not sure if there's a better way to do this
;;   ("/" (lambda ()
;;          (interactive)
;;          (execute-kbd-macro [?\C-s]))
;;    "search")
;;   ("v" helm-execute-persistent-action)
;;   ("g" helm-beginning-of-buffer "top")
;;   ("G" helm-end-of-buffer "bottom")
;;   ("j" helm-next-line "down")
;;   ("k" helm-previous-line "up")
;;   ("q" helm-keyboard-quit) ;exit helm in 1 step
;;   ("i" nil "cancel"))
;; (define-key helm-map (kbd "<escape>") 'helm-like-unite/body)


(progn ;;spawn hydras from a single binding. A hydra of hydras.
  (setq *my-hydras* (mapcar #'symbol-name
                            (list #'hydra-easyscroll/body
                                  #'hydra-window/body
                                  #'hydra-paredit/body)))
  (defun my/choose-hydra ()
    (interactive)
    (funcall (intern (completing-read "pick one: " *my-hydras*))))
  (define-key evil-normal-state-map (kbd "\\") #'my/choose-hydra)
  (define-key evil-motion-state-map (kbd "\\") #'my/choose-hydra))

;;------------------------------------------------------------------------------
;; erc
;;------------------------------------------------------------------------------
(add-hook 'erc-mode-hook #'(lambda ()
                             (setq show-trailing-whitespace nil)))
;;------------------------------------------------------------------------------
;; linum-relative
;;------------------------------------------------------------------------------
;; (when nil ;disable for now. Makes the screen blink when line # changes.
;;   (setq linum-relative-format "%2s") ;rel numbers should never exceed 2 digits.
;;   (setq linum-relative-current-symbol "0")
;;   (require 'linum-relative);linum-mode's behavior is changed by the linum-relative package.
;;   ;;(linum-relative-toggle) ;;toggle between realtive and straight.
;;   )

;;------------------------------------------------------------------------------
;; guide-key
;;------------------------------------------------------------------------------
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
;; (guide-key-mode 1)

;; (setq guide-key/recursive-key-sequence-flag t)
;; ;;(setq guide-key/popup-window-position 'bottom)


;;------------------------------------------------------------------------------
;; unkillable-scratch
;;------------------------------------------------------------------------------
(unkillable-scratch 1)


;;------------------------------------------------------------------------------
;; bookmarks
;;------------------------------------------------------------------------------
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


;;------------------------------------------------------------------------------
;; Misc options. Keep this at the bottom
;;------------------------------------------------------------------------------
(defun what-face (pos)
  "Prints the face at point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(defmacro C-u (&rest args)
  "Makes it easier to programmatically call a function with C-u prefix.
Gotten from #emacs on freenode."
  (let ((prefix (list 4)))
    (while (cdr args)
      (cond
       ((eq (car args) 'C-u)
        (setf (car prefix) (* 4 (car prefix))))
       ((eq (car args) 'M-x)
        ;; ignore
        t)
       (t
        (error "Unknown arg %S" (car args))))
      (setq args (cdr args)))
    (unless (functionp (car args))
      (error "%S is not a function" (car args)))
    `(lambda ()
       (interactive)
       (let ((current-prefix-arg ',prefix))
         (call-interactively ',(car args))))))
;;(global-set-key (kbd "<f12>") (C-u M-x org-refile))

(put 'narrow-to-region 'disabled nil)

(progn ;;use the default emacs scroll bingding for C-v
  (define-key evil-normal-state-map (kbd "C-v") #'scroll-up-command)
  (define-key evil-motion-state-map (kbd "C-v") #'scroll-up-command))


(progn ;;window navigation.
  (global-set-key (kbd "M-h") #'evil-window-left)
  (global-set-key (kbd "M-j") #'evil-window-down)
  (global-set-key (kbd "M-k") #'evil-window-up)
  (global-set-key (kbd "M-l") #'evil-window-right))

;; cycle the buffers really fast. Not doing this anymore since these are error handling shortcuts in some modes.
;; (global-set-key (kbd "M-n") #'next-buffer)
;; (global-set-key (kbd "M-p") #'previous-buffer)


(cond
 ((eq my/curr-computer 'work-laptop)
  (setq browse-url-generic-program "C:\\Program Files (x86)\\conkeror\\conkeror.exe"
        ;;browse-url-generic-program "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe"
        browse-url-browser-function 'browse-url-generic))

 ((eq my/curr-computer 'a-laptop-faster)
  (setq browse-url-generic-program "conkeror"
        browse-url-browser-function 'browse-url-generic))

 ((or (eq my/curr-computer 'raspberry-pi)
      (eq my/curr-computer 'utilite))
  (setq browse-url-generic-program "surf"
        browse-url-browser-function 'browse-url-generic)))


;; (defun my/insert-img ()
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


(defun insert-date-string ()
  "Insert a date string. Everything you need to know about the date and time."
  (interactive)
  (insert
   (format-time-string
    "%Y-%m-%d (Numerical)%n%m-%d-%Y (USA)%n%A %B %e, %Y%n%I:%M%P%nsecond: %S.%3N")))
(global-set-key (kbd "C-c i") #'insert-date-string)

;; Only browse interesting buffers. Not *scratch*, *messages*, etc.
;;(global-set-key "\C-x\C-b" 'bs-show)

;;ibuffer. the way C-x C-b should be.
(global-set-key (kbd "C-x C-b") 'ibuffer)

(defun now ()
  (interactive)
  (format-time-string "%b %a %m-%d-%Y %I:%M %p"))
(defun now-minibuffer ()
  (interactive)
  (message (now)))
(defun now-put-in-buffer ()
  (interactive)
  (insert (now)))


;;(global-linum-mode 0) ;show/hide line numbers in margin

(setq-default column-number-mode nil) ;show/hide column # in mode line.
;;show/hide line # in mode line. Use fn what-line
(setq-default line-number-mode t)
;; do not display modes in the mode-line. They take up too much space.
;; Function `describe-mode' (kbd "C-h m") is better to see active modes anyway.
(setq mode-line-modes nil)
;;(setq mode-line-position nil) ;hide the % of the buffer you are viewing.

(when (display-graphic-p)
  ;;Don't waste mode line space displaying Evil-states.
  ;;Cursor style/color is sufficient to determine mode.
  ;;but if in a terminal without cursor styles then allow it to exist.
  (setq evil-mode-line-format nil))


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
  (unless (eq my/curr-computer 'raspberry-pi)
    (global-prettify-symbols-mode 1)))

;;indent keyword args properly. Use common lisp-style for (if) indendation too?
;;(setq lisp-indent-function 'common-lisp-indent-function)

(setq inhibit-startup-message t)
;;(setq initial-scratch-message ";; Scratch buffer ;;\n\n\n\n")
(setq initial-scratch-message "\n\n\n\n\n")

(blink-cursor-mode 0)
(hl-line-mode 0)

(global-auto-revert-mode t) ;;reload buffer if it changes on disk outside emacs.

(setq-default line-spacing nil)


;;use tilde's as the fringe graphic for empty lines. Like Vim.
(progn
  (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
  (set-fringe-bitmap-face 'tilde 'font-lock-comment-face))

(setq-default indicate-empty-lines t) ;Like vim's tildes
;; (setq-default indicate-buffer-boundaries '((up . nil) (down . nil)
;;                                            (top . left) (bottom . left)))

;;(setq tool-bar-mode nil)
(setq-default transient-mark-mode t)  ;show selected regions
;;(setq-default visible-bell t)
(setq ring-bell-function 'ignore)
;;(show-paren-mode 0)

(progn ;;tab handling
  (setq-default indent-tabs-mode nil) ;;Use only spaces, no tabs.
  (setq-default tab-width 4)
  (setq-default indent-line-function 'insert-tab))

(setq make-backup-files nil backup-inhibited t) ;No annoying backup files
(setq auto-save-default nil) ;No annoying auto-save files
;; Don't echo passwords when dealing with interactive programs
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;;show trailing whitespace.
;; (add-hook 'prog-mode-hook #'(lambda ()
;;                               (setq show-trailing-whitespace t)))

;; (defun my/toggle-show-trailing-whitespace ()
;;   (interactive)
;;   (not-m show-trailing-whitespace)
;;   ;;visual state makes the dipslay refresh.
;;   (evil-visual-char)
;;   (evil-exit-visual-state))
;; (global-set-key (kbd "C-c t") #'my/toggle-show-trailing-whitespace)
;; (global-set-key (kbd "C-c C-t") #'my/toggle-show-trailing-whitespace)


;;******** whitespace-mode *******
(require 'whitespace)
(setq-default whitespace-line-column 80)
;;(setq whitespace-style '(face lines-tail))
(setq-default whitespace-style '(face trailing))
(add-hook 'prog-mode-hook #'(lambda ()
                              (whitespace-mode 1)))
;;(global-whitespace-mode 1)

;;-------------------------------------------------------aaaaaaaaaaaaaaaaaaa

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


(setq my/keep-buffers '("*scratch*" "*Messages*" "*Compile-Log*" "*Minibuf-1*"
                        "*Minibuf-0*" "*code-conversion-work*" "*Echo Area 0*"
                        "*Echo Area 1*" "*helm mini*"))
(defun square-one ()
  "Switch to the scratch buffer, then delete all other buffers.

NOTE: `my/keep-buffers' contains buffers to keep alive.
Emacs tends to crash when some of the basic buffers are absent.
I'm not certain which absences cause the crash.

It seems killing buffers gives cleanup of other things for free!
ie closing running processes (slime/swank, omnisharp, etc) and helm-cmd-t
caches.
TODO: look into an explicit way to clean up non-buffer things in case there are
edge cases not covered by buffer killing."
  (interactive)
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  ;;cl-set-difference does not work on strings.
  ;;so use a set of buffer pointers, not buffer names
  (let ((to-kill (cl-set-difference (buffer-list)
                                    (mapcar 'get-buffer my/keep-buffers))))
    (mapc 'kill-buffer to-kill)))

(evil-leader/set-key "0" 'square-one)


(when (and nil ;don't start server for now.
           ;;`server-start' doesn't seemt to work on MS-windows?
           (eq system-type 'gnu/linux))
  (server-start))


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
;;                           'my/history)))
;;     (message "you said: %s" txt)))

;; ;---------------------------------------------------
;; ; stuff
;; ;---------------------------------------------------
;; (random t) ;seed random with time

;; (defun rand (min max) ;TODO: use a custom implementation for speed.
;;   (+ min
;;      (random (+ 1
;;                 (- max min)))))
;; (defun roll ()
;;   (rand 1 1000))

;; (defun rollCheck (val)
;;   (>= val (roll)))

;; (setq msgDb '("hi"))

;; (setq msgIndex 0)

;; (defun msg ();Use random once database is large enough to rarely get a dupe.
;;   "Display a message."
;;   (interactive)
;;   (let* ((max (- (length msgDb) 1))
;;          (msg (my/getAtIndex msgIndex msgDb)))
;;     (setq msgIndex (+ 1 msgIndex))
;;     (when (> msgIndex max)
;;       (setq msgIndex 0))
;;     (clippy-say msg)))

;; ;; (defun msg ()
;; ;;   "Display a random message."
;; ;;   (interactive)
;; ;;   (let* ((max (- (length msgDb) 1))
;; ;;          (i (rand 0 max))
;; ;;          (msg (my/getAtIndex i msgDb)))
;; ;;     ;(message msg)
;; ;;     (clippy-say msg)
;; ;;     ;(clippy-say (yow))
;; ;;     ))

;; (evil-leader/set-key "m" 'msg)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; touch typing
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun my/type-tutor ()
;;   (interactive)
;;   (my/type-tutor-defaults
;;    (list "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ;top row
;;          "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ;Shift top row
;;          "[" "]" "{" "}")))

;; (defun my/type-tutor-defaults (char-lst)
;;   (let ((lines 7)
;;         (groups-per-line 7)
;;         (group-size 9))
;;     (my/type-tutor-insert-buffer char-lst lines groups-per-line group-size)))

;; (defun my/type-tutor-insert-buffer (chars lines groups-per-line group-size)
;;   (save-excursion
;;     (let ((chars-len (- (length chars) 1))
;;           (l 0))
;;       (while (< l lines)
;;         (let ((gpl 0))
;;           (while (< gpl groups-per-line)
;;             (let ((gs 0))
;;               (while (< gs group-size)
;;                 (insert (my/getAtIndex (rand 0 chars-len)
;;                                        chars))
;;                 (incf gs)))
;;             (insert " ")
;;             (incf gpl)))
;;         (insert "\n\n\n")
;;         (incf l))))
;;   (next-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hour format conversion. 12 -> 24
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interact with Microsoft SQL Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;sqlcmd -S 127.0.0.1,42000\OSHE
;;sqlcmd -S 127.0.0.1,42000\OSHE -q "SELECT 'hello';"

;;; init.el ends here
