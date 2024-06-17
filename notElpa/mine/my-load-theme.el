;;; -*- lexical-binding: t -*-

(require 'my-color-theme-mods) ; for fn `my-get-theme'

;;custom-enabled-themes
;;custom-safe-themes
;;custom-known-themes
;;(custom-available-themes)
(let ((i 0))
  (defun my-cycle-theme ()
    (interactive)
    (let* ((themes (custom-available-themes))
           (len (length themes))
           (thm (nth i themes)))
      (unwind-protect
          (progn
            (load-theme thm t))
        (progn
          (message "theme %d/%d %s"
                   (1+ i) len (symbol-name thm))
          (setq i (1+ i))
          (when (= i len)
            (setq i 0)))))))


(defun my-force-light-bg ()
  "Force Emacs to think the bg is light. Affects some faces that are different
when Emacs thinks the bg is light. By default Emacs treats Ivory4 as dark, but
I want it to be considered light."
  (interactive)
  (let ((frame-background-mode 'light))
    (mapc 'frame-set-background-mode (frame-list))))


(declare-function my-cycle-light-bg-set-index "my-load-theme")
(defvar mayan-smoke)
(defvar my-ultimate)
;; (defvar cycle-colors2 '("papaya whip" "old lace" "floral white" "ivory2"
;;                         "mint cream" "honeydew" "white smoke" "ghost white"
;;                         "snow" "alice blue" "lavender"))

;;        format (color . description)
(let* ((colors `[("old lace" . "old lace")
                 ("floral white" . "floral white" )
                 ("snow" . "snow")
                 ("ghost white" . "ghost white")
                 ("white smoke" . "white smoke")
                 ("white" . "white")
                 ("#F3F1DE" . "my bg 1")
                 ("#F3F2EA" . "my bg 2")
                 (,mayan-smoke . "myan-smoke")
                 (,my-ultimate . "my-ultimate1")
                 ("#D5D1B3" . "my-ultimate2")
                 ("#E5E1C3" . "my-ultimate3")
                 ("#F5F1D3" . "my-ultimate4")
                 ("ivory" . "ivory")
                 ("ivory2" . "ivory2")
                 ("ivory3" . "ivory3")     ; #cdcdc1
                 ("#adada1" . "ivory3.5")  ; #adada1
                 ("#9d9d91" . "ivory3.75") ; #9d9d91
                 ("ivory4" . "ivory4")])   ; #8b8b83
       (len (length colors))
       (i 0))

  (defun my-cycle-light-bg-set-index (new-i)
    (interactive "n") ; read new index from minibuffer if not provided.
    (setq i (if (or (>= new-i len)
                    (< new-i 0))
                (mod new-i len) ; if i out of range then wrap via mod.
              new-i))
    (let* ((pair (aref colors i))
           (bg (car pair))
           (descr (cdr pair)))
      ;; (set-background-color bg)
      (custom-theme-set-faces
       (my-get-theme)
       ;; TODO: hardcoding :foreground "black" a temporary fix for iterm2
       ;; where foreground becomes a messed up super light color. look into proper
       ;; fix later, just getting it to work on iterm2 for now.
       `(default ((t :foreground "black" :background ,bg))))
      (my-force-light-bg)
      (message (format "color %d/%d. %s, %s"
                       (1+ i) len bg descr))))

  (defun my-cycle-light-bg (step)
    (interactive "n") ; read `step' from minibuffer if not provided.
    (my-cycle-light-bg-set-index (+ i step)))

  (defun my-cycle-light-bg-forward ()
    (interactive)
    (my-cycle-light-bg 1))

  (defun my-cycle-light-bg-backward ()
    (interactive)
    (my-cycle-light-bg -1)))




(declare-function my-cycle-dark-bg "my-load-theme")
(declare-function my-cycle-dark-bg-set-index "my-load-theme")
(declare-function my-cycle-light-bg "my-load-theme")
(let* ((colors [("#35352B" . "charcoal")
                ("#25251B" . "charcoal2")
                ("#15150B" . "charcoal3")
                ("#000000" . "black")
                ("#262626" . "charcoal256")
                ("#3F3F3F" . "zenburn")
                ("#24221c" . "desert-night") ; vim
                ("#282828" . "gruvbox")]) ; vim
       (len (length colors))
       (i 0))

  (defun my-cycle-dark-bg-set-index (new-i)
    (interactive "n") ; read new index from minibuffer if not provided.
    (setq i (if (or (>= new-i len)
                    (< new-i 0))
                (mod new-i len) ; if i out of range then wrap via mod.
              new-i))
    (let* ((pair (aref colors i))
           (bg (car pair))
           (descr (cdr pair)))
      ;; (set-background-color bg)
      (custom-theme-set-faces
       (my-get-theme)
       ;; TODO: hardcoding :foreground as temporary fix for iterm2
       ;; where foreground gets messed up. look into proper
       ;; fix later, just getting it to work on iterm2 for now.
       `(default ((t :foreground "#EEEED1" :background ,bg))))
      (message (format "color %d/%d. %s, %s"
                       (1+ i) len bg descr))))

  (defun my-cycle-dark-bg (step)
    (interactive "n") ; read `step' from minibuffer if not provided.
    (my-cycle-dark-bg-set-index (+ i step)))

  (defun my-cycle-dark-bg-forward ()
    (interactive)
    (my-cycle-dark-bg 1))

  (defun my-cycle-dark-bg-backward ()
    (interactive)
    (my-cycle-dark-bg -1)))


(defun my-cycle-bg-forward ()
  "Auto-detect light or dark bg.
Then cycle bg forward with appropriate color list."
  (interactive)
  (let ((dark? (eq 'dark (frame-parameter nil 'background-mode))))
    (if dark?
        (my-cycle-dark-bg 1)
      ;; else light bg
      (my-cycle-light-bg 1))))

(defun my-cycle-bg-backward ()
  "Same as `my-cycle-bg-forward' but go backwards."
  (interactive)
  (let ((dark? (eq 'dark (frame-parameter nil 'background-mode))))
    (if dark?
        (my-cycle-dark-bg -1)
      ;; else light bg
      (my-cycle-light-bg -1))))



(let ((executed-p nil))
  ;; this fn might not be needed anymore. It seems adding the theme folder
  ;; to the load-path fixed all the issues where weird themes require a bunch
  ;; of helper files.
  (defun my-handle-weird-theme-setups ()
    "Some themes work in a special way with custom code to initialize them.
Originally this code would be run in the autoloads when the themes were melpa
packages.  But I am no longer using the themes as packages (for init
performance reasons).
Closure over executed-p."
    (interactive)
    ;; TODO: use an autoload solution instead of loading everything
    ;; immediately.
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
      (load (concat custom-theme-directory "almost-mono-themes"))
      (load (concat custom-theme-directory "humanoid-themes"))
      ;; record the fact we did the set up. To avoid doing it again.
      (setq executed-p t))))


(defvar helm-candidate-number-limit)

(defun my-load-theme-wrapper ()
  (interactive)
  ;; (my-handle-weird-theme-setups)
  ;; nil for no candidate limit. I want to scroll through all the themes.
  (let ((helm-candidate-number-limit nil))
    (call-interactively #'load-theme)))

(declare-function counsel-load-theme "counsel")
(defun my-counsel-load-theme ()
  (interactive)
  ;; (my-handle-weird-theme-setups)
  ;; (let ((ivy-height 100)) ;; taller ivy window for viewing themes.
  ;;   (call-interactively #'counsel-load-theme))
  (counsel-load-theme))


(defvar ivy-height)
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

(declare-function my-toggle-inverse-video "my-load-theme")
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

(provide 'my-load-theme)