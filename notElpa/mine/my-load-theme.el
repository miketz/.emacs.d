;;; -*- lexical-binding: t -*-

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



(declare-function my-cycle-light-bg-set-index 'my-load-theme)
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
                 (,my-ultimate . "my-ultimate")
                 ("ivory" . "ivory")
                 ("ivory2" . "ivory2")
                 ("ivory3" . "ivory3")])
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
      (set-background-color bg)
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






(defvar helm-candidate-number-limit)

(defun my-load-theme-wrapper ()
  (interactive)
  (my-handle-weird-theme-setups)
  ;; nil for no candidate limit. I want to scroll through all the themes.
  (let ((helm-candidate-number-limit nil))
    (call-interactively #'load-theme)))

(defun my-counsel-load-theme ()
  (interactive)
  (my-handle-weird-theme-setups)
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