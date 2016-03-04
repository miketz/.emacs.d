;; TODO: complete this.


(defvar my-backup-mode-line-format nil
  "Backs up the modeline state so it can later be restored after nilling it out.")
(defvar my-backup-fringe-mode 0
  "Backs up the modeline state so it can later be restored after nilling it out.")

(defun my-real-estate-max ()
  (interactive)
  ;; backup modeline, but only if needed so we don't mess up a good backup.
  (when (null my-backup-mode-line-format)
    (setq my-backup-mode-line-format mode-line-format))
  ;; disable mode-line for vertical real-estate
  (setq mode-line-format nil)
  ;; disabel fringe for horizontal real-estate.
  (set-fringe-mode 0))

(defun my-real-estate-restore ()
  (interactive)
  ;; restore mode line from backup, but only if there is a good backup.
  (when (not (null my-backup-mode-line-format))
    (setq mode-line-format my-backup-mode-line-format))
  (set-fringe-mode nil)  ;; nil means the default fringe width. TODO: use a backup value.
  )

(when nil ; docs on fringe.
  (set-fringe-mode 0)         ; width 0
  (set-fringe-mode nil)       ; with default
  (set-fringe-mode '(10 . 30)); custom right/left width
  )
(when nil ;; experimental. moves minibuffer into separate frame, but messes up stuff.
  ;; found on http://stackoverflow.com/questions/5079466/hide-emacs-echo-area-during-inactivity
  (setq initial-frame-alist (append '((minibuffer . nil)) initial-frame-alist))
  (setq default-frame-alist (append '((minibuffer . nil)) default-frame-alist))
  (setq minibuffer-auto-raise t)
  (setq minibuffer-exit-hook '(lambda () (lower-frame))))

;; (add-to-list 'default-frame-alist '(minibuffer  . nil))
;; (modify-frame-parameters (selected-frame) '((minibuffer . nil)))