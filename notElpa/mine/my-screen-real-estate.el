;;; -*- lexical-binding: t -*-

;; TODO: complete this.


;; (defvar my-backup-mode-line-format nil
;;   "Backs up the modeline state so it can later be restored after nilling it out.")
;; (defvar my-backup-fringe-mode 0
;;   "Backs up the modeline state so it can later be restored after nilling it out.")


(let ((backup-format nil)) ;; Backs up the modeline state so it can later be
                           ;; restored after nilling it out.
 (defun my-real-estate-hide-mode-line ()
   (interactive)
   ;; backup modeline, but only if needed so we don't mess up a good backup.
   (when (null backup-format)
     (setq backup-format mode-line-format))
   ;; disable mode-line for vertical real-estate
   (setq mode-line-format nil))

 (defun my-real-estate-restore-mode-line ()
   (interactive)
   ;; restore mode line from backup, but only if there is a good backup.
   (when (not (null backup-format))
     (setq mode-line-format backup-format))))



(defun my-real-estate-hide-fringe ()
  (interactive)
  ;; disabel fringe for horizontal real-estate.
  (set-fringe-mode 0))

(defun my-real-estate-restore-fringe ()
  (interactive)
  ;; disable fringe for more horizontal real-estate.
  ;; nil means the default fringe width. TODO: use a backup value.
  (set-fringe-mode nil))



(defun my-real-estate-max ()
  (interactive)
  (my-real-estate-hide-mode-line)
  (my-real-estate-hide-fringe))

(defun my-real-estate-restore ()
  (interactive)
  (my-real-estate-restore-mode-line)
  (my-real-estate-restore-fringe))



(when nil ; docs on fringe.
  (set-fringe-mode 0)         ; width 0
  (set-fringe-mode nil)       ; width default
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

(provide 'my-screen-real-estate)