;;; -*- lexical-binding: t -*-

;; (when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
;; (when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
;; (when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
;; (when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode 0))

;; supposedly default-frame-alist is faster than (tool-bar-mode 0), etc
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


;; remove title bar. to fit more lines of code on the screen
(unless (eq system-type 'windows-nt) ;; locks window on Windows!
  (add-to-list 'default-frame-alist '(undecorated . t)))

;; on mac thius makes the window a fixed size!!! Rectangle plugin can't resize emacs.
;; (progn
;;   (add-to-list 'default-frame-alist '(undecorated . t))
;;   (add-to-list 'default-frame-alist '(width . 1.0))
;;   (add-to-list 'default-frame-alist '(height . 1.0)))

(setq package-enable-at-startup nil)

;; avoid redundant native compiliation every run of Emacs on windows.
;; this snippet of code combined with the new --init-directory flag passed to emacs
;; solves the redundant native compilations.
(when (eq system-type 'windows-nt)

  ;; this used to be in ~/AppData/Roaming/.emacs.d/init.el. Doing it here to support
  ;; the new --init-directory flag passed to emacs. in theory this should not be
  ;; needed but just making things work for now.
  (when t ;; (memq my-curr-computer '(work-laptop-2025))

    (let ((dir (concat "C:/Users/" (user-login-name))))
      (setq user-init-file (concat dir "/AppData/Local/.emacs.d/init") ; no .el
            user-emacs-directory (concat dir "/AppData/Local/.emacs.d/")
            default-directory dir)
      (setenv "HOME" (concat dir "/AppData/Local/"))) ; so ~ expands correctly
    ))
