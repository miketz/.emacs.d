(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode 0))

;; on mac thius makes the window a fixed size!!! Rectangle plugin can't resize emacs.
;; (progn
;;   (add-to-list 'default-frame-alist '(undecorated . t))
;;   (add-to-list 'default-frame-alist '(width . 1.0))
;;   (add-to-list 'default-frame-alist '(height . 1.0)))

(setq package-enable-at-startup nil)
