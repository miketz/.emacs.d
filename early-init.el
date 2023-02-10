(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode 0))

;; on mac thius makes the window a fixed size and too small!
;; (add-to-list 'default-frame-alist '(undecorated . t))

(setq package-enable-at-startup nil)
