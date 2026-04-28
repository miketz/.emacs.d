;;; my-tab-line.el --- helper funcs for tab-line -*- lexical-binding: t -*-

(require 'hydra)

;; TODO: add a fn for the "+" button

(defun my-tab-line-switch-to-buffer ()
  (interactive)
  (call-interactively #'switch-to-buffer))

;;;###autoload
(defhydra my-tab-line-hydra (:color amaranth :hint nil)
  "
_n_, _j_: next
_p_, _k_: previous
_x_: [X] button. bury buffer.
_+_: [+] button. select buffer
_t_: toggle mode on/off
_T_: toggle global mode on/off
_q_, _C-g_: quit"

  ("n" tab-line-switch-to-next-tab)
  ("p" tab-line-switch-to-prev-tab)
  ("j" tab-line-switch-to-next-tab)
  ("k" tab-line-switch-to-prev-tab)
  ("x" bury-buffer)
  ("+" my-tab-line-switch-to-buffer)
  ("t" tab-line-mode)
  ("T" global-tab-line-mode)

  ("<RET>" nil)
  ("C-g" nil nil)
  ("q" nil))

;;;###autoload
(defhydra my-tab-bar-hydra (:color amaranth :hint nil)
  "
_n_, _j_: next
_p_, _k_: previous
_x_: [X] button. close tab
_+_: [+] button. new tab
_1_: close other tabs
_c_: clone tab, dupe
_r_: rename
_u_: undo
_t_: toggle mode on/off
_q_, _C-g_: quit"

  ("n" tab-bar-switch-to-next-tab)
  ("p" tab-bar-switch-to-prev-tab)
  ("j" tab-bar-switch-to-next-tab)
  ("k" tab-bar-switch-to-prev-tab)
  ("x" tab-bar-close-tab)
  ("+" tab-bar-new-tab)
  ("1" tab-bar-close-other-tabs)
  ("c" tab-bar-duplicate-tab)
  ("r" tab-bar-rename-tab)
  ("u" tab-bar-undo-close-tab)
  ("t" tab-bar-mode)

  ("<RET>" nil)
  ("C-g" nil nil)
  ("q" nil))