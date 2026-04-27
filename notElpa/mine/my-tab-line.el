;;; my-tab-line.el --- helper funcs for tab-line -*- lexical-binding: t -*-

(require 'hydra)

;; TODO: add a fn for the "+" button

;;;###autoload
(defhydra my-tab-line-hydra (:color amaranth :hint nil)
  "
_n_, _j_: next
_p_, _k_: previous
_x_: [X] button. bury buffer.
_q_, _C-g_: quit"

  ("n" tab-line-switch-to-next-tab)
  ("p" tab-line-switch-to-prev-tab)
  ("j" tab-line-switch-to-next-tab)
  ("k" tab-line-switch-to-prev-tab)
  ("x" bury-buffer)

  ("<RET>" nil)
  ("C-g" nil nil)
  ("q" nil))