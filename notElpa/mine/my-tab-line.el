;;; my-tab-line.el --- helper funcs for tab-line -*- lexical-binding: t -*-

(require 'hydra)
(require 'ivy)
(require 'cl-lib)

;; TODO: add a fn for the "+" button

;; (call-interactively #'switch-to-buffer)

(defun my-tab-line-switch-to-buffer ()
  "Switch to a buffer in the tab-line."
  (interactive)
  (switch-to-buffer
   (ivy-completing-read "buff: "
                        (mapcar #'buffer-name
                                (cl-remove-if ; remove current buffer.
                                 (lambda (b)
                                   (eq b (current-buffer)))
                                 (tab-line-tabs-window-buffers))))))

(defun my-tab-line--buffers-for-add ()
  "Return a list of buffers not currently in the tab-line."
  (cl-remove-if (lambda (b)
                  (or
                   ;; hidden
                   (string-prefix-p " " (buffer-name b))
                   ;; dead
                   (not (buffer-live-p b))
                   ;; minibuffer
                   (minibufferp b)
                   ;; already in tab line
                   (memq b (tab-line-tabs-window-buffers))))
                (buffer-list)))

(defun my-tab-line-add-buff-to-tab-line ()
  "Add a buffer to the tab-line.
Buffers already in the tab-line are excluded from the buffer seleciton list."
  (interactive)
  (switch-to-buffer
   (ivy-completing-read "buff: "
                        (mapcar #'buffer-name (my-tab-line--buffers-for-add)))))



;;;###autoload
(defhydra my-tab-line-hydra (:color amaranth :hint nil)
  "
_n_, _j_: next
_p_, _k_: previous
_b_: switch to buffer in tab-line
_x_: [X] button. bury buffer.
_+_: [+] button. add buffer
_t_: toggle mode on/off
_T_: toggle global mode on/off
_q_, _C-g_: quit"

  ("n" tab-line-switch-to-next-tab)
  ("p" tab-line-switch-to-prev-tab)
  ("j" tab-line-switch-to-next-tab)
  ("k" tab-line-switch-to-prev-tab)
  ("b" my-tab-line-switch-to-buffer)
  ("x" bury-buffer)
  ("+" my-tab-line-add-buff-to-tab-line :color blue) ; exits hydra
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