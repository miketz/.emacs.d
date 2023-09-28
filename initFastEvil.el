(load "~/.emacs.d/initFast") ;; .el or .elc if available

;;;----------------------------------------------------------------------------
;;; load-path
;;;----------------------------------------------------------------------------
;; do this at the top becuase some packages like evil leader require being
;; loaded before their parent depency
(push "~/.emacs.d/notElpa/" load-path)
(push "~/.emacs.d/notElpa/evil" load-path)
(push "~/.emacs.d/notElpa/evil-leader" load-path)

;;;----------------------------------------------------------------------------
;;; evil-leader
;;;----------------------------------------------------------------------------
;; NOTE: per docs, evil-leader must be enabled before evil mode.
(require 'evil-leader)
(global-evil-leader-mode)
;; leader keys
(evil-leader/set-leader ",")
(evil-leader/set-key "q" #'balance-windows)
(evil-leader/set-key "x" #'maximize-window)
(evil-leader/set-key "," #'delete-other-windows)
(evil-leader/set-key "d" #'delete-window)
(evil-leader/set-key "c" #'quit-window) ; buffer left alive
(evil-leader/set-key "v" #'evil-visual-block)
(evil-leader/set-key "b" #'switch-to-buffer)

;;;----------------------------------------------------------------------------
;;; evil
;;;----------------------------------------------------------------------------
(require 'evil)
(evil-mode)

;;;----------------------------------------------------------------------------
;;; key-chord
;;;----------------------------------------------------------------------------
(require 'key-chord)
(setq key-chord-two-keys-delay 0.2)
(setq key-chord-one-key-delay 0.4)
(key-chord-define evil-insert-state-map "fj" #'evil-normal-state)
(key-chord-define evil-visual-state-map "fj" #'evil-exit-visual-state)
(key-chord-mode 1)
