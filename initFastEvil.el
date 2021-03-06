(load "~/.emacs.d/initFast.el")

(add-to-list 'load-path "~/.emacs.d/notElpa/evil")
(require 'evil)
(evil-mode)

(add-to-list 'load-path "~/.emacs.d/notElpa")
(require 'key-chord)
(setq key-chord-two-keys-delay 0.2)
(setq key-chord-one-key-delay 0.4)
(key-chord-define evil-insert-state-map "fj" #'evil-normal-state)
(key-chord-define evil-visual-state-map "fj" #'evil-exit-visual-state)
(key-chord-mode 1)

