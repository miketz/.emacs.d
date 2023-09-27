(load "~/.emacs.d/initFast") ;; .el or .elc if available

(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(cua-mode t)
