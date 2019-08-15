;;;----------------------------------------------------------------------------
;;; make a pdump.
;;;
;;; to make a dump:
;;;     emacs --batch -l ~/.emacs.d/notElpa/mine/my-make-pdump.el
;;;
;;; run with the dump
;;;     emacs --dump-file=~/.emacs.d/dump.pdmp
;;;----------------------------------------------------------------------------

;; load init file
(load "~/.emacs.d/init.el")

;; load my helper functions for dumping.
(require 'my-pdump)

;; load many libraries.
(my-load-everything-for-pdump)

;; dump
(my-make-pdump)