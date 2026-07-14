;;; vim-leader.el --- Leader key bind helper for evil. -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Keywords: convenience
;;; Package-Requires: ((emacs "27.1"))
;;; Version: 0.1.0
;;; URL: TODO

;;; Commentary:
;;; Set up "leader" key binds for evil.
;;;
;;; Why? I was having trouble getting the old evil-leader package to work
;;; for non-global map keybinds. I also had issues with evil's new built-in leader
;;; support. Howerver native Emacs prefix commands worked. They only draw back
;;; is the code to setup a "leader" style keybind is a bit noisy/busy/confusing.
;;; This package attempts to make it easier.
;;;
;;; This is not a general keybind package. It's laser focused on my own needs. Just
;;; getting leader keys working with evil. For both global-map and mode specific maps.

;;; Code:
(require 'evil-core) ; for `evil-define-key'

;; interface
;; (vim-leader 'normal global-map (kbd ",") (kbd "i") #'func)

;; since there is only 1 global-map we can set it up early.
(define-prefix-command 'vim-leader-global-map)

;; keep track of mode/prefix-maps
(defvar vim-leader-mode-prefix-assoc
  '((global-map . vim-leader-global-map)))

(defun vim-leader-get-configured-prefix-for-keymap (keymap)
  (cdr (assoc keymap vim-leader-mode-prefix-assoc)))

(vim-leader-get-configured-prefix-for-keymap 'global-map)
;; (cdr (assoc 'global-map vim-leader-mode-prefix-assoc))
(symbol-value global-map)

(defun vim-leader-set (state keymap key-leader key-suffix func)
  "Set a leader key bind.
STATE. the evil mode state. normal, insert, etc.
KEYMAP. Keymap. global-map for global keybinds. Or mode specific like go-mode-map.
KEY-LEADER. Leader keybind. such as (kbd \",\").
KEY-SUFFIX. Key coming key after the leader. Usually a single letter (kbd \"f\").
FUNC. The function to execute."
  (let* ((keymap-sym-name (symbol-name keymap))
         (prefix (or (vim-leader-get-configured-prefix-for-keymap keymap)
                    ;; (define-prefix-command (intern (concat "vim-leader-"
                    ;;                                        (symbol-name keymap-sym))))
                     ))
         )
    (print (concat "vim-leader-"
                   keymap-sym-name))))

(vim-leader-set 'normal 'global-map
                (kbd ",") (kbd "f")
                (lambda () (interactive) (print "hi")))

;;; vim-leader.el ends here