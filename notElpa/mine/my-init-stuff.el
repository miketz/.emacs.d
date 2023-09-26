;;; my-init-stuff.el --- a few fn's related to init stuff -*- lexical-binding: t -*-

;;; Code:

(defconst my-minimal-emacs "24.1"
  "Minimum Emacs version needed to run this init.el.
This version introduced lexical binding.")

;;;###autoload
(defun my-assert-dependencies ()
  "Check for required dependencies.  Warn the user if any are missing."
  (interactive)
  (when (version< emacs-version
                  my-minimal-emacs)
    (display-warning
     'my-init
     (format "my init.el requires Emacs >= %s, you are using %s."
             my-minimal-emacs emacs-version)
     :error)))

;;;###autoload
(defun my-load-common ()
  "Load some commonly used features.  Where it's a gray area whether I should
load during init, or wait with autoloads."
  (interactive)
  (require 'company)
  (require 'expand-region)
  (require 'hydra)
  ;; (require 'my-hydras)
  (require 'swiper)
  (require 'ivy)
  (require 'counsel)
  (require 'lispy)
  (with-current-buffer (get-buffer-create "*scratch*")
    (unless (eq major-mode #'emacs-lisp-mode)
      (emacs-lisp-mode))))

(provide 'my-init-stuff)

;;; my-init-stuff.el ends here