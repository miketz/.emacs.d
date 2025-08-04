;;; my-csharp-helpers.el --- helper funcs for C# code -*- lexical-binding: t -*-

(require 'thingatpt)
(require 'rg)
(require 'my-jump)

(defun my-cs-find-class-regex (txt)
  (concat "(class|struct) " txt))

;;;###autoload
(defun my-cs-find-class ()
  "Find class/struct definition."
  (interactive)
  (my-jump #'my-cs-find-class-regex))



(defun my-cs-find-interface-implementor-regex (txt)
  (concat "class.+:.+" txt))

;;;###autoload
(defun my-cs-find-interface-implementor ()
  "Find class implementing an interface."
  (interactive)
  (my-jump #'my-cs-find-interface-implementor-regex))



(defun my-cs-find-method-regex (txt)
  (concat " p.+ " txt "\\("))

;;;###autoload
(defun my-cs-find-method ()
  "Find method definition."
  (interactive)
  (my-jump #'my-cs-find-method-regex))


(defun my-cs-find-method-refs-regex (txt)
  (concat "\\." txt "\\("))

;;;###autoload
(defun my-cs-find-method-refs ()
  "Find refs/calls of function."
  (interactive)
  (my-jump #'my-cs-find-method-refs-regex))

;;; my-csharp-helpers.el ends here