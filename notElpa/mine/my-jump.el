;;; my-jump.el --- Helper funcs to search with ripgrep. -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Keywords: convenience
;;; Package-Requires: ((emacs "24.1") (rg 2.3.0) (hydra "0.15.0"))
;;; Version: 0.1.0
;;; URL: TODO


;;; Code:
(require 'project)
(require 'rg)
(require 'hydra)


;; TODO: in git submodule project, verify this stops at top of submodule, not top of outer containing project.
(defun my-jump-read-search-dir ()
  "Select the dir to search.
Attempt to use project root as default selection.
If no project detected, current dir will be the default selection."
  (let* ((proj (project-current nil))
         (in-proj? (not (null proj)))
         (starting-dir (if in-proj?
                           (project-root proj)
                         ;; else, not in a project, use current dir
                         default-directory)))
    (read-directory-name "dir: " starting-dir nil t)))


(defun my-jump (regex-fn)
  "Run ripgrep search for thing at point.
Using REGEX-FN to construct the regex with the thing-at-point text."
  (let* ((cursor-txt (thing-at-point 'symbol 'no-properties))
         (default-regex (funcall regex-fn cursor-txt))
         (regex (read-string "regex: " default-regex))
         (dir (my-jump-read-search-dir)))
    (rg regex (rg-read-files) dir)))




;;; NOTE: in regex fn's, double \\ is actually a single \. Doubled up for the
;;;       elisp string escape.

;;;----------------------------------------------------------------------------
;;; Go regexes
;;;----------------------------------------------------------------------------
(defun my-go-find-methods-of-struct-regex (txt) (concat "^func \\(.+" txt "\\)"))
(defun my-go-find-struct-regex (txt) (concat "^type " txt " struct"))
(defun my-go-find-function-regex (txt) (concat "^func " txt "\\("))
(defun my-go-find-method-regex (txt) (concat "^func \\(.+\\) " txt "\\("))
(defun my-go-find-function-or-method-regex (txt) (concat "^(func " txt "\\(|func \\(.+\\) " txt "\\()"))
(defun my-go-find-function-refs-regex (txt)
  ;; requires --pcre2 flag passed to ripgrep
  ;; ^(?!func ) = does *not* start with func.
  ;; .* = any match after the not-func check.
  ;; [\\t \\.] = tab, space, dot before txt(
  (concat "^(?!func ).*[\\t \\.]" txt "\\("))

;;;----------------------------------------------------------------------------
;;; Go UI
;;;----------------------------------------------------------------------------
(defun my-go-find-struct ()
  "Find struct definition."
  (interactive)
  (my-jump #'my-go-find-struct-regex))

(defun my-go-find-function ()
  "Find function definition."
  (interactive)
  (my-jump #'my-go-find-function-regex))

(defun my-go-find-method ()
  "Find method definition."
  (interactive)
  (my-jump #'my-go-find-method-regex))

(defun my-go-find-function-or-method ()
  "Find function or method definition.
More general, but may be slower and find more false matches."
  (interactive)
  (my-jump #'my-go-find-function-or-method-regex))

(defun my-go-find-function-refs ()
  "Find refs/calls of function.
Flawed, does not find functions stored as variables due to use of opening ( in search.
But using this regex anyway for performance and fewer false positive matches."
  (interactive)
  ;; shadow `rg-command-line-flags' for duration this let statement to avoid permanently adding --pcre2 flag
  (let* ((rg-command-line-flags rg-command-line-flags))
    (add-to-list 'rg-command-line-flags "--pcre2") ; supports the "not start with func" search.
    (my-jump #'my-go-find-function-refs-regex)))

;;;###autoload
(defhydra my-go-rg-hydra (:color blue :hint nil)
  "
_s_: struct
_M_: all methods of struct
_f_: function
_m_: method
_F_: function or method. (more general but more false matches)
_r_: functions references (flawed, misses fn vars)
_q_, _C-g_: quit"
  ("s" my-go-find-struct)
  ("M" my-go-find-methods-of-struct)
  ("f" my-go-find-function)
  ("m" my-go-find-method)
  ("F" my-go-find-function-or-method)
  ("r" my-go-find-function-refs)
  ("C-g" nil nil)
  ("q" nil))


;;;----------------------------------------------------------------------------
;;; C# regexes
;;;----------------------------------------------------------------------------
(defun my-cs-find-class-regex (txt) (concat "(class|struct) " txt))
(defun my-cs-find-interface-implementor-regex (txt) (concat "class.+:.+" txt))
(defun my-cs-find-method-regex (txt) (concat " p.+ " txt "\\("))
(defun my-cs-find-method-refs-regex (txt) (concat "\\." txt "\\("))

;;;----------------------------------------------------------------------------
;;; C# UI
;;;----------------------------------------------------------------------------
(defun my-cs-find-class ()
  "Find class/struct definition."
  (interactive)
  (my-jump #'my-cs-find-class-regex))

(defun my-cs-find-interface-implementor ()
  "Find class implementing an interface."
  (interactive)
  (my-jump #'my-cs-find-interface-implementor-regex))

(defun my-cs-find-method ()
  "Find method definition."
  (interactive)
  (my-jump #'my-cs-find-method-regex))

(defun my-cs-find-method-refs ()
  "Find refs/calls of function."
  (interactive)
  (my-jump #'my-cs-find-method-refs-regex))

;;;###autoload
(defhydra my-cs-rg-hydra (:color blue :hint nil)
  "
_c_: class
_i_: implemenators of interface
_m_: method
_r_: method references
_q_, _C-g_: quit"
  ("c" my-cs-find-class)
  ("i" my-cs-find-interface-implementor)
  ("m" my-cs-find-method)
  ("r" my-cs-find-method-refs)
  ("C-g" nil nil)
  ("q" nil))


(provide 'my-jump)

;;; my-jump.el ends here