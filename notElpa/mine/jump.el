;;; jump.el --- Helper funcs to search with ripgrep. -*- lexical-binding: t -*-

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
(defun jump-read-search-dir ()
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


(defun jump (regex-fn)
  "Run ripgrep search for thing at point.
Using REGEX-FN to construct the regex with the thing-at-point text."
  (let* ((cursor-txt (thing-at-point 'symbol 'no-properties))
         (default-regex (funcall regex-fn cursor-txt))
         (regex (read-string "regex: " default-regex))
         (dir (jump-read-search-dir))
         (rg-ignore-case nil) ; ignore case to reduce false hits
         ;; (rg-command-line-flags rg-command-line-flags) ; command line flag -s to ignore case doesn't work with rg.el
         )
    ;; (add-to-list 'rg-command-line-flags "-s") ; command line flag -s to ignore case doesn't work with rg.el
    (rg regex (rg-read-files) dir)))




;;; NOTE: in regex fn's, double \\ is actually a single \. Doubled up for the
;;;       elisp string escape.

;;;----------------------------------------------------------------------------
;;; Go regexes
;;;----------------------------------------------------------------------------
(defun jump-go-methods-of-struct-regex (txt) (concat "^func \\(.+" txt "\\)"))
(defun jump-go-struct-regex (txt) (concat "^type " txt " struct"))
(defun jump-go-function-regex (txt) (concat "^func " txt "\\("))
(defun jump-go-method-regex (txt) (concat "^func \\(.+\\) " txt "\\("))
(defun jump-go-function-or-method-regex (txt) (concat "^(func " txt "\\(|func \\(.+\\) " txt "\\()"))
(defun jump-go-function-refs-regex (txt)
  ;; requires --pcre2 flag passed to ripgrep
  ;; ^(?!func ) = does *not* start with func.
  ;; .* = any match after the not-func check.
  ;; [\\t \\.] = tab, space, dot before txt(
  (concat "^(?!func ).*[\\t \\.]" txt "\\("))

;; too many false hits on short var names. use editor buffer search for local vars.
;; (defun jump-go-var-regex (txt)
;;   (let ((reg1 (concat "var " txt))
;;         (reg2 (concat "const " txt))
;;         (reg3 (concat "var.+, " txt))
;;         (reg4 (concat txt ".+:=")))
;;     (concat "(" reg1 "|" reg2 "|" reg3 "|" reg4 ")")))

(defun jump-go-global-var-regex (txt) (concat "(^var " txt "|^const " txt "|^\t" txt ".* =)"))

;;;----------------------------------------------------------------------------
;;; Go UI
;;;----------------------------------------------------------------------------
(defun jump-go-methods-of-struct ()
  "Find methods of a struct."
  (interactive)
  (jump #'jump-go-methods-of-struct-regex))

(defun jump-go-struct ()
  "Find struct definition."
  (interactive)
  (jump #'jump-go-struct-regex))

(defun jump-go-function ()
  "Find function definition."
  (interactive)
  (jump #'jump-go-function-regex))

(defun jump-go-method ()
  "Find method definition."
  (interactive)
  (jump #'jump-go-method-regex))

(defun jump-go-function-or-method ()
  "Find function or method definition.
More general, but may be slower and find more false matches."
  (interactive)
  (jump #'jump-go-function-or-method-regex))

(defun jump-go-function-refs ()
  "Find refs/calls of function.
Flawed, does not find functions stored as variables due to use of opening ( in search.
But using this regex anyway for performance and fewer false positive matches."
  (interactive)
  ;; shadow `rg-command-line-flags' for duration this let statement to avoid permanently adding --pcre2 flag
  (let ((rg-command-line-flags rg-command-line-flags))
    (add-to-list 'rg-command-line-flags "--pcre2") ; supports the "not start with func" search.
    (jump #'jump-go-function-refs-regex)))

;; too many false hits on short var names. use editor buffer search for local vars.
;; (defun jump-go-var ()
;;   "Find var definition."
;;   (interactive)
;;   (jump #'jump-go-var-regex))

(defun jump-go-global-var ()
  "Find global var definition."
  (interactive)
  (jump #'jump-go-global-var-regex))



;;;###autoload
(defhydra jump-go-hydra (:color blue :hint nil)
  "
_s_: struct
_M_: all methods of struct
_f_: function
_m_: method
_F_: function or method. (more general but more false matches)
_r_: functions references (flawed, misses fn vars)
_V_: global var
_q_, _C-g_: quit"
  ("s" jump-go-struct)
  ("M" jump-go-methods-of-struct)
  ("f" jump-go-function)
  ("m" jump-go-method)
  ("F" jump-go-function-or-method)
  ("r" jump-go-function-refs)
  ;; ("v" jump-go-var)
  ("V" jump-go-global-var)
  ("C-g" nil nil)
  ("q" nil))


;;;----------------------------------------------------------------------------
;;; C# regexes
;;;----------------------------------------------------------------------------
(defun jump-cs-class-regex (txt) (concat "(class|struct) " txt))
(defun jump-cs-interface-implementor-regex (txt) (concat "class.+:.+" txt))
(defun jump-cs-method-regex (txt)
  ;; p = for public/private/protected. filters out interface methods which i find useless for jumping to.
  ;; txt( or txt< to find definition.
  (concat " p.+ " txt "[\\(<]"))
(defun jump-cs-method-refs-regex (txt) (concat "\\." txt "\\("))

;;;----------------------------------------------------------------------------
;;; C# UI
;;;----------------------------------------------------------------------------
(defun jump-cs-class ()
  "Find class/struct definition."
  (interactive)
  (jump #'jump-cs-class-regex))

(defun jump-cs-interface-implementor ()
  "Find class implementing an interface."
  (interactive)
  (jump #'jump-cs-interface-implementor-regex))

(defun jump-cs-method ()
  "Find method definition."
  (interactive)
  (jump #'jump-cs-method-regex))

(defun jump-cs-method-refs ()
  "Find refs/calls of function."
  (interactive)
  (jump #'jump-cs-method-refs-regex))

;;;###autoload
(defhydra jump-cs-hydra (:color blue :hint nil)
  "
_c_: class
_i_: implemenators of interface
_m_: method
_r_: method references
_q_, _C-g_: quit"
  ("c" jump-cs-class)
  ("i" jump-cs-interface-implementor)
  ("m" jump-cs-method)
  ("r" jump-cs-method-refs)
  ("C-g" nil nil)
  ("q" nil))


;;;----------------------------------------------------------------------------
;;; javascript regexes
;;;----------------------------------------------------------------------------
(defun jump-js-function-regex (txt)
  (let ((reg1 (concat "function " txt))
        ;; starts with whitepsace to avoid substring fn name matches.
        ;; assuming txt and : have no whitespace between them
        (reg2 (concat "^[\\t ].*" txt ": function")))
    (concat "(" reg1 "|" reg2 ")")))

(defun jump-js-function-refs-regex (txt)
  ;; requires --pcre2 flag passed to ripgrep
  ;; ^(?!\\s*function ) = does *not* start with whitespace->function.
  ;; .* = any match after the not-func check.
  (concat "^(?!\\s*function ).*" txt "\\("))

;;;----------------------------------------------------------------------------
;;; javascript UI
;;;----------------------------------------------------------------------------
(defun jump-js-function ()
  "Find function definition"
  (interactive)
  (jump #'jump-js-function-regex))

(defun jump-js-function-refs ()
  "Find refs/calls of function.
Flawed, does not find functions stored as variables due to use of opening ( in search.
But using this regex anyway for performance and fewer false positive matches."
  (interactive)
  ;; shadow `rg-command-line-flags' for duration this let statement to avoid permanently adding --pcre2 flag
  (let ((rg-command-line-flags rg-command-line-flags))
    (add-to-list 'rg-command-line-flags "--pcre2") ; supports the "not start with func" search.
    (jump #'jump-js-function-refs-regex)))


;;;###autoload
(defhydra jump-js-hydra (:color blue :hint nil)
  "
_f_: function
_r_: functions references (flawed, misses fn vars)
_q_, _C-g_: quit"
  ("f" jump-js-function)
  ("r" jump-js-function-refs)
  ("C-g" nil nil)
  ("q" nil))


(provide 'jump)

;;; jump.el ends here