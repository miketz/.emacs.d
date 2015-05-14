;;; realm.el --- Focus between tags then turn on a mode. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Provides a feature to focus on text between tags, then turn on a specific mode.
;; It will generate functions for the focus and unfocus. These funcs can
;; be bound to keys.
;;
;; Example use. Focus on <script> tags during `web-mode'.
;; Put in your .emacs or init.el:
;;     (realm-define
;;      :name 'focus-js-in-web-mode
;;      :base-mode #'web-mode
;;      :focus-mode #'js2-mode
;;      :tag-start "<script"
;;      :tag-end "</script"
;;      :key-bind-focus (kbd "C-c j")
;;      :key-bind-unfocus (kbd "C-c u"))

;;; Code:
;;* Requires
(require 'cl-lib)

;; sample use of interface
'(realm-define
  :name 'focus-js-in-web-mode
  :base-mode #'web-mode
  :focus-mode #'js2-mode
  :tag-start "<script"
  :tag-end "</script"
  :key-bind-focus (kbd "C-c j")
  :key-bind-unfocus (kbd "C-c u"))


(defun realm-focus (start end mode)
  "Narrow on the active region, then turn on `focus-mode'.

START => of region.
END => of region.
MODE => to turn on after narrowing."
  (interactive "r")
  (deactivate-mark)
  (narrow-to-region start end)
  (funcall mode))

(defun realm-unfocus (base-mode)
  "Undo the effects of `realm-focus'."
  (interactive)
  (widen)
  (funcall base-mode))

(defmacro realm-define (&key name
                             base-mode
                             focus-mode
                             tag-start
                             tag-end
                             key-bind-focus
                             key-bind-unfocus)
  "Doc."
  `(with-eval-after-load (symbol-name ,:base-mode)
     (require ,focus-mode) ;load it so we can bind keys.

     (defun realm-)
     ))


;; Local Variables:
;; indent-tabs-mode: nil
;; End:

(provide 'realm)

;;; realm.el ends here
