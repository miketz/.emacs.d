;;; -*- lexical-binding: t; -*-
;;; realm.el --- Focus between tags then turn on a mode.

;;; Commentary:
;;
;; Provides a feature to focus on text between tags, then turn on a specific
;; mode. It generates a family of functions for the focus and unfocus. The
;; functions are bound to keys.
;;
;; Multiple indirect buffers could be used to have a window for each section of
;; a buffer. But this technique is not used becuase the font colors are shared
;; among them the buffers, defeating the much of the purpose of using a specific
;; mode for each region.
;;
;; Example use. Focus on javascript between <script> tags during `web-mode'.
;; Put in your .emacs or init.el:
;;     (realm-define
;;      :name 'focus-js-in-web-mode ; descriptive name
;;      :base-mode #'web-mode
;;      :focus-mode #'js2-mode
;;      :tag-start "<script"
;;      :tag-end "</script"
;;      :key-bind-focus (kbd "C-c j")
;;      :key-bind-unfocus (kbd "C-c u"))
;;
;; While you are editing a file in `web-mode' press [C-c j] to focus on the
;; javascript, then `js2-mode' will be turned on automatically.
;; Press [C-c u] to go unfocus and go back to `web-mode'.

;;; Credits:
;;
;; The authors of emacs functions `narrow-to-region' and `widen'. This package
;; is mostly "glue" over these functions; handling nuances of enabling and
;; disabling major and minor modes for a region.
;;
;; The idea of generating a family of funtions was taken from the `hydra'
;; package.

;;; Code:
;;* Requires
(require 'cl-lib)

;; sample use of interface
'(realm-define
  :name 'focus-js-in-web-mode ; descriptive name
  :base-mode #'web-mode
  :base-mode-map 'web-mode-map
  :focus-mode #'js2-mode
  :focus-mode-map 'js2-mode-map
  :tag-start "<script"
  :tag-end "</script"
  :key-bind-focus (kbd "C-c j")
  :key-bind-unfocus (kbd "C-c u"))

;; for compatibility with < 24.4 emacs, define `with-eval-after-load'
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
    (declare (indent 1) (debug t))
    `(eval-after-load ,file (lambda () ,@body))))

(defvar-local realm-from nil
  "Doc.")

(defun realm-focus (start end mode)
  "Narrow on the active region, then turn on `focus-mode'.

START => of region.
END => of region.
MODE => to turn on after narrowing."
  (interactive "r")
  (deactivate-mark)
  (narrow-to-region start end)
  (funcall mode))


(defmacro realm-define (&key name
                             base-mode
                             base-mode-map
                             focus-mode
                             focus-mode-map
                             tag-start
                             tag-end
                             key-bind-focus
                             key-bind-unfocus)
  "Set up keybindings to focus/unfocus between tags for a mode.

NAME => of the realm.
BASE-MODE => of the file.  May be `web-mode'.
FOCUS-MODE => of the region between tags.
TAG-START => of the focusable region.
TAG-END => of the focusable region.
KEY-BIND-FOCUS => to enter the realm and set appropriate modes.
KEY-BIND-UNFOCUS => to leave the realm and set appropriate modes."
  (let* ((name-sym (eval name))
         (name-str (symbol-name name-sym))
         (fn-focus-name (concat "realm-" name-str "-focus"))
         (fn-unfocus-name (concat "realm-" name-str "-unfocus")))
    `(with-eval-after-load (symbol-name ,base-mode)
       ;;load mode library so we can bind keys.
       (require ,focus-mode)
       ;;using `cl-defun' to allow `return-from'
       (cl-defun ,fn-focus-name ()
         (interactive)
         (let ((start-tag-name ,tag-start)
               (end-tag-name   ,tag-end)
               (start          nil)
               (end            nil))
           ;; Find start tag. Search backwards first to give priority to tag
           ;; pairs the cursor is currently inside.
           (setq start (search-backward start-tag-name nil t))
           (when (null start)
             ;; if start tag not found backwards, then try forwards.
             (setq start (search-forward start-tag-name nil t)))
           (when (null start)
             (message "start tag not found")
             (return-from my-focus-javascript nil))
           ;;start is found, move cursor down a line, start highlighitng
           (next-line)
           (move-beginning-of-line nil)
           (set-mark-command nil) ;(evil-visual-line)
           ;; jump to end tag. always search forward
           (setq end (search-forward end-tag-name nil t))
           (when (null end)
             (deactivate-mark)
             (message "end tag not found")
             (return-from my-focus-javascript nil))
           ;;end tag is found. now move cursor up one line
           (previous-line)
           (move-end-of-line nil)
           ;; turn on focus-mode for this region. (and narrow)
           (call-interactively #'realm-focus focus-mode)))

       (defun ,fn-unfocus-name ()
         "Call `widen' and restore base-mode(s)."
         (interactive)
         (widen)
         ;; base-mode is lexically scoped, unless I misunderstand
         (funcall base-mode))

       ;; key bindings
       (define-key ,base-mode-map ,key-bind-focus #',fn-focus-name)
       ;; TODO: Use a different technique for this keybind. If we didn't enter
       ;; `js2-mode' from `web-mode' then we don't want `my-unfocus-javascript'
       ;; to turn on web-mode.
       (define-key ,focus-mode-map ,key-bind-unfocus #',fn-unfocus-name)

       )))


;; Local Variables:
;; indent-tabs-mode: nil
;; End:

(provide 'realm)

;;; realm.el ends here
