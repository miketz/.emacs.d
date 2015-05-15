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
  `(with-eval-after-load (symbol-name ,base-mode)
     ;;load mode library so we can bind keys.
     (require ,focus-mode)
     ;;using `cl-defun' to allow `return-from'
     (cl-defun ,(conat "realm-" (symbol-name name) "-focus") ()
       (interactive)
       (let ((start-tag-name ,tag-start)
             (end-tag-name   ,tag-end)
             (start          nil)
             (end            nil))
         ;; Find start tag. Search backwards first to give priority to tag pairs
         ;; the cursor is currently inside.
         (setq start (search-backward start-tag-name nil t))
         (when (null start)
           ;; if start tag not found backwards, then try forwards.
           (setq start (search-forward start-tag-name nil t)))
         (when (null start)
           (message "start tag not found")
           (return-from my/focus-javascript nil))
         ;;start is found, move cursor down a line, start highlighitng
         (next-line)
         (move-beginning-of-line nil)
         (set-mark-command nil) ;(evil-visual-line)
         ;; jump to end tag. always search forward
         (setq end (search-forward end-tag-name nil t))
         (when (null end)
           (deactivate-mark)
           (message "end tag not found")
           (return-from my/focus-javascript nil))
         ;;end tag is found. now move cursor up one line
         (previous-line)
         (move-end-of-line nil)
         ;; turn on focus-mode for this region. (and narrow)
         (call-interactively #'realm-focus focus-mode)))


     (defun my/unfocus-javascript ()
       "Undo the effects of `my/focus-javascript'."
       (interactive)
       (widen)
       (web-mode))

     ;; key bindings
     (define-key web-mode-map (kbd "C-c j") #'my/focus-javascript)
     ;; TODO: Use a different technique for this keybind. If we didn't enter
     ;; `js2-mode' from `web-mode' then we don't want `my/unfocus-javascript' to
     ;; turn on web-mode.
     (define-key js2-mode-map (kbd "C-c u") #'my/unfocus-javascript)

     ))


;; Local Variables:
;; indent-tabs-mode: nil
;; End:

(provide 'realm)

;;; realm.el ends here
