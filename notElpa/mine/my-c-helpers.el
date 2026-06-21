;;; my-c-helpers.el --- helper funcs for C code -*- lexical-binding: t -*-

(require 'hydra)

(defun my-c-copy-fn-declaration ()
  "Copy text for the C fn declaration into the `kill-ring'.
So you can paste it elsewhere."
  (interactive)
  (let* ((treesit-mode-p (and (fboundp #'treesit-parser-list)
                              (treesit-parser-list)))
         (fn-bod (if treesit-mode-p
                     #'treesit-beginning-of-defun
                   #'c-beginning-of-defun)))
    (save-excursion
      ;; go to beggining of fn
      (funcall fn-bod)
      ;; silently set mark, without visual highlight of region.
      (push-mark (point) t nil) ; (set-mark (point))

      ;; go to end of fn signature
      (search-forward ")")

      ;; copy fn sig, add a semi colon
      (let* ((sig (buffer-substring-no-properties (mark) (point)))
             (sig-1-line (string-replace "\n" " " sig))
             (decl (concat sig-1-line ";")))
        (kill-new decl))

      ;; (copy-region-as-kill (mark) (point))
      )))


;;;----------------------------------------------------------------------------
;;; hydra. List several C helper functions.
;;;----------------------------------------------------------------------------
(defhydra my-c-commands-hydra (:color blue :hint nil)
  "
_d_: copy fn declaration
_q_, _C-g_: quit"

  ("d" my-c-copy-fn-declaration)
  ;; don't use the hint text as it makes (:hint nil) not work?
  ("C-g" nil nil)
  ("q" nil))

;;; my-c-helpers.el ends here
