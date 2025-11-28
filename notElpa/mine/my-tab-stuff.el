;;; -*- lexical-binding: t -*-

(require 'cl-lib)

;; NOTE: at time of writing it seems `go-mode' does not have it's own indent
;; width var. Might be something to do with the standardization on tabs for
;; that langauge.
(defvar my-mode-indent-var-map
  '((c-mode c-basic-offset) (c++-mode c-basic-offset)
    (objc-mode c-basic-offset) (java-mode c-basic-offset)
    (idl-mode c-basic-offset) (pike-mode c-basic-offset)
    (awk-mode c-basic-offset) (csharp-mode c-basic-offset)
    (php-mode c-basic-offset)

    (js2-mode js-indent-level) (js-mode js-indent-level)
    (js-ts-mode js-indent-level)
    (jsonian-mode jsonian-indentation)
    (json-mode js-indent-level)
    (java-ts-mode java-ts-mode-indent-offset)

    (c-ts-mode c-ts-mode-indent-offset)
    (go-ts-mode go-ts-mode-indent-offset)
    (lua-mode lua-indent-level)
    (python-mode python-indent-offset)

    (ruby-mode ruby-indent-level) (ruby-ts-mode ruby-indent-level)
    (powershell-mode powershell-indent-level)

    ;; snippet-mode used tab-width alone? no separate indentation var?
    ;; (snippet-mode ???)

    (rust-mode rust-indent-offset)
    (zig-mode zig-indent-offset)
    (perl-mode perl-indent-level)))

;;;###autoload
(defun my-set-tab-width (width)
  "Set `tab-width' to WIDTH.
For most programming modes you need to change an additional variable as well.
Such as `c-basic-offset' for cc-mode. Or `lua-indent-level' for lua-mode.
This function will try to set the additional variable for a variety of
programming modes."
  (interactive "nindent width: ")

  ;; get "indent-width-var" for the current `major-mode'
  (let ((indent-var-sym (cadr (assoc major-mode my-mode-indent-var-map)))
        (msg ""))

    ;; set indent-var-sym
    (if (not (null indent-var-sym))
        (progn
          (set indent-var-sym width)
          (setq msg (concat msg (format "%s = %d" (symbol-name indent-var-sym) width))))
      ;; else
      (setq msg (format "Extra indent var not configured in my-mode-indent-var-map for %s.
Do not expect my-tabify-buffer to work correctly."
                        major-mode)))

    ;; set tab-width
    (setq tab-width width)
    (setq msg (concat msg (format "\ntab-width = %d" width)))

    ;; refresh indent-bars-mode if it's currently on.
    (when (and (boundp 'indent-bars-mode) indent-bars-mode)
      (indent-bars-reset))

    ;; display "indent-var-sym" and tab-width
    (message msg)))


;;;###autoload
(cl-defun my-tabify-buffer ()
  (interactive)

  ;; GUARD: don't allow this to run in a python-mode buffer
  (when (memq major-mode '(python-mode python-ts-mode))
    (message "don't use this for python as indentation can't be inferred.")
    (cl-return-from my-tabify-buffer))

  (setq indent-tabs-mode t) ;; buffer local
  (tabify (point-min) (point-max))
  ;; assumes smart-tabs-mode is configured for current mode. If not this may
  ;; inject tabs to handle alignments *after* the indentation level is reached
  ;; which would be "wrong".
  (indent-region (point-min) (point-max)))

;;;###autoload
(cl-defun my-untabify-buffer ()
  (interactive)

  ;; GUARD: don't allow this to run in a python-mode buffer
  (when (memq major-mode '(python-mode python-ts-mode))
    (message "don't use this for python as indentation can't be inferred.")
    (cl-return-from my-tabify-buffer))

  (setq indent-tabs-mode nil) ;; buffer local
  (untabify (point-min) (point-max))
  ;; this should be OK with or without smart-tabs-mode.
  (indent-region (point-min) (point-max)))


;; This fn is useful for aligning trailing comments when using tabs for
;; indentation.  It won't work if different numbers of tabs are used within the
;; aligned set of comments. But that case (different tab level) should be rare
;; as a different tab level is a different block of logic, so you wouldn't have
;; a set of comments span across it.
;;;###autoload
(defun my-comment-dwim-align-with-spaces ()
  "Temporarily use spaces while making the comments.
It will still use tabs for left-side indentation.
Useful for aligning trailing comments when using tabs for indentation, spaces
for alignment.  Doesn't solve all comment alignment issues but helps in a few
cases."
  (interactive)
  (let ((indent-tabs-mode nil)) ; spaces
    (call-interactively #'comment-dwim)))

(provide 'my-tab-stuff)