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

    (js2-mode js-indent-level) (js-mode js-indent-level)
    (js-ts-mode js-indent-level)

    (c-ts-mode c-ts-mode-indent-offset)
    (go-ts-mode go-ts-mode-indent-offset)
    (lua-mode lua-indent-level)
    (python-mode python-indent-offset)

    (ruby-mode ruby-indent-level) (ruby-ts-mode ruby-indent-level)

    (rust-mode rust-indent-offset)
    (zig-mode zig-indent-offset)
    (perl-mode perl-indent-level)))

(defun my-set-tab-width (width)
  "Set `tab-width' to WIDTH.
For most programming modes you need to change an additional variable as well.
Such as `c-basic-offset' for cc-mode. Or `lua-indent-level' for lua-mode.
This function will try to set the additional variable for a variety of
programming modes."
  (interactive "nindent width: ")

  ;; get "indent-width-var" for the current `major-mode'
  (let ((indent-var-sym (cadr (assoc major-mode my-mode-indent-var-map))))
    (set indent-var-sym width))
  (setq tab-width width))


(cl-defun my-tabify-buffer ()
  (interactive)

  ;; GUARD: don't allow this to run in a python-mode buffer
  (when (eq major-mode 'python-mode)
    (message "don't use this for python as indentation can't be inferred.")
    (cl-return-from my-tabify-buffer))

  (setq indent-tabs-mode t) ;; buffer local
  (tabify (point-min) (point-max))
  ;; assumes smart-tabs-mode is configured for current mode. If not this may
  ;; inject tabs to handle alignments *after* the indentation level is reached
  ;; which would be "wrong".
  (indent-region (point-min) (point-max)))

(cl-defun my-untabify-buffer ()
  (interactive)

  ;; GUARD: don't allow this to run in a python-mode buffer
  (when (eq major-mode 'python-mode)
    (message "don't use this for python as indentation can't be inferred.")
    (cl-return-from my-tabify-buffer))

  (setq indent-tabs-mode nil) ;; buffer local
  (untabify (point-min) (point-max))
  ;; this should be OK with or without smart-tabs-mode.
  (indent-region (point-min) (point-max)))

(provide 'my-tab-stuff)