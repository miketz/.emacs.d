;;;
;; code snippet from keramida on #emacs irc.
;;
;; When following cross-references or closing windows, keep Emacs windows
;; balanced.  Also when killing windows with `C-x 0'.
;;

(require 'xref)

;; When following cross-references, use a new window if there are up to three
;; windows in the current frame.  Otherwise, just use one of the existing
;; windows if possible.  If we open a new window, balance all windows too.
(defun keramida/xref-find-definitions (identifier)
  (interactive (list (xref--read-identifier "Find definitions of: ")))

  ;; Find out how many 90-column windows we can fit in the current frame
  ;; width.  It's better to re-use windows that make a set of many,
  ;; super-narrow windows which wrap around all the time.
  (let ((frame-window-limit (floor (/ (frame-width) 90))))
    (if (< (count-windows) frame-window-limit)
        (progn
          (xref-find-definitions-other-window identifier))
      (xref-find-definitions identifier)))
  (balance-windows))

(global-set-key (kbd "M-.") 'keramida/xref-find-definitions)

(defun keramida/rebalance (orig-fun &rest args)
  "Rebalance windows of the current frame, after calling one of the
functions which make new windows or destroy existing ones."
  (let ((result (apply orig-fun args)))
    (balance-windows)
    result))

;; Don't allow Emacs to split windows vertically.  This should only be a
;; viable choice if I manually ask for it.
(setq-default split-height-threshold nil)
(advice-add 'delete-window :around 'keramida/rebalance)
(advice-add 'split-window-right :around 'keramida/rebalance)
(advice-add 'split-window-horizontally :around 'keramida/rebalance)
