;;; my-font-stuff --- Font stuff -*- lexical-binding: t -*-

;;; Commentary:
;;; Font helper functions.

;;; Code:
(require 'ivy)

(defvar completing-read-function)
(defvar ivy-re-builders-alist)
(defvar ivy-height)

(defun my-set-frame-font-ivy ()
  "Select the font with ivy's out of order matching."
  (interactive)
  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window
        (ivy-height (- (window-height) 4))) ; -4 is important so scrolling
                                            ; doens't go off screen.

    ;; `set-frame-font' does not list raster/bitmap fonts. so don't use it for selection.
    ;; (call-interactively #'set-frame-font)

    ;; `font-family-list' does include raster/bitmap fonts.
    ;; TODO: inclue bold, itallic, etc in the list.
    (let ((font (ivy-completing-read "font: " (delete-dups (font-family-list)))))
      (set-frame-font font))))




(let ((curr-font-size nil)) ;; Starts out unknown
  (defun my-change-font-size (step)
    "Make font bigger or smaller by STEP.
Closure over `curr-font-size'."
    (let* ((curr-size (if curr-font-size ;; use cached value if it's set
                          curr-font-size
                        (face-attribute 'default :height (selected-frame))))
           (new-size (+ curr-size step)))

      (custom-set-faces `(default ((t (:height ,new-size)))))

      ;; must cache the new value because :height does not actually inc until a
      ;; threshold is breached.
      (setq curr-font-size new-size)

      ;; commenting the window size toggle off. It's seems to have become
      ;; slow on `work-laptop'.
      ;; (when (fboundp 'my-w32-run)
      ;; TODO: make it work on non-Windows machines.
      ;;   (my-w32-run 'restore-curr-frame)
      ;;   (my-w32-run 'max))

      (message (int-to-string new-size)))))

(declare-function my-change-font-size 'my-font-stuff)

(defun my-change-font-size-bigger ()
  "Make font size bigger."
  (interactive)
  (my-change-font-size 1)) ;; TODO: calculate "threshold" step increment.

(defun my-change-font-size-smaller ()
  "Make font size smaller."
  (interactive)
  (my-change-font-size -1)) ;; TODO: calculate "threshold" step decrement.


;; (defun my-set-font-size ()
;;   "Interactive layer over my-set-font. Takes the font size as user input."
;;   (interactive)
;;   (let ((size (string-to-number (read-string "font-size: "
;;                                              nil
;;                                              'my-history))))
;;     (my-set-font :height size
;;                  :resize-window t)))
;; (defun my-set-font-weight ()
;;   "Interactive layer over my-set-font."
;;   (interactive)
;;   (let ((weight (intern (read-string "font-weight: "
;;                                              nil
;;                                              'my-history))))
;;     (my-set-font :weight weight
;;                  :resize-window t)))


(provide 'my-font-stuff)

;;; my-font-stuff.el ends here
