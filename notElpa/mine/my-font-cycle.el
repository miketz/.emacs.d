;;; my-font-cycle --- Cycle through fonts -*- lexical-binding: t -*-

;;; Commentary:
;;; Cycle through good fonts for the current computer.

;;; Code:

(require 'cl-lib)

(defvar my-curr-computer)
;; (declare-function my-cycle-font-forward 'my-font-cycle)
;; (declare-function my-cycle-font-backward 'my-font-cycle)
;; (declare-function my-print-font 'my-font-cycle)
(declare-function my-font-set-index 'my-font-cycle)
(declare-function my-cycle-font 'my-font-cycle)

;; no longer used.
(cl-defun my-curr-font-name ()
  "Return the current font name as a string."
  (interactive)
  (let* ((font-obj (face-attribute 'default :font)))
    (when (eq font-obj 'unspecified)
      (cl-return-from my-curr-font-name nil))
    (aref (font-info font-obj) 0)))

;; no longer used.
(defun my-get-i (val lst)
  "Get the index of VAL in LST."
  (cond ((null lst) 0)
        ((equal val (car lst)) 0)
        (t (+ (my-get-i val (cdr lst))
              1))))

(let*
    ((my-fonts
      (cond
       ((eq my-curr-computer 'wild-dog)
        '["-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-9"
          "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-7"
          "-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso8859-7"
          "-DAMA-Ubuntu Mono-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"])
       ((eq my-curr-computer 'work-laptop)
        '["-raster-Dina-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
          "-raster-Fixedsys-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
          ;; (concat "-raster-ProFontWindows-normal-normal-normal"
          ;;         "-*-22-*-*-*-c-*-iso8859-1")
          "-raster-Terminal-normal-normal-normal-mono-12-*-*-*-c-*-ms-oemlatin"
          "-raster-Terminal-normal-normal-normal-mono-18-*-*-*-c-*-ms-oemlatin"
          "-raster-r_ansi-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
          "-raster-r_ansi-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
          "-outline-Consolas-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
          "-raster-peep-normal-normal-normal-mono-16-*-*-*-c-*-ms-oemlatin"
          "-raster-peep-normal-normal-normal-mono-21-*-*-*-c-*-ms-oemlatin"])
       (t nil)))
     (i 0)
     (len (length my-fonts)))

  (defun my-font-set-index (new-i)
    "Set the current font index and display that font.
Closure over `my-fonts'."
    (interactive "n") ; read new index from minibuffer if not provided.
    (setq i (if (or (>= new-i len)
                    (< new-i 0))
                (mod new-i len) ; if i out of range then wrap via mod.
              new-i))           ; else good index
    (set-frame-font (aref my-fonts i))
    (message "font %d/%d: %s"
             (1+ i)
             len
             (aref my-fonts i)))

  (defun my-cycle-font (step)
    "Cycle through several good fonts for the current computer by N STEPs.
Closure over `my-fonts'."
    (interactive "n") ; read step from minibuffer if not provided.
    (my-font-set-index (+ i step))))

(defun my-cycle-font-forward ()
  "Cycle through several good fonts for the current computer."
  (interactive)
  (my-cycle-font 1))

(defun my-cycle-font-backward ()
  "Cycle through several good fonts for the current computer backwards."
  (interactive)
  (my-cycle-font -1))


(provide 'my-font-cycle)

;;; my-font-cycle.el ends here