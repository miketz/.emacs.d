;;; my-font-cycle --- Cycle through fonts -*- lexical-binding: t -*-

;;; Commentary:
;;; Cycle through good fonts for the current computer.

;;; Code:

(require 'cl-lib)

(defvar my-curr-computer)
(declare-function my-cycle-font-forward 'my-font-cycle)
(declare-function my-cycle-font-backward 'my-font-cycle)
(declare-function my-print-font 'my-font-cycle)

(cl-defun my-curr-font-name ()
  "Return the current font name as a string."
  (interactive)
  (let* ((font-obj (face-attribute 'default :font)))
    (when (eq font-obj 'unspecified)
      (cl-return-from my-curr-font-name nil))
    (aref (font-info font-obj) 0)))

(defun my-get-i (val lst)
  "Get the index of VAL in LST."
  (cond ((null lst) 0)
        ((equal val (car lst)) 0)
        (t (+ (my-get-i val (cdr lst))
              1))))

(let ((my-curr-font (my-curr-font-name))
      (my-fonts
       (cond
        ((eq my-curr-computer 'wild-dog)
         '("-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-9"
           "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-7"
           "-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso8859-7"
           "-DAMA-Ubuntu Mono-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"))
        ((eq my-curr-computer 'work-laptop)
         '("-raster-Dina-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
           "-raster-Fixedsys-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
           ;; (concat "-raster-ProFontWindows-normal-normal-normal"
           ;;         "-*-22-*-*-*-c-*-iso8859-1")
           "-raster-Terminal-normal-normal-normal-mono-12-*-*-*-c-*-ms-oemlatin"
           "-raster-Terminal-normal-normal-normal-mono-18-*-*-*-c-*-ms-oemlatin"
           "-raster-r_ansi-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
           "-raster-r_ansi-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
           "-outline-Consolas-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
           "-raster-peep-normal-normal-normal-mono-16-*-*-*-c-*-ms-oemlatin"
           "-raster-peep-normal-normal-normal-mono-21-*-*-*-c-*-ms-oemlatin"))
        (t '()))))

  (defun my-cycle-font-forward (&optional font-lst)
    "Cycle through several good fonts for the current computer.
Closure over `fonts'."
    (interactive)
    (let ((my-fonts (or font-lst my-fonts)))
      (setq my-curr-font (car (or (cdr (member my-curr-font my-fonts))
                                  my-fonts))))
    (set-frame-font my-curr-font)
    (my-print-font))

  (defun my-cycle-font-backward ()
    "Cycle through several good fonts for the current computer backwards.
Closure over `fonts'."
    (interactive)
    (my-cycle-font-forward (reverse my-fonts)))

  (defun my-print-font ()
    "Print the current font.
Include the current index and length for `my-fonts'."
    (message "font %d/%d: %s"
             (1+ (my-get-i my-curr-font my-fonts))
             (length my-fonts)
             my-curr-font)))


(provide 'my-font-cycle)

;;; my-font-cycle.el ends here