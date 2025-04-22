;;; my-font-cycle --- Cycle through fonts -*- lexical-binding: t -*-

;;; Commentary:
;;; Cycle through good fonts for the current computer.

;;; Code:
(require 'ivy)
(require 'cl-lib)

(defvar my-curr-computer)
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
       ((eq my-curr-computer 'mac-mini-m1-2021)
        ["-*-Iosevka-thin-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         "-*-Iosevka-light-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         "-*-Iosevka-regular-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         "-*-Iosevka-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         "-*-Iosevka-semibold-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         "-*-Iosevka-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         "-*-Iosevka-ultraheavy-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         ;; "-*-Iosevka-regular-normal-normal-*-17-*-*-*-m-0-iso10646-1"
         "-*-Ubuntu Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         "-*-Ubuntu Mono-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         "-*-JetBrains Mono NL-thin-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         "-*-JetBrains Mono NL-light-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         "-*-JetBrains Mono NL-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         "-*-JetBrains Mono NL-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         "-*-JetBrains Mono NL-ultrabold-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         "-*-Unifont-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1"
         "-*-Unifont-bold-normal-normal-*-*-*-*-*-p-0-iso10646-1"
         "-*-PT Mono-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         "-*-PT Mono-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1"
         ;; "-*-PT Mono-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1"
         ;; "-*-PT Mono-bold-normal-normal-*-18-*-*-*-m-0-iso10646-1"
         ])
       ((eq my-curr-computer 'wild-dog)
        '["-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-9"
          "-misc-fixed-bold-r-normal--18-*-100-100-c-90-iso10646-1"
          "-misc-fixed-medium-r-normal--18-*-100-100-c-90-iso10646-1"
          "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-7"
          "-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso8859-7"
          ;; "-DAMA-Ubuntu Mono-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"
          "-DAMA-Ubuntu Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
          "-DAMA-Ubuntu Mono-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1"
          "-Bits-Bitstream Vera Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
          "-Bits-Bitstream Vera Sans Mono-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1"])
       ((memq my-curr-computer '(work-laptop-2019 work-laptop))
        '["-raster-Dina-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1"

          "-raster-Terminus-normal-normal-normal-mono-6-*-*-*-c-*-iso8859-1"

          "-raster-Terminus-normal-normal-normal-mono-14-*-*-*-c-*-iso8859-1"
          "-raster-Terminus-bold-normal-normal-mono-14-*-*-*-c-*-iso8859-1"
          "-raster-Terminus-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
          "-raster-Terminus-bold-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
          "-raster-Terminus-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
          "-raster-Terminus-bold-normal-normal-mono-17-*-*-*-c-*-iso8859-1"

          "-raster-Terminus-normal-normal-normal-mono-19-*-*-*-c-*-iso8859-1"
          "-raster-Terminus-bold-normal-normal-mono-19-*-*-*-c-*-iso8859-1"
          "-raster-Terminus-normal-normal-normal-mono-21-*-*-*-c-*-iso8859-1"
          "-raster-Terminus-bold-normal-normal-mono-21-*-*-*-c-*-iso8859-1"
          "-raster-Terminus-normal-normal-normal-mono-23-*-*-*-c-*-iso8859-1"
          "-raster-Terminus-bold-normal-normal-mono-23-*-*-*-c-*-iso8859-1"
          "-raster-Terminus-normal-normal-normal-mono-26-*-*-*-c-*-iso8859-1"
          "-raster-Terminus-bold-normal-normal-mono-26-*-*-*-c-*-iso8859-1"
          "-raster-Terminus-normal-normal-normal-mono-30-*-*-*-c-*-iso8859-1"
          "-raster-Terminus-bold-normal-normal-mono-30-*-*-*-c-*-iso8859-1"

          "-raster-Fixedsys-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
          "-raster-Terminal-normal-normal-normal-mono-12-*-*-*-c-*-ms-oemlatin"
          "-raster-Terminal-normal-normal-normal-mono-18-*-*-*-c-*-ms-oemlatin"
          "-outline-Iosevka Thin-thin-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
          "-outline-Iosevka Extralight-ultralight-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
          "-outline-Iosevka Light-light-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
          "-outline-Iosevka-regular-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
          "-outline-Iosevka Medium-medium-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
          "-outline-Iosevka Semibold-semibold-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
          "-outline-Iosevka-bold-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
          "-outline-Iosevka Extrabold-extrabold-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
          "-outline-Iosevka Term Heavy-black-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
          "-raster-r_ansi-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
          "-raster-r_ansi-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
          "-outline-Consolas-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
          "-outline-Consolas-bold-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
          ;; "-raster-peep-normal-normal-normal-mono-16-*-*-*-c-*-ms-oemlatin"
          "-raster-ProggySquare-normal-normal-normal-mono-11-*-*-*-c-*-iso8859-1"
          "-outline-Ubuntu Mono-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
          "-outline-Ubuntu Mono-bold-normal-normal-mono-16-*-*-*-c-*-iso10646-1"
          "-outline-Lucida Console-normal-normal-normal-mono-*-*-*-*-c-*-tis620-2533"
          "-outline-Envy Code R-bold-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
          "-outline-JetBrains Mono NL Thin-thin-normal-normal-mono-13-*-*-*-c-*-iso10646-1"
          "-outline-JetBrains Mono NL ExtraLight-extralight-normal-normal-mono-13-*-*-*-c-*-iso10646-1"
          "-outline-JetBrains Mono NL Light-light-normal-normal-mono-13-*-*-*-c-*-iso10646-1"
          "-outline-JetBrains Mono NL-normal-normal-normal-mono-13-*-*-*-c-*-iso10646-1"
          "-outline-JetBrains Mono NL Medium-normal-normal-normal-mono-13-*-*-*-c-*-iso10646-1"
          "-outline-JetBrains Mono NL-bold-normal-normal-mono-13-*-*-*-c-*-iso10646-1"
          "-outline-JetBrains Mono NL ExtraBold-extrabold-normal-normal-mono-13-*-*-*-c-*-iso10646-1"])
       (t nil)))
     (i 0)
     (len (length my-fonts))
     ;; convert the array to a list so i can use `completing-read'.
     (my-fonts-list (cl-loop for f across my-fonts
                             with lst = '()
                             do
                             (push f lst) ; (add-to-list 'lst f t)
                             finally
                             (return lst))))

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
    (my-font-set-index (+ i step)))

  (defun my-select-font ()
    "Use `completing-read' to select the font instead of cycling."
    (interactive)
    (let (;; dynamically shadow ivy completion style to ignore order.
          (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
          ;; taller ivy window
          (ivy-height (- (window-height) 4))) ; -4 is important so scrolling
                                              ; doens't go off screen.
      (set-frame-font (ivy-completing-read "font: " my-fonts-list)))))

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