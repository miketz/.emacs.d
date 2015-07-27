;;; charcoal-theme.el --- A dark theme with pastelish chalkish charcoal colors.

;;; Credits: Using zenburn-theme.el from Bozhidar Batsov as a base.

;;; Commentary:
;;; Keywords should appear "raised" or "3D" against the bg.

;;; Code:

(deftheme charcoal "The charcoal color theme")

;;background: #35352B #000000

;; TODO: make these setq's let bound
(setq charcoal-colors-alist
  ;; symbol            gui       256         8
  '((charcoal-bg       "#35352B" "color-231" "#35352B")
    (charcoal-fg       "#DCDCCC" "" "")
    (charcoal-fg+1     "#FFFFEF" "" "")
    (charcoal-fg-1     "#656555" "" "")
    (charcoal-bg-2     "#000000" "" "")
    (charcoal-bg-1     "#2B2B2B" "" "")
    (charcoal-bg-05    "#383838" "" "")
    (charcoal-bg+05    "#494949" "" "")
    (charcoal-bg+1     "#4F4F4F" "" "")
    (charcoal-bg+2     "#5F5F5F" "" "")
    (charcoal-bg+3     "#6F6F6F" "" "")
    (charcoal-red+1    "#DCA3A3" "" "")
    (charcoal-red      "#CC9393" "" "")
    (charcoal-red-1    "#BC8383" "" "")
    (charcoal-red-2    "#AC7373" "" "")
    (charcoal-red-3    "#9C6363" "" "")
    (charcoal-red-4    "#8C5353" "" "")
    (charcoal-orange   "#DFAF8F" "" "")
    (charcoal-yellow   "#F0DFAF" "" "")
    (charcoal-yellow-1 "#E0CF9F" "" "")
    (charcoal-yellow-2 "#D0BF8F" "" "")
    (charcoal-green-1  "#5F7F5F" "" "")
    (charcoal-green    "#7F9F7F" "" "")
    (charcoal-green+1  "#8FB28F" "" "")
    (charcoal-green+2  "#9FC59F" "" "")
    (charcoal-green+3  "#AFD8AF" "" "")
    (charcoal-green+4  "#BFEBBF" "" "")
    (charcoal-cyan     "#93E0E3" "" "")
    (charcoal-blue+1   "#94BFF3" "" "")
    (charcoal-blue     "#8CD0D3" "" "")
    (charcoal-blue-1   "#7CB8BB" "" "")
    (charcoal-blue-2   "#6CA0A3" "" "")
    (charcoal-blue-3   "#5C888B" "" "")
    (charcoal-blue-4   "#4C7073" "" "")
    (charcoal-blue-5   "#366060" "" "")
    (charcoal-magenta  "#DC8CC3" "" "")))

(setq charcoal-type 'gui)

(defmacro charcoal-with-color-variables (&rest body)
  "`let' bind all colors defined in `charcoal-color-alist' around BODY."
  (declare (indent 0))
  (let ((index (cond ((eq charcoal-type 'gui) 1)
                     ((eq charcoal-type 256) 2)
                     ((eq charcoal-type 8) 3))))
    `(let (,@(mapcar (lambda (ele)
                       (list (car ele) (nth index ele)))
                     charcoal-colors-alist))
       ,@body)))

(charcoal-with-color-variables
  (if (stringp charcoal-bg)
      (insert charcoal-bg))
  (custom-theme-set-faces
   'charcoal
   `(default ((t (:foreground ,charcoal-fg :background ,charcoal-bg)))))
  )

