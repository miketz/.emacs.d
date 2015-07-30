;;; charcoal-theme.el --- A dark theme with pastelish chalkish charcoal colors.

;;; Credits: Using zenburn-theme.el from Bozhidar Batsov as a base.

;;; Commentary:
;;; Keywords should appear "raised" or "3D" against the bg.

;;; Code:

(deftheme charcoal "The charcoal color theme")

;;background: #35352B #000000
(let ((charcoal-bg "#35352B")
      (charcoal-fg "gray80" ;"lightyellow3"
       ))
  (custom-theme-set-faces
   'charcoal
   `(default ((t (:foreground ,charcoal-fg :background ,charcoal-bg))))))



