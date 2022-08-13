;;; bluey.el --- Blue/yellow theme -*- lexical-binding: t -*-

;;; Commentary:
;;; Blue background theme.

;;; Code:

(deftheme bluey "Blue bg theme")

(custom-theme-set-faces
   'charcoal

   `(default ((t (:foreground "yellow" :background "dark blue"))))

   `(cursor ((t (:background "spring green"))))

   )

(provide-theme 'bluey)

;;; bluey.el ends here