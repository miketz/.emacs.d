;;; csv-stuff.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; From cog2011 on reddit.
;; https://old.reddit.com/r/emacs/comments/26c71k/csv_column_highlighting/
;; Turning it into a package/feature.
;;
;; Highlights csv columns with colors.

;;; Code:
(require 'cl-lib)
(require 'color)

;;;###autoload
(defun csv-highlight (&optional separator)
  "Add colored text for each csv column.
Specify the SEPARATOR."
  (interactive (list (when current-prefix-arg (read-char "Separator: "))))
  (font-lock-mode 1)
  (let* ((separator (or separator ?\,))
         (n (count-matches (string separator) (point-at-bol) (point-at-eol)))
         (colors (cl-loop for i from 0 to 1.0 by (/ 2.0 n)
                          collect (apply #'color-rgb-to-hex
                                         (color-hsl-to-rgb i 0.3 0.5)))))
    (cl-loop for i from 2 to n by 2
             for c in colors
             for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
             do
             (font-lock-add-keywords nil
                                     `((,r (1 '(face (:foreground ,c)))))))))


(provide 'csv-stuff)

;;; csv-stuff.el ends here
