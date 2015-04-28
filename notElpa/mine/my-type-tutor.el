(defun my/type-tutor ()
  (interactive)
  (my/type-tutor-defaults
   (list "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ;top row
         "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ;Shift top row
         "[" "]" "{" "}")))

(defun my/type-tutor-defaults (char-lst)
  (let ((lines 7)
        (groups-per-line 7)
        (group-size 9))
    (my/type-tutor-insert-buffer char-lst lines groups-per-line group-size)))

(defun my/type-tutor-insert-buffer (chars lines groups-per-line group-size)
  (save-excursion
    (let ((chars-len (- (length chars) 1))
          (l 0))
      (while (< l lines)
        (let ((gpl 0))
          (while (< gpl groups-per-line)
            (let ((gs 0))
              (while (< gs group-size)
                (insert (my/getAtIndex (rand 0 chars-len)
                                       chars))
                (incf gs)))
            (insert " ")
            (incf gpl)))
        (insert "\n\n\n")
        (incf l))))
  (next-line))