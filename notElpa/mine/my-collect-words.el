
;; helper fn i made for earthToAdam in #emacs
(defun my-collect-bracketed-words (start end)
  "Collect all words inside of brackets []."
  (interactive "r") ;; r wires up the current region's start/end
                    ;; to the func args start/end.
  (save-excursion
    (goto-char start)
    (let ((matches '())
          (b-open nil))
      (cl-loop while (not (null (setq b-open (search-forward "[" end t))))
               do
               (let ((b-close (search-forward "]" end t)))
                 ;; found matching brackets [ ]
                 (when (and b-open b-close)
                   (push (buffer-substring-no-properties b-open (1- b-close))
                         matches))))
      (reverse matches))))