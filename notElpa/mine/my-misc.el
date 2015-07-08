;;;--------------------------------------------------------------------
;;; let macros
;;;--------------------------------------------------------------------
(cl-defmacro let1 (var-val-pair &body body)
  "Syntax sugar macro. Let 1 variable without double nesting the var list (())
Ex: (let1 (x 4) (print x))"
  (declare (indent 1)) ;;indent it like a normal let.
  `(let (,var-val-pair)
     ,@body))

(cl-defmacro l (var val &body body)
  "Syntax sugar macro. Let 1 variable without double nesting the var list (())
Ex: (l x 4
      (print x))"
  (declare (indent 2)) ;;indent it like a normal let.
  `(let ((,var ,val))
     ,@body))

(defmacro my-letify-alist (alist &rest body)
  ;; destructuring macro found by inspecting Bozhidar Batsov's zenburn color
  ;; theme.
  ;; example usage:
  ;; (my-letify-alist ((red . "#FF0000")
  ;;                   (green . "#00FF00")
  ;;                   (blue . "#0000FF"))
  ;;   (print red)
  ;;   (print green)
  ;;   (print blue))
  "Convert the keys of ALIST to variables.
BODY is the core code that will use the variables."
  (declare (indent defun))
  `(let (,@(mapcar (lambda (cell)
                     (list (car cell) (cdr cell)))
                   alist)) ;TODO make alist work for a variable passed in.
     ,@body))


;;;--------------------------------------------------------------------
;;; string helpers
;;;--------------------------------------------------------------------
(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (s-trim-left (s-trim-right s)))


(defun my-str-replace (what with in)
  "Replace WHAT WITH IN."
  (replace-regexp-in-string (regexp-quote what) with in))

;;;--------------------------------------------------------------------
;;; alist helpers
;;;--------------------------------------------------------------------
(defun my-alst-get-keys (lst)
  (mapcar 'car lst))

(defun my-alst-get-values (lst)
  "Return the values from LST."
  (mapcar 'cdr lst))

(defun my-alst-print-keys (lst)
  "Insert the keys of an a-list LST into the buffer."
  (mapc (lambda (key)
          (insert (symbol-name key))    ;key must be a symbol
          (insert "\n"))
        (my-alst-get-keys lst)))


;;;--------------------------------------------------------------------
;;; file stuff
;;;--------------------------------------------------------------------
(defun my-read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))


;;;--------------------------------------------------------------------
;;; helper functions to draw a list in N columns. 2-d diplay from a
;;; 1-d list.
;;;--------------------------------------------------------------------
(when nil ;these are special-use functions. Don't create them.

  (defun display-lists (lst1 lst2)
    "displays 2 lists (of strings) veritcally, side-by-side"
    (let ((long1 (apply #'max
                        (mapcar #'length
                                lst1))))
      (dotimes (i (length lst1))
        (let ((val1 (nth i lst1))
              (val2 (nth i lst2)))
          (insert val1) (insert " ")
          (dotimes (p (- long1 (length val1)))
            (insert " "))
          (insert val2)
          (insert "\n")))))

  (defun my-get-longest-str (lst)
    "returns length of the longest str"
    (apply #'max
           (mapcar #'length
                   lst)))

  (defun my-get-longest-sym (lst) ;PASS
    "returns length of the longest symbol name"
    (my-get-longest-str (mapcar #'symbol-name
                                lst)))

  (defun my-index-1d (r c num-cols) ;PASS, but not used.
    "returns a 1-D index. Using the 2d indexs r and c.
And the number of columns (and vertical layout)."
    (+ (* c num-cols)
       r))

  (defun my-index-column (i num-cols) ;PASS, but not used.
    "returns the column the 1-D index falls under for N cols
(and vertical layout)"
    (floor (/ i num-cols)))

  (defun my-index-row (i num-cols) ;PASS, but not used.
    "returns the row the 1-D index falls under for N cols (and vertical layout)"
    (let* ((c (floor (/ i num-cols)))
           (r (- i (* c num-cols))))
      r))

  (defun my-get-columns (lst num-cols) ;PASS
    "returns a list-of-lists. A list for each column from the 1-D lst.
Assums a vertically stacked display of the list.
(my-get-columns '(a b c d e f g) 3)
=>
((a b c) (d e f) (g))"
    ;;STRANGE: for some reason if I align docstring to the left without white
    ;;space it messes up paredit's ability to match parens in the code
    ;;following this fucntion.
    (let ((len            (length lst))
          (lst-of-columns nil) ;the goal
          (num-rows (+ (floor (/ len num-cols))
                       (if (> (mod len num-cols) 0) 1 0)))
          (i 0)
          (c              0))
      (while (< c num-cols)
        (let ((column nil)
              (r 0))
          (dotimes (r num-rows)
            (let ((val (nth i lst)))
              (when (not (null val));last col may have empty slots to be skipped
                (setq column (append column (list val)))))
            (incf i))
          (setq lst-of-columns (cons column lst-of-columns)))
        (incf c))
      (reverse lst-of-columns)))

  (defun my-get-longest-forEachCol (lst num-cols) ;;PASS
    "Gets the longest length for each column in LST, assuming NUM-COLS.
'(lenCol1 lenCol2... lenColN)."
    (mapcar (lambda (column)
              (my-get-longest-str column))
            (my-get-columns lst num-cols)))

  (defun my-render-list (lst num-cols min-col-spaces)
    (let* ((data (mapcar #'symbol-name lst))
           (len (length data))
           (num-rows (+ (floor (/ len num-cols))
                        (if (> (mod len num-cols) 0) 1 0)))
           (col-lengths (my-get-longest-forEachCol data num-cols))
           (columns (my-get-columns data num-cols)))
      (insert "(:i ") ; add junk item to circumvent elisp indentation rules.
      (dotimes (r num-rows)
        (dotimes (c num-cols)
          (let* ((col (nth c columns))
                 (val (nth r col))
                 (curr-col-len (nth c col-lengths))
                 (pad-size (- curr-col-len (length val)))
                 (is-last-col (= c (- num-cols 1))))
            (when (not (null val))
              (insert val)
              (unless is-last-col
                (dotimes (s min-col-spaces) (insert " "))
                (dotimes (p pad-size) (insert " ")))))
          )
        (unless (= r (- num-rows 1)) ;unless last row
          (insert "\n")))
      (insert ")")))

  );end when, render list functions