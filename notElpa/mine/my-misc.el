;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'my-date-stuff)

(defmacro do-n-times (n &rest body)
  "Just like `dotimes', but without specifying the current index variable.
Use if you need to do something N times, but don't need to know the current
index."
  (declare (indent 1)) ;;indent it like a normal let.
  `(dotimes (,(cl-gensym) ,n)
     ,@body))

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
;; don't need these anymore. properly importing the s.el library.

;; (defun s-trim-left (s)
;;   "Remove whitespace at the beginning of S."
;;   (if (string-match "\\`[ \t\n\r]+" s)
;;       (replace-match "" t t s)
;;     s))

;; (defun s-trim-right (s)
;;   "Remove whitespace at the end of S."
;;   (if (string-match "[ \t\n\r]+\\'" s)
;;       (replace-match "" t t s)
;;     s))

;; (defun s-trim (s)
;;   "Remove whitespace at the beginning and end of S."
;;   (s-trim-left (s-trim-right s)))


;; (defun my-str-replace (what with in)
;;   "Replace WHAT WITH IN."
;;   (replace-regexp-in-string (regexp-quote what) with in))

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
          (cl-loop repeat (- long1 (length val1)) do
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
    (let* ((len            (length lst))
           (lst-of-columns nil) ;the goal
           (num-rows (+ (floor (/ len num-cols))
                        (if (> (mod len num-cols) 0) 1 0)))
           (i 0)
           (c              0))
      (while (< c num-cols)
        (let ((column nil)
              ;; (r 0)
              )
          (cl-loop repeat num-rows do
                   (let ((val (nth i lst)))
                     (when (not (null val));last col may have empty slots to be skipped
                       (setq column (append column (list val)))))
                   (cl-incf i))
          (setq lst-of-columns (cons column lst-of-columns)))
        (cl-incf c))
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
                (cl-loop repeat min-col-spaces do (insert " "))
                (cl-loop repeat pad-size do (insert " ")))))
          )
        (unless (= r (- num-rows 1)) ;unless last row
          (insert "\n")))
      (insert ")")))

  );end when, render list functions

(defun my-delete-brackets (start end)
  "Delete brackets [] in the region.
START = start of region.
END = end of region."
  (interactive "r") ; automatically wires up the current region's start/end to
                    ; the args start/end.
  (goto-char start)
  (while (search-forward "[" end t)
    (replace-match "")
    (cl-decf end))
  (goto-char start)
  (while (search-forward "]" end t)
    (replace-match "")
    (cl-decf end)))

(defun my-inject-newlines (pat1 pat2)
  "Inject newlines throughout a buffer based on PAT1 and PAT2.
Useful to break up long lines that cause performance issues.

If PAT1 and PAT2 are supplied, a newline is injected between PAT1 and PAT2.
If PAT1 is ommited a newline is injected before PAT2.
If PAT2 is ommited a newline is injected after PAT1.

For example in an html file you may break up long lines by injecting newlines
between > and <."
  (interactive
   ;; wires up pat1 and pat2 args with user input if fn called interactively.
   (list (read-string "start pattern: ")
         (read-string "end pattern: ")))
  (save-excursion
    (let ((has-pat1 (and (not (null pat1)) (not (string= "" pat1))))
          (has-pat2 (and (not (null pat2)) (not (string= "" pat2)))))
      (cond
       ;; missing pat1. inject newline before pat2
       ((and (not has-pat1) has-pat2)
        (goto-char 1)
        (let ((with-nl-injected (concat "\n" pat2)))
          (while (search-forward pat2 nil t)
            (replace-match with-nl-injected))))
       ;; missing pat2. inject newline after pat1
       ((and (not has-pat2) has-pat1)
        (goto-char 1)
        (let ((with-nl-injected (concat pat1 "\n")))
          (while (search-forward pat1 nil t)
            (replace-match with-nl-injected))))
       ;; has pat1 and pat2.  inject newline between pat1 and pat2
       ((and has-pat1 has-pat2)
        (goto-char 1)
        (let ((full-pat (concat pat1 pat2))
              (with-nl-injected (concat pat1 "\n" pat2)))
          (while (search-forward full-pat nil t)
            (replace-match with-nl-injected))))
       ;; missing both pat1 and pat2
       (t (message "Please supply start and/or end patterns."))))))

(defun my-list-holidays ()
  "List the major holidays."
  (interactive)
  (let ((year (string-to-number (format-time-string "%Y"))))
    (list-holidays year
                   year
                   (append holiday-general-holidays
                           holiday-christian-holidays))))


(defun my-indent-defun ()
  "Indent the function the cursor is inside."
  (interactive)
  (mark-defun)
  (call-interactively #'indent-region))

(defun my-win-count ()
  "Calculate the number of windows in the current frame."
  (length (window-list))
  ;; (cl-loop for w being the windows of (selected-frame)
  ;;          sum 1)
  )

(defun find-shell (&optional shell-only)
  ;; from jwd630. https://www.reddit.com/r/emacs/comments/48opk1/eshell_and_why
  ;; _cant_i_convert_to_you/
  "Find end of shell buffer or create one by splitting the current window.
If shell is already displayed in current frame, delete other windows
in frame.  Stop displaying shell in all other windows.
SHELL-ONLY will be documented later."
  (interactive)
  (let* ((shellbuf (get-buffer "*shell*")))
    (if (or (eq (window-buffer) shellbuf) shell-only)
        (delete-other-windows)
      (when (eq 1 (count-windows))
        (split-window-vertically))
      (unless (eq (window-buffer) shellbuf)
        (other-window 1)))
    ;; un-display shell in other windows (on other devices)
    (and shellbuf
         (> (length (get-buffer-window-list shellbuf nil t)) 0)
         (replace-buffer-in-windows shellbuf)))
  (shell)
  (goto-char (point-max))
  (recenter -2))

(defun what-face (pos)
  "Prints the face at point.  POS = point."
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; delete spaces between words
(defun my-cycle-spacing ()
  "Call `cycle-spacing', using fast mode.
This deletes the space if there is only 1 space.  Otherwise it would do nothing
on the first call."
  (interactive)
  (if (version< emacs-version "29.0")
      ;; old method had 3 args
      (cycle-spacing current-prefix-arg nil 'fast)
    ;; Else: new method has 1 arg and seems to do what I want with 0 args.
    (cycle-spacing)))


(progn
  ;; replacing position info in mode line with a function called on demand.
  ;; Bound to "g a".

  (defun my-what-line ()
    (interactive)
    (let ((start (point-min))
          (n (line-number-at-pos)))
      (if (= start 1)
          (format "L%d" n)
        (save-excursion
          (save-restriction
            (widen)
            (format "L%d (narrowed L%d)"
                    (+ n (line-number-at-pos start) -1) n))))))

  (defun my-what-position (&optional _detail)
    "Your position in space and time."
    (interactive "P")
    (let* ((pos (point))
           (total (buffer-size))
           (percent (if (> total 50000)
                        ;; Avoid overflow from multiplying by 100!
                        (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
                      (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1))))
           (line (my-what-line))
           (col (+ 1 (current-column))))
      (if current-prefix-arg
          ;; big output if C-u prefix used.
          (let ((curr-buff (current-buffer))
                (tmp-buff (get-buffer-create "*Space and Time*")))
            (unless (eq tmp-buff (current-buffer))
              (switch-to-buffer-other-window tmp-buff))
            (with-current-buffer tmp-buff
              (goto-char (point-max)) ;; end of buffer
              (insert "\n\n\n")
              (insert (format "%s  %d%% %s C%d\n\n"
                              (buffer-name curr-buff)
                              percent line col))
              (my-insert-date-big)))
          ;; else small output in mini buffer
          (message "%s  %d%% %s C%d     %s"
                   (buffer-name)
                   percent line col
                   (format-time-string "%-m-%-d-%Y %a %-I:%M%#p")))))
  (defalias 'my-what-time #'my-what-position))


(defun my-follow-mode ()
  "Get the windows set up for `follow-mode', then turn it on."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (shrink-window-horizontally 10)
  (follow-mode 1))


(cl-defmacro my-time-task (&body body)
  "Wrap around code to time how long it takes to execute."
  ;; require `time-date' for time-to-seconds alias. pay cost of loading lib
  ;; once during the first macro expansion. Prevents `time-to-seconds' call
  ;; from autoloading the `time-date' library during time measurement.
  (require 'time-date)
  `(let ((start (float-time)))
     ;; the work
     ,@body
     ;; print elapsed time
     (message
      (format "elapsed seconds: %f"
              (time-to-seconds
               (time-subtract (float-time)
                              start))))))

;; (defun my-getAtIndex (i lst)
;;   "Return the element at I from LST."
;;   (cond
;;    ((null lst) nil)
;;    ((= i 0) (car lst))
;;    (t (my-getAtIndex (- i 1) (cdr lst)))))

(defun my-str-starts-with-p (string prefix)
  "Return t if STRING begins with PREFIX."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(defun my-str-ends-with-p (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

(defun my-turn-on-electric-pair-local-mode ()
  "Attempt to turn on `electric-pair-local-mode'.
But check for existence first to avoid breaks on older Emacs versions.
Do not fall back to function `electric-pair-mode' because it's global."
  (if (fboundp #'electric-pair-local-mode)
      (electric-pair-local-mode 1)
    (message "electric-pair-local-mode not supported.")))






;;;----------------------------------------------------------------------------
;;; transparency stuff
;;;----------------------------------------------------------------------------
;; TODO: get `curr-alpha' on-the-fly rather than caching to avoid a
;; transparency "jump" if alpha var gets out of sync.
(let ((curr-alpha 100)) ;; Starts out 100

  (cl-defun my-set-alpha (alpha)
    "Set frame's transparency to ALPHA."
    ;; exit early if not in range 1-100.
    (when (or (> alpha 100)
              (< alpha 0))
      (message (int-to-string curr-alpha))
      (cl-return-from my-set-alpha))
    (setq curr-alpha alpha)
    (set-frame-parameter (selected-frame) 'alpha
                         `(,curr-alpha ,curr-alpha))
    (message (int-to-string curr-alpha)))

  (defun my-change-alpha (step)
    "Make frame more or less transparent by STEP."
    (let ((new-alpha (+ curr-alpha step)))
      (my-set-alpha new-alpha))))

(declare-function my-set-alpha 'my-misc)
(declare-function my-change-alpha 'my-misc)

(defun my-change-alpha-more-solid ()
  (interactive)
  (my-change-alpha 1))

(defun my-change-alpha-less-solid ()
  (interactive)
  (my-change-alpha -1))


;;;----------------------------------------------------------------------------
;;; string interpolation via a macro. Like the new C# feature.
;;;----------------------------------------------------------------------------
(defmacro template-format (str)
  "String interpolation macro.
Made by tali713 in 15 minutes on #emacs irc freenode.

Example use:
(let ((foo \"quux\")
      (bar 'baz))
  (template-format \"A string with {{foo}} and {{bar}} and maybe {{foo}} again.\"))"
  (let ((index nil)
        (locs nil)
        (strs nil))
    (while (let ((loc (string-match (rx (or "{{" "}}"))
                                    str index)))
             (when loc
               (push loc locs)
               (setq index (1+ loc)))))
    (setq locs (nreverse locs))
    (while locs
      (push (substring str (+ 2 (pop locs)) (pop locs))
            strs))
    `(format ,(replace-regexp-in-string (rx (seq "{{" (+ (not "}")) "}}"))
                                        "%s"
                                        str)
             ,@(mapcar 'intern (nreverse strs)))))



;;;----------------------------------------------------------------------------
;;; increment numbers. like Vims C-a, C-x key binds.
;;;----------------------------------------------------------------------------
(require 'thingatpt)
(defun my-inc ()
  "Increment number at point."
  (interactive)
  (let ((num (number-at-point)))
    (when num ;; GUARD: thing must be a number
      (let ((b (bounds-of-thing-at-point 'word)))
        (delete-region (car b) (cdr b)))
      (insert (number-to-string (+ 1 num))))))

;;;----------------------------------------------------------------------------
;;; my-paste-below. I think I made this for someone on irc #emacs
;;;----------------------------------------------------------------------------
(defun my-paste-below ()
  "Paste to line below.  Preserve indentation of current line."
  (interactive)
  (move-end-of-line 1)
  (newline)
  (yank)
  (indent-region (mark) (point)))





(defun my-letter-to-hex (letter)
  (format "%X" (string-to-char letter)))

(defun my-hex-to-letter (hex)
  (char-to-string (string-to-number hex 16)))


;; code sample from:
;; https://old.reddit.com/r/emacs/comments/1v0jl1/convert_string_to_ascii_code_sequence/
;;;###autoload
(defun charcode-region (start end)
  "Convert the characters between START and END in current buffer
to their character codes.  Move the unconverted text to the kill
ring."
  (interactive "r")
  (let ((characters (string-to-list (buffer-substring start end))))
    (kill-region start end)
    (insert (mapconcat 'number-to-string characters " "))))


;;; quick open of the .emacs (or init.el) file.
;;;###autoload
(defun my-open-init ()
  "Open my Emacs init file."
  (interactive)
  (find-file-existing "~/.emacs.d/init.el"))

;; blink effect on current line when switching windows or buffers.
;;;###autoload
(defun my-pulse-line-on-window-selection-change (frame)
  (when (eq frame (selected-frame))
    (pulse-momentary-highlight-one-line)))

;; I use this fn with `lisp-mode-shared-map'.
;;;###autoload
(defun my-eval-region (start end)
  "Call `eval-region' with t flag to display the result in the echo area.
START and END define the region."
  (interactive "r")
  (eval-region start end t))

;; This turns on info mode with the user-friendly GUI.
;; see https://stackoverflow.com/questions/1921049/how-to-open-info-file-in-ema
;; cs-in-info-mode
;;;###autoload
(defun my-info-mode ()
  "Turn on info mode with the user-friendly GUI."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (info file-name)))

;;;###autoload
(defun my-get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;;;###autoload
(defun my-ms-to-minute-sec (ms)
  (let* ((min (/ ms 1000 60)) ; want truncation in minute calc
         (sec-float (/ ms 1000.0))
         (sec (mod sec-float 60)))
    `(:min ,min :sec ,sec)))


(provide 'my-misc)
