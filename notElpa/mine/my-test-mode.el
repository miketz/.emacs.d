;;; my-test-mode.el --- testing minor mode -*- lexical-binding: t -*-
;;; temporary scratch pad while i figure out how to do sql server completion

(require 'posframe)
(require 'ivy)
(require 'ivy-posframe)
(require 'cl-lib)

(define-minor-mode my-test-mode
  "testing mode for keybinds."
  :lighter " mor-tmp"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-o") #'my-test-complete)
            map))

(define-key my-test-mode-map (kbd "C-c o") #'my-test-complete)

(defun my-test-complete ()
  (interactive)
  (let* ((items '("this" "is" "a" "test" "of" "the" "alphabet"))
         (chosen (completing-read "items: " items)))
    (insert chosen)))



(defun my-test-prev-type ()
  (interactive)
  (print (buffer-substring-no-properties (- (point) 1) (point)))
  )

(defvar completion-types '(schemas tables cols))


(global-set-key (kbd "C-c l") #'prev-char)




(cl-defun prev-char ()
  ;; GUARD: if at beginning of line return nil
  (when (= (point) (line-beginning-position))
    (cl-return-from prev-char nil))

  (buffer-substring-no-properties (- (point) 1)
                                  (point)))

(cl-defun determine-completion-type ()
  (let ((p-char (prev-char)))
    (cond
     ((or (null p-char) ; beginning of line
          ;; whitesapce
          (string-equal p-char " ")
          (string-equal p-char "\t"))
      'schemas)
     ;; dot "."
     ((string-equal p-char ".")
      'table-or-col)
     (t 'wha?))))
;; zzz
;; thisi sa test.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-sql-schemas '())
(defvar my-sql-tables '())
(defvar my-sql-views '())
(defvar my-sql-tables-and-views '())
(defvar my-sql-cols '())

;; will reuse this struct for views too
(cl-defstruct my-sql-table
  "Struct to hold info about a table meta data."
  (schema nil)
  (name nil))


(cl-defstruct my-sql-col-data
  "Struct to hold info about a table meta data."
  (table-schema nil)
  (table-name nil)
  (col-name nil)
  (data-type nil)
  (character-max-len nil) ; int, nullable
  (ordinal-position nil) ; int
  )

(defun my-sql-fill-completion-data ()
  "Fill the sql shema data for completion.
Overwrite any existing data."
  (interactive)
  (message "Filling sql completion data. Wait a bit...")
  (let* ((prog (expand-file-name "~/.emacs.d/notElpaYolo/dbQueryHelper/dbQueryHelper"))
         (cmd-schemas (concat prog " schemas"))
         (cmd-tables (concat prog " tables"))
         (cmd-views (concat prog " views"))
         (cmd-cols (concat prog " cols"))

         ;; csv: schema|...
         (csv-schemas (shell-command-to-string cmd-schemas))
         ;; csv: schema|table, ...
         (csv-tables (shell-command-to-string cmd-tables))
         ;; csv: schema|view, ...
         (csv-views (shell-command-to-string cmd-views))
         ;; csv: schema|table|col|dataType|maxLen|ordPos, ...
         (csv-cols (shell-command-to-string cmd-cols))
         (csv-sep "|")
         (csv-sep-outer ",")

         (table-recs (string-split csv-tables csv-sep-outer))
         (view-recs (string-split csv-views csv-sep-outer))
         (col-recs (string-split csv-cols csv-sep-outer)))

    ;; schemas
    (setq my-sql-schemas (string-split csv-schemas csv-sep))

    ;; tables
    (setq my-sql-tables '()) ; reset
    (cl-loop for csv in table-recs
             do
             (let* ((parts (string-split csv csv-sep))
                    (tab (make-my-sql-table :schema (nth 0 parts)
                                            :name (nth 1 parts))))
               (push tab my-sql-tables)))

    ;; views. made with same struct as tables above.
    (setq my-sql-views '()) ; reset
    (cl-loop for csv in view-recs
             do
             (let* ((parts (string-split csv csv-sep))
                    (view (make-my-sql-table :schema (nth 0 parts)
                                             :name (nth 1 parts))))
               (push view my-sql-views)))

    ;; tables and views. useful for col completion later
    (setq my-sql-tables-and-views (append my-sql-tables
                                          my-sql-views))
    ;; cols
    (setq my-sql-cols '()) ; reset
    (cl-loop for csv in col-recs
             do
             (let* ((parts (string-split csv csv-sep))
                    (col (make-my-sql-col-data :table-schema (nth 0 parts)
                                               :table-name (nth 1 parts) ; might be view name
                                               :col-name (nth 2 parts)
                                               :data-type (nth 3 parts)
                                               :character-max-len (nth 4 parts)
                                               :ordinal-position (nth 5 parts))))

               (push col my-sql-cols))))
  (message "Sql completion data is ready!"))


(defun my-sql-complete-schema (&optional schema-prefix)
  (interactive)
  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window
        (ivy-height (- (window-height) 4))) ; -4 is important so scrolling
                                        ; doens't go off screen.
    (insert (completing-read "schema: " my-sql-schemas
                             nil nil
                             (or schema-prefix "")))))

(defun my-sql-complete-table (&optional schema tab-prefix)
  (interactive)
  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window
        (ivy-height (- (window-height) 4))) ; -4 is important so scrolling
                                        ; doens't go off screen.
    ;; prepare schema filter
    (when (null schema)
      (setq schema (completing-read "schema: " my-sql-schemas)))

    (let* ((tabs-in-schema (cl-remove-if-not (lambda (tab)
                                               (string-equal (my-sql-table-schema tab)
                                                             schema))
                                             my-sql-tables))
           (tab-names-in-schema (mapcar (lambda (tab) ; just the string name field
                                          (my-sql-table-name tab))
                                        tabs-in-schema)))
      (insert (completing-read "table: " tab-names-in-schema
                               nil nil
                               (or tab-prefix ""))))))


(defun my-sql-complete-view (&optional schema view-prefix)
  (interactive)
  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window
        (ivy-height (- (window-height) 4))) ; -4 is important so scrolling
                                        ; doens't go off screen.
    ;; prepare schema filter
    (when (null schema)
      (setq schema (completing-read "schema: " my-sql-schemas)))

    (let* ((views-in-schema (cl-remove-if-not (lambda (view)
                                                (string-equal (my-sql-table-schema view)
                                                              schema))
                                              my-sql-views))
           (view-names-in-schema (mapcar (lambda (view) ; just the string name field
                                           (my-sql-table-name view))
                                         views-in-schema)))
      (insert (completing-read "view: " view-names-in-schema
                               nil nil
                               (or view-prefix ""))))))

(defun my-sql-compelete-table-or-view (&optional schema tab-prefix)
  (interactive)
  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window
        (ivy-height (- (window-height) 4))) ; -4 is important so scrolling
                                        ; doens't go off screen.
    ;; prepare schema filter
    (when (null schema)
      (setq schema (completing-read "schema: " my-sql-schemas)))

    (let* ((tabs-in-schema (cl-remove-if-not (lambda (tab)
                                               (string-equal (my-sql-table-schema tab)
                                                             schema))
                                             my-sql-tables-and-views))
           (tab-names-in-schema (mapcar (lambda (tab) ; just the string name field
                                          (my-sql-table-name tab))
                                        tabs-in-schema)))
      (insert (completing-read "table: " tab-names-in-schema
                               nil nil
                               (or tab-prefix ""))))))


(defun my-sql-complete-col (&optional schema table-or-view col-prefix)
  (interactive)
  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window
        (ivy-height (- (window-height) 4))) ; -4 is important so scrolling
                                        ; doens't go off screen.

    ;; prepare schema filter
    (when (null schema)
      (setq schema (completing-read "schema: " my-sql-schemas)))

    ;; prepare table/view filter
    (when (null table-or-view)
      (let* ((tabs-in-schema (cl-remove-if-not (lambda (tab)
                                                 (string-equal (my-sql-table-schema tab)
                                                               schema))
                                               my-sql-tables-and-views))
             (tab-names-in-schema (mapcar (lambda (tab) ; just the string name field
                                            (my-sql-table-name tab))
                                          tabs-in-schema)))
        (setq table-or-view (completing-read "table: " tab-names-in-schema))))

    (let* ((cols-in-schema (cl-remove-if-not (lambda (col)
                                               (string-equal (my-sql-col-data-table-schema col)
                                                             schema))
                                             my-sql-cols))
           (cols-in-table (cl-remove-if-not (lambda (col)
                                              (string-equal (my-sql-col-data-table-name col)
                                                            table-or-view))
                                            cols-in-schema))
           (col-names (mapcar (lambda (col)
                                (my-sql-col-data-col-name col))
                              cols-in-table)))
      (insert (completing-read "col: " col-names
                               nil nil
                               (or col-prefix ""))))))



(defun my-sql-get-dot-loc (bounds)
  "Return location of previous dot (.) relative to the thing at point.
Searches exactly 1 character before thing at point.  No big search backwards as
that can find the wrong dot, not related to thing at point.
BOUNDS represents thing at point.
Return nil if dot is not found at previous character."
  (let* ((start (car bounds))
         (prev (1- start))
         (prev-char (buffer-substring-no-properties prev start)))
    (if (string-equal prev-char ".")
        prev
      nil)))

(defun my-sql-dot-loc ()
  "Location of dot . on currnet line.
Nil if not found."
  (save-excursion
    (re-search-backward "\\." (line-beginning-position) t)))

(defun my-sql-txt-before-dot (dot-loc)
  (save-excursion
    (goto-char dot-loc)
    (thing-at-point 'symbol 'no-properties)))

(cl-defun my-sql-complete-guess-work ()
  (interactive)
  (let* ((txt (or (thing-at-point 'symbol 'no-properties) ""))
         (dot-loc (my-sql-dot-loc)))
    ;; no dot "." found
    (when (null dot-loc)
      ;; TODO: append to shcema text already typed.
      (my-sql-complete-schema txt)
      (cl-return-from my-sql-complete-guess-work))

    (let* ((txt-before-dot (my-sql-txt-before-dot dot-loc))
           (schema-p (member-ignore-case txt-before-dot my-sql-schemas)))
      ;; (print `(,txt ,dot-loc ,txt-before-dot ,schema-p))
      (if schema-p
          (my-sql-compelete-table-or-view txt-before-dot txt)
        ;; else, maybe an alias. TODO: do more work to derive table from alias by text search
        (my-sql-complete-col)))))

(defun my-sql-complete-guess-work-BAK ()
  (interactive)
  (save-excursion
    (let* ((txt (or (thing-at-point 'symbol 'no-properties) ""))
           (bounds (bounds-of-thing-at-point 'symbol))
           (before-dot (let ((bol (line-beginning-position)))
                         (let ((dot (my-sql-get-dot-loc bounds)))
                           (when (null dot)
                             (cl-return-from my-sql-complete-guess-work nil))
                           (goto-char dot)
                           ;; Now point is on the dot ".". Find begging of schema name.
                           ;; Search backward for all of these symbols (ie don't stop after first match)
                           ;; Use the match closest to the dot as `pack-begin'.
                           (let ((pack-begin nil)
                                 (begin-chars '("(" "[" "{" " " "	"))) ;; tab
                             (cl-loop for c in begin-chars
                                      do
                                      (let ((tmp-begin (save-excursion (search-backward c bol t))))
                                        ;; only set pack-begin if it's the nearest match so far.
                                        (when (or (null pack-begin) ;; if not set yet
                                                  (and (not (null tmp-begin))
                                                       (> tmp-begin pack-begin)))
                                          (setq pack-begin tmp-begin))))

                             ;; get the sub-string that is package name.
                             (when (and pack-begin dot) ;; found both start/end
                               (cl-incf pack-begin) ;; +1 to go forward past the delimiter we matched on.
                               (buffer-substring-no-properties pack-begin dot)))))))

      ;; compelete table/view
      (when (member before-dot my-sql-schemas)
        (my-sql-complete-table before-dot txt)))))

;;; my-test-mode.el ends here
