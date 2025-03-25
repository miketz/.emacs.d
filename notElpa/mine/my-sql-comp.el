;;; my-sql-comp.el --- sql completion -*- lexical-binding: t -*-

(require 'ivy)
(require 'cl-lib)

;; single global store of schema data.
(defvar my-sql-schemas '())
(defvar my-sql-tables '())
(defvar my-sql-views '())
(defvar my-sql-tables-and-views '())
(defvar my-sql-cols '())
(defvar my-sql-conn-str) ; set in `my-sql-conn-str-external-file'
(defvar my-sql-conn-str-external-file "~/my-sql-conn-str.el"
  "Store the conn string in an external location, out of this git repo.
containing code like:
  (setq my-sql-conn-str \"sqlserver://username:passw@localhost/MSSQLSERVER01?database=dbName\")")

(defun my-sql-clear-shcema-data ()
  "Clear the stored schema data."
  (interactive)
  (setq my-sql-schemas '())
  (setq my-sql-tables '())
  (setq my-sql-views '())
  (setq my-sql-tables-and-views '())
  (setq my-sql-cols '()))

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


;;;###autoload
(defun my-sql-set-conn-str ()
  "Set the conn str. Load from external source"
  (load my-sql-conn-str-external-file)
  my-sql-conn-str)

;;;###autoload
(defun my-sql-fill-completion-data ()
  "Fill the sql shema data for completion.
Overwrite any existing data."
  (interactive)
  (message "Filling sql completion data. Wait a bit...")
  (my-sql-set-conn-str) ;; set connStr
  (let* ((prog (expand-file-name "~/.emacs.d/notElpaYolo/dbQueryHelper/dbQueryHelper"))
         (cmd-schemas (concat prog " schemas \"" my-sql-conn-str "\""))
         (cmd-tables (concat prog " tables \"" my-sql-conn-str "\""))
         (cmd-views (concat prog " views \"" my-sql-conn-str "\""))
         (cmd-cols (concat prog " cols \"" my-sql-conn-str "\""))

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

    ;; clear all the stored data. 1 global set.
    (my-sql-clear-shcema-data)

    ;; schemas
    (setq my-sql-schemas (string-split csv-schemas csv-sep))

    ;; tables
    (cl-loop for csv in table-recs
             do
             (let* ((parts (string-split csv csv-sep))
                    (tab (make-my-sql-table :schema (nth 0 parts)
                                            :name (nth 1 parts))))
               (push tab my-sql-tables)))

    ;; views. made with same struct as tables above.
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


;;;###autoload
(cl-defun my-sql-complete-schema (&optional schema-prefix)
  (interactive)
  (when (null my-sql-schemas) ;; GUARD: ensure there is data to complete against.
    (message "No schema data found. Try populated via M-x my-sql-fill-completion-data.")
    (cl-return-from my-sql-complete-schema))

  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window
        (ivy-height (- (window-height) 4))) ; -4 is important so scrolling
                                        ; doens't go off screen.
    (insert (completing-read "schema: " my-sql-schemas
                             nil nil
                             (or schema-prefix "")))))

;;;###autoload
(cl-defun my-sql-complete-table (&optional schema tab-prefix)
  (interactive)
  (when (null my-sql-tables) ;; GUARD: ensure there is data to complete against.
    (message "No schema data found. Try populated via M-x my-sql-fill-completion-data.")
    (cl-return-from my-sql-complete-table))

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


;;;###autoload
(cl-defun my-sql-complete-view (&optional schema view-prefix)
  (interactive)
  (when (null my-sql-views) ;; GUARD: ensure there is data to complete against.
    (message "No schema data found. Try populated via M-x my-sql-fill-completion-data.")
    (cl-return-from my-sql-complete-view))

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

;;;###autoload
(cl-defun my-sql-complete-table-or-view (&optional schema tab-prefix)
  (interactive)
  (when (null my-sql-tables-and-views) ;; GUARD: ensure there is data to complete against.
    (message "No schema data found. Try populated via M-x my-sql-fill-completion-data.")
    (cl-return-from my-sql-complete-table-or-view))

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


;;;###autoload
(cl-defun my-sql-complete-col (&optional schema table-or-view col-prefix)
  (interactive)
  (when (null my-sql-cols) ;; GUARD: ensure there is data to complete against.
    (message "No schema data found. Try populated via M-x my-sql-fill-completion-data.")
    (cl-return-from my-sql-complete-col))

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



(defun my-sql-guess-select-bounds ()
  "Get start and end locations of current select query.
Imperfect guess, assumes certain formatting."
  (save-excursion
    (let ((start (re-search-backward "select" nil t))
          (end (re-search-forward "\n\n" nil t)))
      `(,start . ,end))))

(defun my-sql-dot-loc ()
  "Location of dot . on currnet line.
Nil if not found."
  (save-excursion
    (re-search-backward "\\." (line-beginning-position) t)))

(defun my-sql-txt-before-dot (dot-loc)
  (save-excursion
    (goto-char dot-loc)
    (thing-at-point 'symbol 'no-properties)))

;; TODO: implmeent more advanced detection of "current statement" to bound the alias search.
;;       the search can escape to a different query and get the wrong alias. also find the bounds of
;;       various sql statements: select, update, insert, delete, createTab, etc.
(cl-defun my-sql-alias-def-info (alias)
  (save-excursion
    ;; TODO: search forward too to handle clase of alias in select statement
    (let ((select-bounds (my-sql-guess-select-bounds)))
      (let* ((alias-def-backward (or (re-search-backward (concat " " alias " ") (car select-bounds) t)
                                     (re-search-backward (concat " " alias "\n") (car select-bounds) t)))
             (alias-def-forward (and (null alias-def-backward) ; not found backward
                                     (or (re-search-forward (concat " " alias " ") (cdr select-bounds) t)
                                         (re-search-forward (concat " " alias "\n") (cdr select-bounds) t)))))
        (when (and (null alias-def-forward)
                   (null alias-def-backward))
          (cl-return-from my-sql-alias-def-info nil))

        (when alias-def-forward
          ;; adjust for forward search putting cursor at the end of the match.
          (backward-word 2))

        (let ((table (thing-at-point 'symbol 'no-properties)))
          ;; (print table)
          (re-search-backward "\\." (line-beginning-position) t)
          (let ((schema (thing-at-point 'symbol 'no-properties)))
            `(:schema ,schema :table ,table)))))))

;;;###autoload
(cl-defun my-sql-complete-guess-work ()
  (interactive)
  (when (null my-sql-schemas) ;; GUARD: ensure there is data to complete against.
    (message "No schema data found. Try populated via M-x my-sql-fill-completion-data.")
    (cl-return-from my-sql-complete-guess-work))

  (let* ((txt (or (thing-at-point 'symbol 'no-properties) ""))
         (dot-loc (my-sql-dot-loc)))
    ;; no dot "." found
    (when (null dot-loc)
      ;; TODO: append to shcema text already typed.
      (my-sql-complete-schema txt)
      (cl-return-from my-sql-complete-guess-work))

    (let* ((txt-before-dot (my-sql-txt-before-dot dot-loc))
           (schema-p (member-ignore-case txt-before-dot my-sql-schemas)))
      (if schema-p
          ;; table/view completion
          (my-sql-complete-table-or-view txt-before-dot txt)
        ;; else, maybe a table alias.
        (let* ((info (my-sql-alias-def-info txt-before-dot)))
          (when (null info)
            (cl-return-from my-sql-complete-guess-work))
          ;; TODO: fill in only the remaining chars of col excluding prefix.
          (insert (my-sql-complete-col (cl-getf info :schema)
                                       (cl-getf info :table)
                                       txt)))))))


(provide 'my-sql-comp)

;;; my-sql-comp.el ends here
