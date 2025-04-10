;;; -*- lexical-binding: t -*-
;;; just testing out the new sqlite functions

(sqlite-available-p)

;;; open db. create if missing
(defvar *db* (sqlite-open "cars.db"))

;;; testing
(sqlitep *db*)
(sqlite-close *db*)

;;; table
(sqlite-execute *db* "create table Emp (
    Name string,
    Age int
);")

;;; data
(sqlite-execute *db* "insert into Emp values ('joe',20)")
(sqlite-execute *db* "insert into Emp values ('bob',25)")
(sqlite-execute *db* "insert into Emp values (?,?)" ["mike" 4900543])
(sqlite-execute *db* "insert into Emp values (?,?)" ["mike2" 30])

;;; query 1
(sqlite-select *db* "select * from Emp")
'(("joe" 20)
  ("bob" 25)
  ("mike" 4900543))

;;; query 2
(sqlite-select *db* "select * from Emp where Age > ?" [29])
'(("mike" 4900543) ("mike2" 30))

;;; for table output with col names
(sqlite-select *db* "select * from Emp where Age > ?" [29] 'full)
'(("Name" "Age")
  ("mike" 4900543)
  ("mike2" 30))

;;; for use with `sqlite-next'
(sqlite-select *db* "select * from Emp where Age > ?" [29] 'set)


;;; data reader style
(let* ((query "select * from Emp where Age > ?")
       (set (sqlite-select *db* query [29] 'set)))
  ;; col names
  (print (sqlite-columns set))
  ;; row data
  (let ((row nil))
    (while (setq row (sqlite-next set))
      (print row)
      ;; (print `(:more-p ,(sqlite-more-p set)))
      ))
  ;; clean up now. don't wait for GC
  (sqlite-finalize set))

(sqlite-transaction)
(sqlite-commit)
(sqlite-rollback)
(sqlite-version)
;; "3.46.1"


;;; my own macro. like a "using" block in C#. free immediatley instead of waiting for GC.
(defmacro with-sqlite-db (db db-open &rest body)
  "Closes DB when statement block ends. Not waiting for GC."
  (declare (indent defun)) ; treat indenation in body like it's in a defun
  `(let ((,db ,db-open))
     (ignore-errors
       ,@body)
     (sqlite-close ,db)))

;;; my own macro. like a "using" block in C#. free set immediatley instead of waiting for GC.
(defmacro with-sqlite-set (set set-init &rest body)
  "Closes SET when block ends. Not waiting for GC."
  (declare (indent defun)) ; treat indenation in body like it's in a defun
  `(let ((,set ,set-init))
     (ignore-errors
       ,@body)
     (sqlite-finalize ,set)))

(macroexpand-1 '(with-sqlite-set my-set (get-set)
                                 (let ((row nil))
                                   (while (setq row (sqlite-next my-set))
                                     (print row)))))



;;; test set macro
(with-sqlite-set set (sqlite-select *db*
                                    "select * from Emp where Age > ?"
                                    [29] 'set)
  ;; row data
  (let ((row nil))
    (while (setq row (sqlite-next set))
      (print row))))



;;; test db macro with sub macro for set
(with-sqlite-db db (sqlite-open "cars.db")
  (with-sqlite-set set (sqlite-select db
                                      "select * from Emp where Age > ?"
                                      [29] 'set)
    ;; print rows
    (let ((row nil))
      (while (setq row (sqlite-next set))
        (print row)))))

;;; test db macro
(with-sqlite-db db (sqlite-open "cars.db")
  (print
   (sqlite-select db "select * from Emp where Age > ?" [29])))


;;; GUI to browse DB
(sqlite-mode-open-file "cars.db")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; performance test.  closing DB each time, vs resuing global.
;; for a single user emacs app there may be no need to close after each operation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 1. keep dp open
(setq *db* (sqlite-open "cars.db"))
(my-time-task
 (cl-loop repeat 500 do
          (sqlite-select *db* "select * from Emp where Age > ?" [29])))
"elapsed seconds: 0.029898"
"elapsed seconds: 0.032978"
"elapsed seconds: 0.030495"

;; 2. auto-close after each operatoin. about 5.3x slower.
(my-time-task
 (cl-loop repeat 500 do
          (with-sqlite-db db (sqlite-open "cars.db")
            (sqlite-select db "select * from Emp where Age > ?" [29]))))
"elapsed seconds: 0.159390"
"elapsed seconds: 0.146265"
"elapsed seconds: 0.206349"

;; 3. keep dp open, but use the "with-" macro to close after loop ends
;; should be roughly equivalent to test 1.
;; maybe it's still a good idea to auto close, just wrap at the outer level.
(my-time-task
 (with-sqlite-db db (sqlite-open "cars.db")
   (cl-loop repeat 500 do
            (sqlite-select db "select * from Emp where Age > ?" [29]))))
"elapsed seconds: 0.031638"
"elapsed seconds: 0.031256"
"elapsed seconds: 0.032786"

