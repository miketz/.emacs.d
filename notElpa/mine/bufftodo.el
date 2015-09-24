(defvar bufftodo-lst '()
  "The list of todo buffers.")

(defun bufftodo-clear-all ()
  "Clear buffers from the todo list."
  (interactive)
  (setq bufftodo-lst '()))

(defun bufftodo-add (buff)
  "Add a buffer to the todo list."
  (interactive)
  (add-to-list 'bufftodo-lst buff nil #'eq))

(defun bufftodo-add-selected-buff ()
  "Add a manually selected buffer to the todo list."
  (interactive)
  (let ((buff (get-buffer
               (completing-read "Buf: "
                                (mapcar (lambda (b)
                                          (buffer-name b))
                                        (buffer-list))
                                nil
                                t))))
    (bufftodo-add buff)))

(defun bufftodo-add-current-buff ()
  "Add the current buffer to the todo list."
  (interactive)
  (bufftodo-add (current-buffer)))

(require 'ibuf-ext)
;; create a filter for `ibuffer' for members of `bufftodo-lst'
(define-ibuffer-filter ;; creates a new fn `ibuffer-filter-by-todo-only'
    todo-only
    "Filters ibuffer results to memebers of `bufftodo-lst'."
  (:description "todo" :reader bufftodo-lst)
  ;; buf variable is introduced in the macro.
  (member buf bufftodo-lst))


(defun bufftodo-view ()
  "Dispaly the members of `bufftodo-lst' with `ibuffer'."
  (interactive)
  (let ((ibuffer-filtering-alist '((todo-only "todo"
                                              (lambda (buf qualifier)
                                                (member buf bufftodo-lst))))))
    (ibuffer t "TODO buffers" ibuffer-filtering-alist)))

(when nil ;; ad-hoc interactive testing
  (bufftodo-add-current-buff)
  (bufftodo-add-selected-buff)
  (bufftodo-clear-all)
  (bufftodo-view)
  bufftodo-lst
  (ibuffer-filter-by-predicate
   (member buf bufftodo-lst)))
