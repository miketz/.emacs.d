(defvar bufftodo-lst '()
  "The todo list of buffers")

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

(defvar bufftodo-filter
  (define-ibuffer-filter
      todo-only
      "Filters ibuffer results to only show buffers in `bufftodo-lst'."
    (:description "todo"
                  :reader bufftodo-lst)
    (member buf bufftodo-lst))
  "Filters ibuffer to only the todo buffers.")

(defun bufftodo-view ()
  (interactive)
  (ibuffer t              ; open in new window
           "TODO buffers" ; ibuffer name
           bufftodo-lst))  ; quallifiers

(bufftodo-add-selected-buff)
(bufftodo-clear-all)
(bufftodo-view)
bufftodo-lst

