(defgroup bufftodo ()
  "Add/remove buffers from a todo list."
  :group 'convenience
  :prefix "bufftodo-")


(defvar bufftodo-lst '()
  "The list of todo buffers.
NOTE:
`ibuffer' apperas to automatically modify this list if we delete a buffer.
So we don't need to worry about adding functionality to ibuffer to keep this list in sync.")

(defcustom bufftodo-open-new-window-p nil
  "If t open `ibuffer' in a new window."
  :type 'boolean
  :group 'bufftodo)


;;;###autoload
(defun bufftodo-clear-all ()
  "Clear buffers from the todo list."
  (interactive)
  (setq bufftodo-lst '()))

;;;###autoload
(defun bufftodo-add (buff)
  "Add a buffer to the todo list."
  (interactive)
  (add-to-list 'bufftodo-lst buff nil #'eq))

(defun bufftodo--read (lst)
  "Return a manually selected buffer"
  (get-buffer
   (completing-read "Buf: "
                    (mapcar (lambda (b)
                              (buffer-name b))
                            lst)
                    nil t)))

;;;###autoload
(defun bufftodo-add-selected-buff ()
  "Add a manually selected buffer to the todo list."
  (interactive)
  (bufftodo-add (bufftodo--read (buffer-list))))

;;;###autoload
(defun bufftodo-remove-selected-buff ()
  "Manually select buffer in `bufftodo-lst', then remove it."
  (interactive)
  (setq bufftodo-lst (delq (bufftodo--read bufftodo-lst) bufftodo-lst)))

;;;###autoload
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


;;;###autoload
(defun bufftodo-view ()
  "Dispaly the members of `bufftodo-lst' with `ibuffer'."
  (interactive)
  (let ((ibuffer-filtering-alist '((todo-only "todo"
                                              (lambda (buf qualifier)
                                                (member buf bufftodo-lst))))))
    (ibuffer bufftodo-open-new-window-p
             "TODO buffers"
             ibuffer-filtering-alist)))

(when nil
  ;; keybinds. the "user interface"
  (global-set-key (kbd "C-c v") #'bufftodo-view)
  (global-set-key (kbd "C-c c") #'bufftodo-add-current-buff)
  (global-set-key (kbd "C-c a") #'bufftodo-add-selected-buff)
  (global-set-key (kbd "C-c w") #'bufftodo-clear-all) ; w -> "wipe"
  (global-set-key (kbd "C-c e") #'bufftodo-remove-selected-buff))

(when nil ;; ad-hoc interactive testing
  (bufftodo-add-current-buff)
  (bufftodo-add-selected-buff)
  (bufftodo-remove-selected-buff)
  (bufftodo-clear-all)
  (bufftodo-view)
  bufftodo-lst
  ;; (ibuffer-filter-by-predicate
  ;;  (member buf bufftodo-lst))
  )

(provide 'bufftodo)

;;; bufftodo.el ends here
