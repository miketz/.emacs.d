;;; bufftodo.el --- A list of buffers. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Defines several functions to maintain a "TODO list" of buffers. It's just
;; a plain list of buffers.
;;
;; Allows the list of buffers to be viewed with `ibuffer', exluding bufers not
;; in the list. If a buffer is deleted via `ibuffer', it is automatically
;; removed from the list.
;;
;; Created as an expiriment after reading the post:
;; https://www.reddit.com/r/emacs/comments/3lvly2/multifile_editing_workflow/

;;; Code:

(defgroup bufftodo ()
  "Add/remove buffers from a todo list."
  :group 'convenience
  :prefix "bufftodo-")


(defvar bufftodo-lst '()
  "The list of todo buffers.
NOTE:
`ibuffer' appears to automatically modify this list if we delete a buffer.
So we don't need to worry about adding functionality to ibuffer to keep this
list in sync.")

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
  "Add BUFF to the todo list."
  (interactive)
  (add-to-list 'bufftodo-lst buff nil #'eq))

(defun bufftodo--read (lst)
  "Return a manually selected buffer from the LST of inputs."
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
  (memq buf bufftodo-lst))


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


(defvar bufftodo-ui-fn-lst '() ;;TODO: populate here once I can make it work.
  "An a-list of functions and their user friendly name for \"completing read\".
These functions make up the user interface of bufftodo.
This list to access the ui functoins from a single keybind for `bufftodo-ui'.")

;;;###autoload
(defun bufftodo-ui ()
  "The user central user interface of bufftodo."
  (interactive)
  ;; lst doesn't seem to load at startup, so load it here for now.
  ;; TODO: delete this dynamic check/fill
  (when (null bufftodo-ui-fn-lst)
    (setq bufftodo-ui-fn-lst '(("view" . #'bufftodo-view)
                               ("add current buff" . #'bufftodo-add-current-buff)
                               ("add buff" . #'bufftodo-add-selected-buff)
                               ("clear all" . #'bufftodo-clear-all)
                               ("remove buff" . #'bufftodo-remove-selected-buff))))
  ;; need to use nth 2 instead of cdr. assoc returns the #' as a separate item.
  (funcall (nth 2 (assoc (completing-read "pick one: "
                                          (mapcar #'car
                                                  bufftodo-ui-fn-lst))
                         bufftodo-ui-fn-lst))))

(when nil
  ;; ad-hoc interactive testing with C-x C-e
  (bufftodo-ui)
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
