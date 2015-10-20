;;; bufftodo.el --- A list of buffers. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Defines several functions to maintain a "TODO list" of buffers. It's just
;; a plain list of buffers.
;;
;; The list of buffers is viewed with `ibuffer', exluding buffers not in the
;; list.
;;
;; Created as an expiriment after reading the post:
;; https://www.reddit.com/r/emacs/comments/3lvly2/multifile_editing_workflow/

;;; Code:

(defgroup bufftodo ()
  "Add/remove buffers from a todo list."
  :group 'convenience
  :prefix "bufftodo-")

(defvar bufftodo-lst '()
  "The list of todo buffers.")

(defcustom bufftodo-open-new-window-p nil
  "If t open `ibuffer' in a new window."
  :type 'boolean
  :group 'bufftodo)

(defun bufftodo--add (buff)
  "Add BUFF to `bufftodo-lst'."
  (interactive)
  (add-to-list 'bufftodo-lst buff nil #'eq))

(defun bufftodo--read (lst)
  "Return a manually selected buffer from the LST."
  (get-buffer
   (completing-read "Buf: "
                    (mapcar (lambda (b)
                              (buffer-name b))
                            lst)
                    nil t)))

(require 'cl-lib)
(defun bufftodo--clean-deleted-buffs ()
  "Remove buffers which no longer exist from `bufftodo-lst'."
  (setq bufftodo-lst (cl-delete-if-not #'buffer-live-p bufftodo-lst)))

;;;###autoload
(defun bufftodo-clear-all ()
  "Clear all buffers from `bufftodo-lst'."
  (interactive)
  (setq bufftodo-lst '()))

;;;###autoload
(defun bufftodo-add-selected-buff ()
  "Add a manually selected buffer to `bufftodo-lst'."
  (interactive)
  (bufftodo--add (bufftodo--read (buffer-list))))

;;;###autoload
(defun bufftodo-remove-selected-buff ()
  "Manually select buffer in `bufftodo-lst', then remove it."
  (interactive)
  ;; TODO: fix bugs when buffers are killed, but still in the list.
  ;;       cache invalidation bug.
  ;;       maybe fixed now? now troubleshoot why completing-read isn't working here.
  (bufftodo--clean-deleted-buffs)
  (setq bufftodo-lst (delq (bufftodo--read bufftodo-lst) bufftodo-lst)))

;;;###autoload
(defun bufftodo-add-current-buff ()
  "Add the current buffer to the todo list."
  (interactive)
  (bufftodo--add (current-buffer)))

(require 'ibuf-ext)
;; create a filter for `ibuffer' for members of `bufftodo-lst'
(define-ibuffer-filter ;; creates a new fn `ibuffer-filter-by-todo-only'
    todo-only
    "Filters ibuffer results to memebers of `bufftodo-lst'."
  (:description "todo" :reader bufftodo-lst) ;; :reader is `qualifier' below.
  ;; buf variable is introduced in the macro.
  (memq buf qualifier))

;;;###autoload
(defun bufftodo-view ()
  "Dispaly the members of `bufftodo-lst' with `ibuffer'."
  (interactive)
  (bufftodo--clean-deleted-buffs)
  (let ((ibuffer-filtering-alist '((todo-only "todo"
                                              (lambda (buf qualifier)
                                                (member buf bufftodo-lst))))))
    (ibuffer bufftodo-open-new-window-p
             "TODO buffers"
             ibuffer-filtering-alist)))

(defvar bufftodo-ui-fn-lst
  '((            "view" . #'bufftodo-view)
    ("add current buff" . #'bufftodo-add-current-buff)
    (        "add buff" . #'bufftodo-add-selected-buff)
    (       "clear all" . #'bufftodo-clear-all)
    (     "remove buff" . #'bufftodo-remove-selected-buff))
  "An a-list of functions and their keyboard-friendly name.
These functions make up the user interface of bufftodo.
Keyboard-Friendly names are used in `completing-read' by function
`bufftodo-ui'.")

;;;###autoload
(defun bufftodo-ui ()
  "The user central user interface of bufftodo.
Allows access to all the ui functions through 1 central access function.
This function is optional; The core ui functions can be accessed directly
instead."
  (interactive)
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
  (bufftodo--clean-deleted-buffs)
  (bufftodo--read bufftodo-lst)
  bufftodo-lst
  ;; (ibuffer-filter-by-predicate
  ;;  (member buf bufftodo-lst))
  )

(provide 'bufftodo)

;;; bufftodo.el ends here
