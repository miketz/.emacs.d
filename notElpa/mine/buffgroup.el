;;; buffgroup.el --- Define groups of buffers. -*- lexical-binding: t -*-

;;; Commentary:
;;; Define groups of buffers.

;;; Code:
(require 'cl-lib)
(require 'ibuffer)

(defvar buffgroup-groups '()
  "Association list of buffer group names and their associated buffers.
A buffer may belong to multiple groups.
Format: ((\"group-1\" b1 b2 b3)
         (\"group-2\" b1 b4 b5))")

;; TODO: use this var
(defvar buffgroup-curr-active-group nil
  "The current group to limit buffer operations on.")


(let ((seq 1))
  (defun buffgroup-gen-default-name ()
    "Generate an arbitrary, but unique group name.
Useful if you want to quickly create a group(s) and don't care about the
labeling."
    (prog1
        (concat "group-" (int-to-string seq))
      (cl-incf seq))))

(defun buffgroup-create (name)
  "Create buffer group."
  (add-to-list 'buffgroup-groups `(,name . ())))

(defun buffgroup-remove (name)
  "Remove buffer group."
  (setq buffgroup-groups
        (delete (assoc name buffgroup-groups)
                buffgroup-groups)))

(defun buffgroup-add-buffer (group-name buff)
  (let ((group-buffers (cdr (assoc group-name buffgroup-groups))))
    (add-to-list 'group-buffers buff)
    (setf (cdr (assoc group-name buffgroup-groups))
          group-buffers)))

(defun buffgroup-remove-buffer (group-name buff)
  (let ((group-buffers (cdr (assoc group-name buffgroup-groups))))
    (setq group-buffers (delete buff group-buffers))
    (setf (cdr (assoc group-name buffgroup-groups))
          group-buffers)))


(defun buffgroup-cleanup-killed-buffers ()
  ;; todo: implement. `buffgroup-list-buffers' currently breaks if there
  ;; are killed buffers remaining in the list.
  )

(defvar *group-buffers* '()
  "Dynamic var. Used to limit dispaly of buffers in custom ibuffer fn.")

(defun buffgroup-list-buffers (group-name)
  ;; dynamically shadw `*group-buffers*' with buffers of the current group only
  (let* ((*group-buffers* (cdr (assoc group-name buffgroup-groups))))
    (buffgroup-ibuffer nil ; other-window-p
                       (concat "*Ibuffer*-" group-name) ; name
                       )

    ;; from `projectile-buffer'
    ;; (ibuffer nil
    ;;          (format "*%s Buffers*" project-name)
    ;;          (list (cons 'projectile-files project-root)))
    ))

;; I couldn't figure out how the `qualifiers' param worked in ibuffer.
;; So just forcefully making a new version that only lists the buffers I want.
(defun buffgroup-ibuffer (&optional other-window-p name qualifiers noselect
			  shrink filter-groups formats)
  "Begin using Ibuffer to edit a list of buffers.
Type \\<ibuffer-mode-map>\\[describe-mode] after entering ibuffer for more information.

All arguments are optional.
OTHER-WINDOW-P says to use another window.
NAME specifies the name of the buffer (defaults to \"*Ibuffer*\").
QUALIFIERS is an initial set of filtering qualifiers to use;
  see `ibuffer-filtering-qualifiers'.
NOSELECT means don't select the Ibuffer buffer.
SHRINK means shrink the buffer to minimal size.  The special
  value `onewindow' means always use another window.
FILTER-GROUPS is an initial set of filtering groups to use;
  see `ibuffer-filter-groups'.
FORMATS is the value to use for `ibuffer-formats'.
  If specified, then the variable `ibuffer-formats' will have
  that value locally in this buffer."
  (interactive "P")
  (when ibuffer-use-other-window
    (setq other-window-p t))
  (let ((buf (get-buffer-create (or name "*Ibuffer*"))))
    (if other-window-p
	(or (and noselect (display-buffer buf t))
	    (pop-to-buffer buf t))
      (funcall (if noselect #'display-buffer #'switch-to-buffer) buf))
    (with-current-buffer buf
      (save-selected-window
	;; We switch to the buffer's window in order to be able
	;; to modify the value of point
	(select-window (get-buffer-window buf 0))
	(or (derived-mode-p 'ibuffer-mode)
	    (ibuffer-mode))
    (when shrink
      (setq ibuffer-shrink-to-minimum-size shrink))
	(when qualifiers
	  (require 'ibuf-ext)
	  (setq ibuffer-filtering-qualifiers qualifiers))
	(when filter-groups
	  (require 'ibuf-ext)
	  (setq ibuffer-filter-groups filter-groups))
	(when formats
	  (setq-local ibuffer-formats formats))
	(my-ibuffer-update nil)
	;; Skip the group name by default.
	(ibuffer-forward-line 0 t)
	(unwind-protect
	    (progn
	      (setq buffer-read-only nil)
	      (run-hooks 'ibuffer-hook))
	  (setq buffer-read-only t))
	(unless ibuffer-expert
          (message (substitute-command-keys
                    (concat "Commands: \\[ibuffer-mark-forward], "
                            "\\[ibuffer-unmark-forward], "
                            "\\[ibuffer-toggle-marks], "
                            "\\[ibuffer-visit-buffer], "
                            "\\[ibuffer-update], "
                            "\\[ibuffer-do-kill-lines], "
                            "\\[ibuffer-do-save], "
                            "\\[ibuffer-do-delete], "
                            "\\[ibuffer-do-query-replace]; "
                            "\\[quit-window] to quit; "
                            "\\[describe-mode] for help"))))))))

(defun my-ibuffer-update (arg &optional silent)
  "Regenerate the list of all buffers.

Prefix arg non-nil means to toggle whether buffers that match
`ibuffer-maybe-show-predicates' should be displayed.

If optional arg SILENT is non-nil, do not display progress messages."
  (interactive "P")
  (if arg
      (setq ibuffer-display-maybe-show-predicates
	    (not ibuffer-display-maybe-show-predicates)))
  (ibuffer-forward-line 0)
  (let* ((bufs *group-buffers*) ;; changed this part!
	 (blist (ibuffer-filter-buffers
		 (current-buffer)
		 (if (and
		      (cadr bufs)
		      (eq ibuffer-always-show-last-buffer
			  :nomini)
		      (minibufferp (cadr bufs)))
		     (nth 2 bufs)
		   (cadr bufs))
		 (ibuffer-current-buffers-with-marks bufs)
		 ibuffer-display-maybe-show-predicates)))
    (and (null blist)
	 (featurep 'ibuf-ext)
	 ibuffer-filtering-qualifiers
	 (message "No buffers! (note: filtering in effect)"))
    (unless silent
      (message "Updating buffer list..."))
    (ibuffer-redisplay-engine blist arg)
    (unless silent
      (message "Updating buffer list...done")))
  (if (eq ibuffer-shrink-to-minimum-size 'onewindow)
      (ibuffer-shrink-to-fit t)
    (when ibuffer-shrink-to-minimum-size
      (ibuffer-shrink-to-fit)))
  (ibuffer-forward-line 0)
  ;; I tried to update this automatically from the mode-line-process format,
  ;; but changing nil-ness of header-line-format while computing
  ;; mode-line-format is asking a bit too much it seems.  --Stef
  (setq header-line-format
        (and ibuffer-use-header-line
             ibuffer-filtering-qualifiers
             ibuffer-header-line-format)))

(provide 'buffgroup)

;;; buffgroup.el ends here