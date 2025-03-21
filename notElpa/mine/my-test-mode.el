;;; my-test-mode.el --- testing minor mode -*- lexical-binding: t -*-
;;; temporary scratch pad while i figure out how to do sql server completion

(require 'posframe)
(require 'ivy)
(require 'ivy-posframe)

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

;;; my-test-mode.el ends here