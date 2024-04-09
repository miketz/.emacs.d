;;; my-swift-helpers.el --- helper funcs for swift code -*- lexical-binding: t -*-

(require 'ivy)
(require 'hydra)

(defvar my-swift-types
  '("Int" "Int64" "Int32" "Int16" "Int8" "UInt64" "UInt32" "UInt16" "UInt8"
    "Float" "Double" "Bool" "Character" "String")
  "Built in types.")

;;;###autoload
(defun my-swift-insert-type ()
  "Select and insert a go type with completing-read."
  (interactive)
  (let ((completing-read-function #'ivy-completing-read)
        ;; dynamically shadow ivy completion style to ignore order.
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        ;; taller ivy window. -4 so scrolling doens't go off screen.
        (ivy-height (- (window-height) 4)))
    (insert (completing-read "type: " my-swift-types))))


;; List several go helper functions.
(defhydra my-swift-commands-hydra (:color blue :hint nil) ;;(:color blue)
  "
_t_: types
_q_, _C-g_: quit"

  ("t" my-swift-insert-type)

  ;; don't use the hint text as it makes (:hint nil) not work?
  ;; ("t" my-swift-insert-type "types")

  ("C-g" nil nil)
  ("q" nil))

;;; my-swift-helpers.el ends here
