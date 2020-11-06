;;; fn found by johnjay in #emacs.
;;; Something do do with rectangular regions?


(defun mouse-start-end (start end mode)
  (if (> start end)
      (let ((temp start))
        (setq start end
              end temp)))

  (cond ((= mode 0)
         (list start end))
        ((and (= mode 1)
              (= start end)
              (char-after start)
              (= (char-syntax (char-after start)) ?\())
         (if (/= (syntax-class (syntax-after start)) 4) ; raw syntax code for ?\(
             ;; This happens in CC Mode when unbalanced parens in CPP
             ;; constructs are given punctuation syntax with
             ;; syntax-table text properties.  (2016-02-21).
             (signal 'scan-error (list "Containing expression ends prematurely"
                                       start start))
           (list start
                 (save-excursion
                   (goto-char start)
                   (forward-sexp 1)
                   (point)))))
        ((and (= mode 1)
              (= start end)
              (char-after start)
              (= (char-syntax (char-after start)) ?\)))
         (if (/= (syntax-class (syntax-after start)) 5) ; raw syntax code for ?\)
             ;; See above comment about CC Mode.
             (signal 'scan-error (list "Unbalanced parentheses" start start))
           (list (save-excursion
                   (goto-char (1+ start))
                   (backward-sexp 1)
                   (point))
                 (1+ start))))
        ((and (= mode 1)
              (= start end)
              (char-after start)
              (= (char-syntax (char-after start)) ?\"))
         (let ((open (or (eq start (point-min))
                         (save-excursion
                           (goto-char (- start 1))
                           (looking-at "\\s(\\|\\s \\|\\s>")))))
           (if open
               (list start
                     (save-excursion
                       (condition-case nil
                           (progn
                             (goto-char start)
                             (forward-sexp 1)
                             (point))
                         (error end))))
             (list (save-excursion
                     (condition-case nil
                         (progn
                           (goto-char (1+ start))
                           (backward-sexp 1)
                           (point))
                       (error end)))
                   (1+ start)))))
        ((= mode 1)
         (list (save-excursion
                 (goto-char start)
                 (mouse-skip-word -1)
                 (point))
               (save-excursion
                 (goto-char end)
                 (mouse-skip-word 1)
                 (point))))
        ((= mode 2)
         (list (save-excursion
                 (goto-char start)
                 (line-beginning-position 1))
               (save-excursion
                 (goto-char end)
                 (forward-line 1)
                 (point))))
        ((> mode 2)              ;Add in this to select multiple lines
         (list (save-excursion
                 (goto-char start)
                 (line-beginning-position 1))
               (save-excursion
                 (goto-char end)
                 (forward-line (- mode 1))
                 (point))))))