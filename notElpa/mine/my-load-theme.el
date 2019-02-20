;;; -*- lexical-binding: t -*-

;;custom-enabled-themes
;;custom-safe-themes
;;custom-known-themes
;;(custom-available-themes)
(let ((i 0))
  (defun my-cycle-theme ()
    (interactive)
    (let* ((themes (custom-available-themes))
           (len (length themes))
           (thm (nth i themes)))
      (unwind-protect
          (progn
            (load-theme thm t))
        (progn
          (message "theme %d/%d %s"
                   (1+ i) len (symbol-name thm))
          (setq i (1+ i))
          (when (= i len)
            (setq i 0)))))))



;; (defvar cycle-colors2 '("papaya whip" "old lace" "floral white" "ivory2"
;;                         "mint cream" "honeydew" "white smoke" "ghost white"
;;                         "snow" "alice blue" "lavender"))

;;        format (color . description)
(let* ((colors `[("old lace" . "old lace")
                 ("floral white" . "floral white" )
                 ("snow" . "snow")
                 ("ghost white" . "ghost white")
                 ("white smoke" . "white smoke")
                 ("white" . "white")
                 ("#F3F1DE" . "my bg 1")
                 ("#F3F2EA" . "my bg 2")
                 (,mayan-smoke . "myan-smoke")
                 (,my-ultimate . "my-ultimate")
                 ("ivory" . "ivory")
                 ("ivory2" . "ivory2")
                 ("ivory3" . "ivory3")])
       (len (length colors))
       (i 0))

  (defun my-cycle-light-bg-set-index (new-i)
    (interactive "n") ; read new index from minibuffer if not provided.
    (setq i (if (or (>= new-i len)
                    (< new-i 0))
                (mod new-i len) ; if i out of range then wrap via mod.
              new-i))
    (let* ((pair (aref colors i))
           (bg (car pair))
           (descr (cdr pair)))
      (set-background-color bg)
      (message (format "color %d/%d. %s, %s"
                       (1+ i) len bg descr))))

  (defun my-cycle-light-bg (step)
    (interactive "n") ; read `step' from minibuffer if not provided.
    (my-cycle-light-bg-set-index (+ i step)))

  (defun my-cycle-light-bg-forward ()
    (interactive)
    (my-cycle-light-bg 1))

  (defun my-cycle-light-bg-backward ()
    (interactive)
    (my-cycle-light-bg -1)))

(provide 'my-load-theme)