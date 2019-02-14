;;; -*- lexical-binding: t -*-

;; (defun my-load-theme (theme &optional no-confirm no-enable)
;;   "A duplicate of `load-theme'.
;; With a mod to not ask for permission to change color.
;; See docs of `load-theme' to read about args THEME, NO-CONFIRM, NO-ENABLE."
;;   (interactive
;;    (list
;;     (intern (completing-read "Load custom theme: "
;;                              (mapcar 'symbol-name
;;                                      (custom-available-themes))))
;;     nil nil))

;;   ;;disable any active themes
;;   (dolist (theme custom-enabled-themes)
;;     (disable-theme theme))

;;   (let ((no-confirm t)
;;         (no-enable nil))
;;     (unless (custom-theme-name-valid-p theme)
;;       (error "Invalid theme name `%s'" theme))
;;     ;; If THEME is already enabled, re-enable it after loading, even if
;;     ;; NO-ENABLE is t.
;;     (if no-enable
;;         (setq no-enable (not (custom-theme-enabled-p theme))))
;;     ;; If reloading, clear out the old theme settings.
;;     (when (custom-theme-p theme)
;;       (disable-theme theme)
;;       (put theme 'theme-settings nil)
;;       (put theme 'theme-feature nil)
;;       (put theme 'theme-documentation nil))
;;     (let ((fn (locate-file (concat (symbol-name theme) "-theme.el")
;;                            (custom-theme--load-path)
;;                            '("" "c")))
;;           hash)
;;       (unless fn
;;         (error "Unable to find theme file for `%s'" theme))
;;       (with-temp-buffer
;;         (insert-file-contents fn)
;;         (setq hash (secure-hash 'sha256 (current-buffer)))
;;         ;; Check file safety with `custom-safe-themes', prompting the
;;         ;; user if necessary.
;;         (when (or no-confirm
;;                   (eq custom-safe-themes t)
;;                   (and (memq 'default custom-safe-themes)
;;                        (equal (file-name-directory fn)
;;                               (expand-file-name "themes/" data-directory)))
;;                   (member hash custom-safe-themes)
;;                   (custom-theme-load-confirm hash))
;;           (let ((custom--inhibit-theme-enable t)
;;                 (buffer-file-name fn))  ;For load-history.
;;             (eval-buffer))
;;           ;; Optimization: if the theme changes the `default' face, put that
;;           ;; entry first.  This avoids some `frame-set-background-mode' rigmarole
;;           ;; by assigning the new background immediately.
;;           (let* ((settings (get theme 'theme-settings))
;;                  (tail settings)
;;                  found)
;;             (while (and tail (not found))
;;               (and (eq (nth 0 (car tail)) 'theme-face)
;;                    (eq (nth 1 (car tail)) 'default)
;;                    (setq found (car tail)))
;;               (setq tail (cdr tail)))
;;             (if found
;;                 (put theme 'theme-settings (cons found (delq found settings)))))
;;           ;; Finally, enable the theme.
;;           (unless no-enable
;;             (enable-theme theme))
;;           t)))))


(defun my-load-theme-vim (theme &optional no-confirm no-enable)
  "Duplicate of `my-load-theme' to simulate :color in vim.
See docs of `load-theme' to read about args THEME, NO-CONFIRM, NO-ENABLE."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))
    nil nil))

  ;;disable any active themes
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))

  (let ((no-confirm t)
        (no-enable nil))
    (unless (custom-theme-name-valid-p theme)
      (error "Invalid theme name `%s'" theme))
    ;; If THEME is already enabled, re-enable it after loading, even if
    ;; NO-ENABLE is t.
    (if no-enable
        (setq no-enable (not (custom-theme-enabled-p theme))))
    ;; If reloading, clear out the old theme settings.
    (when (custom-theme-p theme)
      (disable-theme theme)
      (put theme 'theme-settings nil)
      (put theme 'theme-feature nil)
      (put theme 'theme-documentation nil))
    (let ((fn (locate-file (concat (symbol-name theme) "-theme.el")
                           (custom-theme--load-path)
                           '("" "c")))
          hash)
      (unless fn
        (error "Unable to find theme file for `%s'" theme))
      (with-temp-buffer
        (insert-file-contents fn)
        (setq hash (secure-hash 'sha256 (current-buffer)))
        ;; Check file safety with `custom-safe-themes', prompting the
        ;; user if necessary.
        (when (or no-confirm
                  (eq custom-safe-themes t)
                  (and (memq 'default custom-safe-themes)
                       (equal (file-name-directory fn)
                              (expand-file-name "themes/" data-directory)))
                  (member hash custom-safe-themes)
                  (custom-theme-load-confirm hash))
          (let ((custom--inhibit-theme-enable t)
                (buffer-file-name fn))  ;For load-history.
            (eval-buffer))
          ;; Optimization: if the theme changes the `default' face, put that
          ;; entry first.  This avoids some `frame-set-background-mode' rigmarole
          ;; by assigning the new background immediately.
          (let* ((settings (get theme 'theme-settings))
                 (tail settings)
                 found)
            (while (and tail (not found))
              (and (eq (nth 0 (car tail)) 'theme-face)
                   (eq (nth 1 (car tail)) 'default)
                   (setq found (car tail)))
              (setq tail (cdr tail)))
            (if found
                (put theme 'theme-settings (cons found (delq found settings)))))
          ;; Finally, enable the theme.
          (unless no-enable
            (enable-theme theme))
          t)))))


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