(defun my-load-theme (theme &optional no-confirm no-enable)
  "A duplicate of `load-theme'.
With a mod to not ask for permission to change color.
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
(defvar my-c-index 0)
(defun my-cycle-theme ()
  (interactive)
  (let* ((themes (custom-available-themes))
         (thm (nth my-c-index themes)))
    (unwind-protect
        (progn
          (load-theme thm t))
      (progn
        (print thm)
        (incf my-c-index)
        (when (= my-c-index (length themes))
          (setq my-c-index 0))))))



;; (defvar cycle-colors2 '("papaya whip" "old lace" "floral white" "ivory2"
;;                         "mint cream" "honeydew" "white smoke" "ghost white"
;;                         "snow" "alice blue" "lavender"))
(defvar cycle-colors `("old lace" "floral white" "snow" "ghost white" "white smoke" "white"
                       "#F3F1DE" "#F3F2EA" ,mayan-smoke))
(defvar cycle-index 0)
(defun my-cycle-light-bg ()
  (interactive)
  (if (= cycle-index (1- (length cycle-colors)))
      (setq cycle-index 0)
    (setq cycle-index (1+ cycle-index)))
  (let ((bg (my-getAtIndex cycle-index cycle-colors)))
    (set-background-color (my-getAtIndex cycle-index cycle-colors))
    (message bg)))
