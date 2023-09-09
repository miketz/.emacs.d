;;; -*- lexical-binding: t -*-

(require 'replace) ; for `occur'
(require 'xref) ; for `xref-pulse-momentarily'

;; ;; automatically scroll buffer to matches like `swiper' or `helm-swoop'.
;; (add-hook 'occur-mode-hook #'next-error-follow-minor-mode)

(defvar my-blink-fn #'xref-pulse-momentarily
  ;; (if (>= emacs-major-version 25)
  ;;     (progn
  ;;       (require 'xref) ;; for fn `xref-pulse-momentarily'. to flash match.
  ;;       #'xref-pulse-momentarily)
  ;;   #'hl-line-flash) ;; TODO: avoid hl-line+ dependency ;; (hl-line-mode 1)
  "Function to blink the matching line in the buffer from occur-buffer.")

(progn ;; functions copied from https://github.com/emacsfodder/occur-follow
  (defun my--occur-move (move-fn)
    (funcall move-fn)
    (occur-mode-goto-occurrence-other-window)
    (recenter)
    (funcall my-blink-fn)
    (switch-to-buffer-other-window "*Occur*"))
  (defun my-occur-next ()
    (interactive)
    (my--occur-move #'occur-next))
  (defun my-occur-prev ()
    (interactive)
    (my--occur-move #'occur-prev))
  (defun my-occur-mode-goto-occurrence ()
    "Same as the built in `occur-mode-goto-occurrence', but add a blink."
    (interactive)
    (occur-mode-goto-occurrence)
    (funcall my-blink-fn)))

;; turn off the line highlight when jumping back to the buffer.
;; close the occur window when jumping back to the buffer.
(defadvice occur-mode-goto-occurrence (after turn-off-highlight)
  ;; (hl-line-mode 0)
  ;; close occur window.
  (quit-window nil (get-buffer-window "*Occur*")))
(ad-activate 'occur-mode-goto-occurrence)

(defun my-occur-wild-spaces (regexp &optional nlines)
  "Same as `occur'.  But treat spaces as wild cards like in `swiper'."
  (interactive (occur-read-primary-args))
  (occur-1 (replace-regexp-in-string " " ".*" regexp)
           nlines
           (list (current-buffer))))
;; ;; treat spaces as wild cards. Like in `swiper'.
;; (defadvice occur (around space-to-wild activate compile)
;;   (let* ((new-regexp (replace-regexp-in-string " " ".*" regexp))
;;          (regexp new-regexp))
;;     ad-do-it))

(defun my--occur-jump-to-first-match ()
  ;; switch to the results window immediately.
  (switch-to-buffer-other-window "*Occur*")
  ;; jump to the first match.
  (my-occur-next))
(add-hook 'occur-hook #'my--occur-jump-to-first-match)

(provide 'my-occur-wild-spaces)