;;; greenit-dark-theme.el --- Greenit green-injected Emacs theme -*- lexical-binding: t -*-
;;; Recipe: energreen, slightly greener — every neutral hue-shifted toward
;;; green, every accent a green derivative.

(deftheme greenit-dark
  "Greenit: a green-injected theme family — {{variant}} variant.
Every neutral is hue-shifted toward green; every accent is a green derivative."
  :family 'greenit
  :kind 'color-scheme
  :background-mode '{{variant}})

(custom-theme-set-faces
 'greenit-dark
  ;; UI
 '(default ((t (:foreground "#C9E4CE" :background "#0C1F17"))))
 '(cursor ((t (:background "#7BD88F" :foreground "#0C1F17"))))
 '(region ((t (:background "#7BD88F" :foreground "#0C1F17"))))
 '(hl-line ((t (:background "#122A1F"))))
 '(fringe ((t (:background "#0C1F17"))))
 '(mode-line ((t (:foreground "#C9E4CE" :background "#122A1F" :bold t))))
 '(mode-line-inactive ((t (:foreground "#78997F" :background "#0C1F17"))))
 '(mode-line-buffer-id ((t (:bold t :foreground "#7BD88F" :background "#122A1F"))))
 '(mode-line-highlight ((t (:foreground "#B8CC7A" :background "#122A1F" :weight bold))))
 '(minibuffer-prompt ((t (:bold t :foreground "#A88EA0"))))

 ;; Syntax — mirrors colors/greenit.vim
 '(font-lock-comment-face ((t (:foreground "#78997F" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "#A88EA0" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "#63C9A8" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#7BD88F"))))
 '(font-lock-variable-name-face ((t (:foreground "#C9E4CE"))))
 '(font-lock-type-face ((t (:foreground "#7FD7C4"))))
 '(font-lock-constant-face ((t (:foreground "#A88EA0"))))
 '(font-lock-builtin-face ((t (:foreground "#C9E4CE"))))
 '(font-lock-preprocessor-face ((t (:foreground "#A88EA0"))))
 '(font-lock-warning-face ((t (:foreground "#C07A6E"))))
 '(font-lock-number-face ((t (:foreground "#7FD7C4"))))
 '(font-lock-operator-face ((t (:foreground "#B8CC7A"))))

 ;; Diff / org / misc
 '(diff-added ((t (:foreground "#7BD88F" :background "#122A1F"))))
 '(diff-removed ((t (:foreground "#C07A6E" :background "#122A1F"))))
 '(diff-changed ((t (:foreground "#B8CC7A" :background "#122A1F"))))
 '(org-code ((t (:foreground "#7BD88F"))))
 '(org-link ((t (:foreground "#7FD7C4" :underline t))))
 '(org-todo ((t (:weight bold :foreground "#A88EA0"))))

 ;; Highlights
 '(highlight ((t (:foreground "#C9E4CE" :background "#B8CC7A"))))
 '(link ((t (:foreground "#7FD7C4" :underline t))))
 '(error ((t (:foreground "#C07A6E" :weight bold))))
 '(warning ((t (:foreground "#B8CC7A"))))

 ;; Completion / UI extras
 '(company-preview ((t (:background "#122A1F" :foreground "#C9E4CE"))))
 '(company-tooltip ((t (:foreground "#C9E4CE" :background "#122A1F" :bold t))))
 '(magit-section-heading ((t (:foreground "#7BD88F" :weight bold))))
 '(magit-item-highlight ((t (:background "#122A1F")))))

(provide-theme 'greenit-dark)
;;; greenit-dark-theme.el ends here
