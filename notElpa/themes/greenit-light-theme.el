;;; greenit-light-theme.el --- Greenit green-injected Emacs theme -*- lexical-binding: t -*-
;;; Recipe: energreen, slightly greener — every neutral hue-shifted toward
;;; green, every accent a green derivative.

(deftheme greenit-light
  "Greenit: a green-injected theme family — {{variant}} variant.
Every neutral is hue-shifted toward green; every accent is a green derivative."
  :family 'greenit
  :kind 'color-scheme
  :background-mode '{{variant}})

(custom-theme-set-faces
 'greenit-light
  ;; UI
 '(default ((t (:foreground "#24382C" :background "#F2F7EF"))))
 '(cursor ((t (:background "#7BD88F" :foreground "#F2F7EF"))))
 '(region ((t (:background "#7BD88F" :foreground "#F2F7EF"))))
 '(hl-line ((t (:background "#E6EFE2"))))
 '(fringe ((t (:background "#F2F7EF"))))
 '(mode-line ((t (:foreground "#24382C" :background "#E6EFE2" :bold t))))
 '(mode-line-inactive ((t (:foreground "#56695C" :background "#F2F7EF"))))
 '(mode-line-buffer-id ((t (:bold t :foreground "#7BD88F" :background "#E6EFE2"))))
 '(mode-line-highlight ((t (:foreground "#B8CC7A" :background "#E6EFE2" :weight bold))))
 '(minibuffer-prompt ((t (:bold t :foreground "#A88EA0"))))

 ;; Syntax — mirrors colors/greenit.vim
 '(font-lock-comment-face ((t (:foreground "#56695C" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "#A88EA0" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "#63C9A8" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#7BD88F"))))
 '(font-lock-variable-name-face ((t (:foreground "#24382C"))))
 '(font-lock-type-face ((t (:foreground "#7FD7C4"))))
 '(font-lock-constant-face ((t (:foreground "#A88EA0"))))
 '(font-lock-builtin-face ((t (:foreground "#24382C"))))
 '(font-lock-preprocessor-face ((t (:foreground "#A88EA0"))))
 '(font-lock-warning-face ((t (:foreground "#C07A6E"))))
 '(font-lock-number-face ((t (:foreground "#7FD7C4"))))
 '(font-lock-operator-face ((t (:foreground "#B8CC7A"))))

 ;; Diff / org / misc
 '(diff-added ((t (:foreground "#7BD88F" :background "#E6EFE2"))))
 '(diff-removed ((t (:foreground "#C07A6E" :background "#E6EFE2"))))
 '(diff-changed ((t (:foreground "#B8CC7A" :background "#E6EFE2"))))
 '(org-code ((t (:foreground "#7BD88F"))))
 '(org-link ((t (:foreground "#7FD7C4" :underline t))))
 '(org-todo ((t (:weight bold :foreground "#A88EA0"))))

 ;; Highlights
 '(highlight ((t (:foreground "#24382C" :background "#B8CC7A"))))
 '(link ((t (:foreground "#7FD7C4" :underline t))))
 '(error ((t (:foreground "#C07A6E" :weight bold))))
 '(warning ((t (:foreground "#B8CC7A"))))

 ;; Completion / UI extras
 '(company-preview ((t (:background "#E6EFE2" :foreground "#24382C"))))
 '(company-tooltip ((t (:foreground "#24382C" :background "#E6EFE2" :bold t))))
 '(magit-section-heading ((t (:foreground "#7BD88F" :weight bold))))
 '(magit-item-highlight ((t (:background "#E6EFE2")))))

(provide-theme 'greenit-light)
;;; greenit-light-theme.el ends here
