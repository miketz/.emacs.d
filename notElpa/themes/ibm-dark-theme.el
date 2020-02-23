;; ibm-dark-theme.el --- Custom theme for faces

;;; Commentary:
;;; https://gitlab.com/willvaughn/dotfiles/-/blob/master/emacs.d/themes/ibm-dark-theme.el

;;; Code:

(deftheme ibm-dark
  "Face colors using a dark IBM inspired color theme.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'ibm-dark
   `(Info-title-1-face ((,class (:family "helv" :weight bold :height 1.728))))
   `(Info-title-2-face ((,class (:family "helv" :weight bold :height 1.44))))
   `(Info-title-3-face ((,class (:family "helv" :weight bold :height 1.2))))
   `(Info-title-4-face ((,class (:family "helv" :weight bold))))

   `(cursor ((,class (:background "#0f62fe"))))
   `(default ((,class (:background "#121619" :foreground "#878d96"))))

   `(dired-marked ((,class (:background "#a56eff" :foreground "#dde1e6"))))

   `(magit-section-heading ((,class (:foreground "#a2a9b0" :weight bold))))
   `(magit-section-highlight ((,class (:background "#21272a"))))
   `(magit-diff-added ((,class (:foreground "#dde1e6" :background "#198038"))))
   `(magit-diff-added-highlight ((,class (:foreground "#dde1e6" :background "#198038"))))
   `(magit-diff-removed ((,class (:foreground "#dde1e6" :background "#6929c4"))))
   `(magit-diff-removed-highlight ((,class (:foreground "#dde1e6" :background "#6929c4"))))
   `(magit-diff-file-header ((,class (:family "Sans Serif" :height 1.1 :weight bold :foreground "#c1c7cd"))))
   `(magit-diff-hunk-header ((,class (:inherit diff-header))))
   `(magit-diff-none ((,class (:foreground "#878996"))))

   `(error ((,class (:foreground "#8a3ffc"))))
   `(flymake-errline ((,class (:background nil :underline "#a56eff"))))
   `(flymake-warnline ((,class (:background nil :underline "#ee5396"))))

   `(font-lock-builtin-face ((,class (:foreground "#08bdba"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "#c1c7cd"))))
   `(font-lock-comment-face ((,class (:foreground "#c1c7cd"))))
   `(font-lock-constant-face ((,class (:foreground "#a56eff"))))
   `(font-lock-doc-face ((,class (:foreground "#a2a9b0"))))
   `(font-lock-doc-string-face ((,class (:foreground "#a2a9b0"))))
   `(font-lock-function-name-face ((,class (:foreground "#0072c3"))))
   `(font-lock-keyword-face ((,class (:foreground "#4d5358" :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground "#d4bbff"))))
   `(font-lock-regexp-grouping-backslash ((,class (:weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:weight bold))))
   `(font-lock-string-face ((,class (:foreground "#6fdc8c"))))
   `(font-lock-type-face ((,class (:foreground "#009d9a"))))
   `(font-lock-variable-name-face ((,class (:foreground "#1192e8"))))
   `(fringe ((,class (:background "black"))))
   `(highlight ((,class (:background "#21272a"))))

   `(ido-first-match ((,class (:weight normal :foreground "orange"))))
   `(ido-only-match ((,class (:foreground "#a56eff"))))
   `(ido-subdir ((,class (:foreground nil :inherit font-lock-keyword-face))))

   ;; Ivy tweeks
   `(ivy-minibuffer-match-face-2 ((,class (:inherit highlight))))

   `(info-header-node ((,class (:foreground "#4589ff"))))
   `(info-header-xref ((,class (:foreground "#3ddbd9"))))
   `(info-menu-header ((,class (:family "helv" :weight bold))))
   `(info-node ((,class (:foreground "#4589ff"))))
   `(info-xref ((,class (:foreground "#3ddbd9"))))
   `(isearch ((,class (:background "#a56eff" :foreground "#dde1e6"))))
   `(isearch-lazy-highlight-face ((,class (:background "#491d8b" :foreground "#dde1e6"))))
   `(lazy-highlight ((,class (:background "#009d9a" :foreground "#dde1e6"))))
   `(match ((,class (:background "#a56eff" :foreground "#dde1e6"))))
   `(minibuffer-prompt ((,class (:foreground "#82cfff"))))
   `(mode-line ((,class (:background "#c6c6c6" :foreground "black" :box (:line-width 1 :style released-button)))))
   `(mode-line-buffer-id ((,class (:weight bold :background nil :foreground "#002d9c"))))
   `(mode-line-inactive ((,class (:background "#8d8d8d" :foreground "black" :box (:line-width 1 :color "#343a3f" :style nil)))))
   `(outline-1 ((,class (:foreground "#a2a9b0" :weight bold))))
   `(outline-2 ((,class (:foreground "#878d96"))))
   `(outline-3 ((,class (:foreground "#a2a9b0"))))
   `(outline-4 ((,class (:foreground "#c1c7cd"))))
   `(outline-5 ((,class (:foreground "#4d5358"))))
   `(org-todo ((,class (:foreground "#d4bbff"))))
   `(org-done ((,class (:foreground "#a7f0ba"))))
   `(region ((,class (:background "#21272a"))))
   `(show-paren-match-face ((,class (:background "#0043ce" :foreground "#dde1e6"))))
   `(show-paren-mismatch-face ((,class (:background "#6929c4" :foreground "#dde1e6"))))
   `(success ((,class (:foreground "#4589ff"))))
   `(warning ((,class (:foreground "#be95ff"))))))

(provide-theme 'ibm-dark)

;;; ibm-dark-theme.el ends here
