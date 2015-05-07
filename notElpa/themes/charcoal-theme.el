;;; charcoal-theme.el --- A dark theme with pastelish chalkish charcoal colors.

;;; Credits: Using zenburn-theme.el from Bozhidar Batsov as a base.

;;; Commentary:
;;; Keywords should appear "raised" or "3D" against the bg.

;;; Code:

(deftheme charcoal "The charcoal color theme")

;;background: #35352B #000000

;;; Color Palette


(defvar charcoal-colors-alist
  '(("charcoal-bg"       . "#35352B")
    ("charcoal-fg"       . "#DCDCCC")
    ("charcoal-fg+1"     . "#FFFFEF")
    ("charcoal-fg-1"     . "#656555")
    ("charcoal-bg-2"     . "#000000")
    ("charcoal-bg-1"     . "#2B2B2B")
    ("charcoal-bg-05"    . "#383838")
    ("charcoal-bg+05"    . "#494949")
    ("charcoal-bg+1"     . "#4F4F4F")
    ("charcoal-bg+2"     . "#5F5F5F")
    ("charcoal-bg+3"     . "#6F6F6F")
    ("charcoal-red+1"    . "#DCA3A3")
    ("charcoal-red"      . "#CC9393")
    ("charcoal-red-1"    . "#BC8383")
    ("charcoal-red-2"    . "#AC7373")
    ("charcoal-red-3"    . "#9C6363")
    ("charcoal-red-4"    . "#8C5353")
    ("charcoal-orange"   . "#DFAF8F")
    ("charcoal-yellow"   . "#F0DFAF")
    ("charcoal-yellow-1" . "#E0CF9F")
    ("charcoal-yellow-2" . "#D0BF8F")
    ("charcoal-green-1"  . "#5F7F5F")
    ("charcoal-green"    . "#7F9F7F")
    ("charcoal-green+1"  . "#8FB28F")
    ("charcoal-green+2"  . "#9FC59F")
    ("charcoal-green+3"  . "#AFD8AF")
    ("charcoal-green+4"  . "#BFEBBF")
    ("charcoal-cyan"     . "#93E0E3")
    ("charcoal-blue+1"   . "#94BFF3")
    ("charcoal-blue"     . "#8CD0D3")
    ("charcoal-blue-1"   . "#7CB8BB")
    ("charcoal-blue-2"   . "#6CA0A3")
    ("charcoal-blue-3"   . "#5C888B")
    ("charcoal-blue-4"   . "#4C7073")
    ("charcoal-blue-5"   . "#366060")
    ("charcoal-magenta"  . "#DC8CC3"))
  "List of charcoal colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

;; for dynamic re=evaluation while in dev. Also keeps a "bakcup" of the original charcoal colors
;; ready to go. Delete this `setq' when finished.
(setq charcoal-colors-alist
      '(("charcoal-bg"       . "#35352B")
        ("charcoal-fg"       . "#DCDCCC")
        ("charcoal-fg+1"     . "#FFFFEF")
        ("charcoal-fg-1"     . "#656555")
        ("charcoal-bg-2"     . "#000000")
        ("charcoal-bg-1"     . "#2B2B2B")
        ("charcoal-bg-05"    . "#383838")
        ("charcoal-bg+05"    . "#494949")
        ("charcoal-bg+1"     . "#4F4F4F")
        ("charcoal-bg+2"     . "#5F5F5F")
        ("charcoal-bg+3"     . "#6F6F6F")
        ("charcoal-red+1"    . "#DCA3A3")
        ("charcoal-red"      . "#CC9393")
        ("charcoal-red-1"    . "#BC8383")
        ("charcoal-red-2"    . "#AC7373")
        ("charcoal-red-3"    . "#9C6363")
        ("charcoal-red-4"    . "#8C5353")
        ("charcoal-orange"   . "#DFAF8F")
        ("charcoal-yellow"   . "#F0DFAF")
        ("charcoal-yellow-1" . "#E0CF9F")
        ("charcoal-yellow-2" . "#D0BF8F")
        ("charcoal-green-1"  . "#5F7F5F")
        ("charcoal-green"    . "#7F9F7F")
        ("charcoal-green+1"  . "#8FB28F")
        ("charcoal-green+2"  . "#9FC59F")
        ("charcoal-green+3"  . "#AFD8AF")
        ("charcoal-green+4"  . "#BFEBBF")
        ("charcoal-cyan"     . "#93E0E3")
        ("charcoal-blue+1"   . "#94BFF3")
        ("charcoal-blue"     . "#8CD0D3")
        ("charcoal-blue-1"   . "#7CB8BB")
        ("charcoal-blue-2"   . "#6CA0A3")
        ("charcoal-blue-3"   . "#5C888B")
        ("charcoal-blue-4"   . "#4C7073")
        ("charcoal-blue-5"   . "#366060")
        ("charcoal-magenta"  . "#DC8CC3")))


(defmacro charcoal-with-color-variables (&rest body)
  "`let' bind all colors defined in `charcoal-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   charcoal-colors-alist))
     ,@body))

;;; Theme Faces
(charcoal-with-color-variables
  (custom-theme-set-faces
   'charcoal
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,charcoal-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,charcoal-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,charcoal-fg :background ,charcoal-bg))))
   `(cursor ((t (:foreground ,charcoal-fg :background ,charcoal-fg+1))))
   `(escape-glyph ((t (:foreground ,charcoal-yellow :bold t))))
   `(fringe ((t (:foreground ,charcoal-fg :background ,charcoal-bg+1))))
   `(header-line ((t (:foreground ,charcoal-yellow
                                  :background ,charcoal-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,charcoal-bg-05))))
   `(success ((t (:foreground ,charcoal-green :weight bold))))
   `(warning ((t (:foreground ,charcoal-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,charcoal-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,charcoal-green))))
   `(compilation-error-face ((t (:foreground ,charcoal-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,charcoal-fg))))
   `(compilation-info-face ((t (:foreground ,charcoal-blue))))
   `(compilation-info ((t (:foreground ,charcoal-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,charcoal-green))))
   `(compilation-line-face ((t (:foreground ,charcoal-yellow))))
   `(compilation-line-number ((t (:foreground ,charcoal-yellow))))
   `(compilation-message-face ((t (:foreground ,charcoal-blue))))
   `(compilation-warning-face ((t (:foreground ,charcoal-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,charcoal-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,charcoal-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,charcoal-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,charcoal-fg-1))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,charcoal-fg))))
   `(grep-error-face ((t (:foreground ,charcoal-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,charcoal-blue))))
   `(grep-match-face ((t (:foreground ,charcoal-orange :weight bold))))
   `(match ((t (:background ,charcoal-bg-1 :foreground ,charcoal-orange :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,charcoal-yellow-2 :weight bold :background ,charcoal-bg+2))))
   `(isearch-fail ((t (:foreground ,charcoal-fg :background ,charcoal-red-4))))
   `(lazy-highlight ((t (:foreground ,charcoal-yellow-2 :weight bold :background ,charcoal-bg-05))))

   `(menu ((t (:foreground ,charcoal-fg :background ,charcoal-bg))))
   `(minibuffer-prompt ((t (:foreground ,charcoal-yellow))))
   `(mode-line
     ((,class (:foreground ,charcoal-green+1
                           :background ,charcoal-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,charcoal-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,charcoal-green-1
                      :background ,charcoal-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,charcoal-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,charcoal-bg+2))))
   `(trailing-whitespace ((t (:background ,charcoal-red))))
   `(vertical-border ((t (:foreground ,charcoal-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,charcoal-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,charcoal-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,charcoal-green-1))))
   `(font-lock-constant-face ((t (:foreground ,charcoal-green+4))))
   `(font-lock-doc-face ((t (:foreground ,charcoal-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,charcoal-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,charcoal-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,charcoal-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,charcoal-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,charcoal-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,charcoal-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,charcoal-red))))
   `(font-lock-type-face ((t (:foreground ,charcoal-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,charcoal-orange))))
   `(font-lock-warning-face ((t (:foreground ,charcoal-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,charcoal-fg))))
   `(newsticker-default-face ((t (:foreground ,charcoal-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,charcoal-green+3))))
   `(newsticker-extra-face ((t (:foreground ,charcoal-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,charcoal-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,charcoal-green))))
   `(newsticker-new-item-face ((t (:foreground ,charcoal-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,charcoal-red))))
   `(newsticker-old-item-face ((t (:foreground ,charcoal-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,charcoal-fg))))
   `(newsticker-treeview-face ((t (:foreground ,charcoal-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,charcoal-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,charcoal-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,charcoal-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,charcoal-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,charcoal-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,charcoal-bg-1 :foreground ,charcoal-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,charcoal-fg-1 :background ,charcoal-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,charcoal-green+2 :background ,charcoal-bg :inverse-video nil))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,charcoal-green+1))))
   `(android-mode-error-face ((t (:foreground ,charcoal-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,charcoal-fg))))
   `(android-mode-verbose-face ((t (:foreground ,charcoal-green))))
   `(android-mode-warning-face ((t (:foreground ,charcoal-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,charcoal-cyan :weight bold))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,charcoal-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,charcoal-yellow))))
   `(font-latex-italic-face ((t (:foreground ,charcoal-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,charcoal-orange))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,charcoal-bg+3 :foreground ,charcoal-bg-2))))
   `(ac-selection-face ((t (:background ,charcoal-blue-4 :foreground ,charcoal-fg))))
   `(popup-tip-face ((t (:background ,charcoal-yellow-2 :foreground ,charcoal-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,charcoal-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,charcoal-bg-1))))
   `(popup-isearch-match ((t (:background ,charcoal-bg :foreground ,charcoal-fg))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,charcoal-fg :background ,charcoal-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,charcoal-orange :background ,charcoal-bg+1))))
   `(company-tooltip-selection ((t (:foreground ,charcoal-fg :background ,charcoal-bg-1))))
   `(company-tooltip-mouse ((t (:background ,charcoal-bg-1))))
   `(company-tooltip-common ((t (:foreground ,charcoal-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,charcoal-green+2))))
   `(company-scrollbar-fg ((t (:background ,charcoal-bg-1))))
   `(company-scrollbar-bg ((t (:background ,charcoal-bg+2))))
   `(company-preview ((t (:background ,charcoal-green+2))))
   `(company-preview-common ((t (:foreground ,charcoal-green+2 :background ,charcoal-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,charcoal-yellow-1 :foreground ,charcoal-bg))))
   `(bm-fringe-face ((t (:background ,charcoal-yellow-1 :foreground ,charcoal-bg))))
   `(bm-fringe-persistent-face ((t (:background ,charcoal-green-1 :foreground ,charcoal-bg))))
   `(bm-persistent-face ((t (:background ,charcoal-green-1 :foreground ,charcoal-bg))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,charcoal-cyan))))
   `(circe-my-message-face ((t (:foreground ,charcoal-fg))))
   `(circe-fool-face ((t (:foreground ,charcoal-red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,charcoal-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,charcoal-fg))))
   `(circe-server-face ((t (:foreground ,charcoal-green))))
   `(circe-topic-diff-new-face ((t (:foreground ,charcoal-orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,charcoal-orange :background ,charcoal-bg :weight bold))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,charcoal-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,charcoal-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,charcoal-green+1 :weight bold :underline t))))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,charcoal-blue :foreground ,charcoal-bg))))
   `(ctbl:face-continue-bar ((t (:background ,charcoal-bg-05 :foreground ,charcoal-bg))))
   `(ctbl:face-row-select ((t (:background ,charcoal-cyan :foreground ,charcoal-bg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,charcoal-green+4 :background nil))
                 (t (:foreground ,charcoal-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,charcoal-yellow))))
   `(diff-removed ((,class (:foreground ,charcoal-red :background nil))
                   (t (:foreground ,charcoal-red-3 :background nil))))
   `(diff-refine-added ((t (:inherit diff-added :weight bold))))
   `(diff-refine-change ((t (:inherit diff-changed :weight bold))))
   `(diff-refine-removed ((t (:inherit diff-removed :weight bold))))
   `(diff-header ((,class (:background ,charcoal-bg+2))
                  (t (:background ,charcoal-fg :foreground ,charcoal-bg))))
   `(diff-file-header
     ((,class (:background ,charcoal-bg+2 :foreground ,charcoal-fg :bold t))
      (t (:background ,charcoal-fg :foreground ,charcoal-bg :bold t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,charcoal-blue-2 :background ,charcoal-bg-05))))
   `(diff-hl-delete ((,class (:foreground ,charcoal-red+1 :background ,charcoal-bg-05))))
   `(diff-hl-insert ((,class (:foreground ,charcoal-green+1 :background ,charcoal-bg-05))))
   `(diff-hl-unknown ((,class (:foreground ,charcoal-yellow :background ,charcoal-bg-05))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,charcoal-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,charcoal-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,charcoal-orange))))
   `(diredp-date-time ((t (:foreground ,charcoal-magenta))))
   `(diredp-deletion ((t (:foreground ,charcoal-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,charcoal-red))))
   `(diredp-dir-heading ((t (:foreground ,charcoal-blue :background ,charcoal-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,charcoal-cyan))))
   `(diredp-exec-priv ((t (:foreground ,charcoal-red))))
   `(diredp-executable-tag ((t (:foreground ,charcoal-green+1))))
   `(diredp-file-name ((t (:foreground ,charcoal-blue))))
   `(diredp-file-suffix ((t (:foreground ,charcoal-green))))
   `(diredp-flag-mark ((t (:foreground ,charcoal-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,charcoal-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,charcoal-red))))
   `(diredp-link-priv ((t (:foreground ,charcoal-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,charcoal-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,charcoal-orange))))
   `(diredp-no-priv ((t (:foreground ,charcoal-fg))))
   `(diredp-number ((t (:foreground ,charcoal-green+1))))
   `(diredp-other-priv ((t (:foreground ,charcoal-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,charcoal-red-1))))
   `(diredp-read-priv ((t (:foreground ,charcoal-green-1))))
   `(diredp-symlink ((t (:foreground ,charcoal-yellow))))
   `(diredp-write-priv ((t (:foreground ,charcoal-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,charcoal-fg :background ,charcoal-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,charcoal-fg :background ,charcoal-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,charcoal-fg :background ,charcoal-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,charcoal-fg :background ,charcoal-blue-5))))
   `(ediff-even-diff-A ((t (:background ,charcoal-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,charcoal-bg+1))))
   `(ediff-even-diff-B ((t (:background ,charcoal-bg+1))))
   `(ediff-even-diff-C ((t (:background ,charcoal-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,charcoal-fg :background ,charcoal-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,charcoal-fg :background ,charcoal-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,charcoal-fg :background ,charcoal-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,charcoal-fg :background ,charcoal-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,charcoal-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,charcoal-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,charcoal-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,charcoal-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,charcoal-fg))))
   `(egg-help-header-1 ((t (:foreground ,charcoal-yellow))))
   `(egg-help-header-2 ((t (:foreground ,charcoal-green+3))))
   `(egg-branch ((t (:foreground ,charcoal-yellow))))
   `(egg-branch-mono ((t (:foreground ,charcoal-yellow))))
   `(egg-term ((t (:foreground ,charcoal-yellow))))
   `(egg-diff-add ((t (:foreground ,charcoal-green+4))))
   `(egg-diff-del ((t (:foreground ,charcoal-red+1))))
   `(egg-diff-file-header ((t (:foreground ,charcoal-yellow-2))))
   `(egg-section-title ((t (:foreground ,charcoal-yellow))))
   `(egg-stash-mono ((t (:foreground ,charcoal-green+4))))
;;;;; elfeed
   `(elfeed-search-date-face ((t (:foreground ,charcoal-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,charcoal-green))))
   `(elfeed-search-feed-face ((t (:foreground ,charcoal-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,charcoal-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,charcoal-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,charcoal-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,charcoal-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,charcoal-green+2 :background ,charcoal-bg))))
   `(w3m-lnum-match ((t (:background ,charcoal-bg-1
                                     :foreground ,charcoal-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,charcoal-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,charcoal-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,charcoal-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,charcoal-yellow))))
   `(erc-keyword-face ((t (:foreground ,charcoal-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,charcoal-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,charcoal-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,charcoal-green))))
   `(erc-pal-face ((t (:foreground ,charcoal-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,charcoal-orange :background ,charcoal-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,charcoal-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,charcoal-green+4 :background ,charcoal-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,charcoal-red :background ,charcoal-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,charcoal-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,charcoal-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,charcoal-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,charcoal-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,charcoal-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,charcoal-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,charcoal-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,charcoal-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,charcoal-red-1) :inherit unspecified))
      (t (:foreground ,charcoal-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,charcoal-yellow) :inherit unspecified))
      (t (:foreground ,charcoal-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,charcoal-cyan) :inherit unspecified))
      (t (:foreground ,charcoal-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,charcoal-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,charcoal-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,charcoal-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,charcoal-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,charcoal-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,charcoal-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,charcoal-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,charcoal-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,charcoal-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,charcoal-orange) :inherit unspecified))
      (t (:foreground ,charcoal-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,charcoal-red) :inherit unspecified))
      (t (:foreground ,charcoal-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,charcoal-fg))))
   `(ack-file ((t (:foreground ,charcoal-blue))))
   `(ack-line ((t (:foreground ,charcoal-yellow))))
   `(ack-match ((t (:foreground ,charcoal-orange :background ,charcoal-bg-1 :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,charcoal-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,charcoal-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,charcoal-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,charcoal-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,charcoal-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,charcoal-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,charcoal-magenta :weight bold))))
;;;;; git-rebase-mode
   `(git-rebase-hash ((t (:foreground, charcoal-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,charcoal-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,charcoal-blue))))
   `(gnus-summary-high-read ((t (:foreground ,charcoal-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,charcoal-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,charcoal-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,charcoal-blue))))
   `(gnus-summary-low-read ((t (:foreground ,charcoal-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,charcoal-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,charcoal-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,charcoal-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,charcoal-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,charcoal-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,charcoal-fg))))
   `(gnus-summary-selected ((t (:foreground ,charcoal-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,charcoal-blue))))
   `(gnus-cite-10 ((t (:foreground ,charcoal-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,charcoal-yellow))))
   `(gnus-cite-2 ((t (:foreground ,charcoal-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,charcoal-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,charcoal-green+2))))
   `(gnus-cite-5 ((t (:foreground ,charcoal-green+1))))
   `(gnus-cite-6 ((t (:foreground ,charcoal-green))))
   `(gnus-cite-7 ((t (:foreground ,charcoal-red))))
   `(gnus-cite-8 ((t (:foreground ,charcoal-red-1))))
   `(gnus-cite-9 ((t (:foreground ,charcoal-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,charcoal-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,charcoal-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,charcoal-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,charcoal-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,charcoal-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,charcoal-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,charcoal-bg+2))))
   `(gnus-signature ((t (:foreground ,charcoal-yellow))))
   `(gnus-x ((t (:background ,charcoal-fg :foreground ,charcoal-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,charcoal-blue))))
   `(guide-key/key-face ((t (:foreground ,charcoal-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,charcoal-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,charcoal-green
                      :background ,charcoal-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,charcoal-yellow
                      :background ,charcoal-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,charcoal-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,charcoal-bg+1))))
   `(helm-visible-mark ((t (:foreground ,charcoal-bg :background ,charcoal-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,charcoal-green+4 :background ,charcoal-bg-1))))
   `(helm-separator ((t (:foreground ,charcoal-red :background ,charcoal-bg))))
   `(helm-time-zone-current ((t (:foreground ,charcoal-green+2 :background ,charcoal-bg))))
   `(helm-time-zone-home ((t (:foreground ,charcoal-red :background ,charcoal-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,charcoal-orange :background ,charcoal-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,charcoal-magenta :background ,charcoal-bg))))
   `(helm-bookmark-info ((t (:foreground ,charcoal-green+2 :background ,charcoal-bg))))
   `(helm-bookmark-man ((t (:foreground ,charcoal-yellow :background ,charcoal-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,charcoal-magenta :background ,charcoal-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,charcoal-red :background ,charcoal-bg))))
   `(helm-buffer-process ((t (:foreground ,charcoal-cyan :background ,charcoal-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,charcoal-fg :background ,charcoal-bg))))
   `(helm-buffer-size ((t (:foreground ,charcoal-fg-1 :background ,charcoal-bg))))
   `(helm-ff-directory ((t (:foreground ,charcoal-cyan :background ,charcoal-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,charcoal-fg :background ,charcoal-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,charcoal-green+2 :background ,charcoal-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,charcoal-red :background ,charcoal-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,charcoal-yellow :background ,charcoal-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,charcoal-bg :background ,charcoal-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,charcoal-cyan :background ,charcoal-bg))))
   `(helm-grep-file ((t (:foreground ,charcoal-fg :background ,charcoal-bg))))
   `(helm-grep-finish ((t (:foreground ,charcoal-green+2 :background ,charcoal-bg))))
   `(helm-grep-lineno ((t (:foreground ,charcoal-fg-1 :background ,charcoal-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,charcoal-red :background ,charcoal-bg))))
   `(helm-moccur-buffer ((t (:foreground ,charcoal-cyan :background ,charcoal-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,charcoal-fg-1 :background ,charcoal-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,charcoal-fg :background ,charcoal-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,charcoal-fg :background ,charcoal-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,charcoal-yellow :background ,charcoal-bg+2 :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,charcoal-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,charcoal-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,charcoal-bg+1))
                   (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,charcoal-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,charcoal-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,charcoal-yellow))))
   `(ido-indicator ((t (:foreground ,charcoal-yellow :background ,charcoal-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,charcoal-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,charcoal-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,charcoal-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,charcoal-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,charcoal-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,charcoal-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,charcoal-red+1))))
   `(jabber-activity-face((t (:foreground ,charcoal-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,charcoal-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,charcoal-orange))))
   `(js2-error ((t (:foreground ,charcoal-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,charcoal-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,charcoal-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,charcoal-green+3))))
   `(js2-function-param ((t (:foreground, charcoal-green+3))))
   `(js2-external-variable ((t (:foreground ,charcoal-orange))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,charcoal-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,charcoal-fg :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,charcoal-bg+1))))
   `(ledger-font-pending-face ((t (:foreground ,charcoal-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,charcoal-fg))))
   `(ledger-font-posting-account-face ((t (:foreground ,charcoal-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,charcoal-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,charcoal-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,charcoal-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,charcoal-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,charcoal-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,charcoal-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,charcoal-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,charcoal-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,charcoal-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,charcoal-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,charcoal-green+2 :background ,charcoal-bg))))
;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,charcoal-blue-1))))
   `(lui-hilight-face ((t (:foreground ,charcoal-green+2 :background ,charcoal-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,charcoal-green+2 :background ,charcoal-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,charcoal-red+1 :background ,charcoal-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,charcoal-blue+1 :background ,charcoal-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,charcoal-magenta :background ,charcoal-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,charcoal-yellow :background ,charcoal-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-item-highlight ((t (:background ,charcoal-bg+05))))
   `(magit-section-title ((t (:foreground ,charcoal-yellow :weight bold))))
   `(magit-process-ok ((t (:foreground ,charcoal-green :weight bold))))
   `(magit-process-ng ((t (:foreground ,charcoal-red :weight bold))))
   `(magit-branch ((t (:foreground ,charcoal-blue :weight bold))))
   `(magit-log-author ((t (:foreground ,charcoal-orange))))
   `(magit-log-sha1 ((t (:foreground, charcoal-orange))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,charcoal-green+1))))
   `(message-header-other ((t (:foreground ,charcoal-green))))
   `(message-header-to ((t (:foreground ,charcoal-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,charcoal-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,charcoal-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,charcoal-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,charcoal-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,charcoal-green))))
   `(message-mml ((t (:foreground ,charcoal-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,charcoal-orange))))
   `(mew-face-header-from ((t (:foreground ,charcoal-yellow))))
   `(mew-face-header-date ((t (:foreground ,charcoal-green))))
   `(mew-face-header-to ((t (:foreground ,charcoal-red))))
   `(mew-face-header-key ((t (:foreground ,charcoal-green))))
   `(mew-face-header-private ((t (:foreground ,charcoal-green))))
   `(mew-face-header-important ((t (:foreground ,charcoal-blue))))
   `(mew-face-header-marginal ((t (:foreground ,charcoal-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,charcoal-red))))
   `(mew-face-header-xmew ((t (:foreground ,charcoal-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,charcoal-red))))
   `(mew-face-body-url ((t (:foreground ,charcoal-orange))))
   `(mew-face-body-comment ((t (:foreground ,charcoal-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,charcoal-green))))
   `(mew-face-body-cite2 ((t (:foreground ,charcoal-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,charcoal-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,charcoal-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,charcoal-red))))
   `(mew-face-mark-review ((t (:foreground ,charcoal-blue))))
   `(mew-face-mark-escape ((t (:foreground ,charcoal-green))))
   `(mew-face-mark-delete ((t (:foreground ,charcoal-red))))
   `(mew-face-mark-unlink ((t (:foreground ,charcoal-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,charcoal-green))))
   `(mew-face-mark-unread ((t (:foreground ,charcoal-red-2))))
   `(mew-face-eof-message ((t (:foreground ,charcoal-green))))
   `(mew-face-eof-part ((t (:foreground ,charcoal-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,charcoal-cyan :background ,charcoal-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,charcoal-bg :background ,charcoal-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,charcoal-bg :background ,charcoal-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,charcoal-blue))))
   `(mingus-pausing-face ((t (:foreground ,charcoal-magenta))))
   `(mingus-playing-face ((t (:foreground ,charcoal-cyan))))
   `(mingus-playlist-face ((t (:foreground ,charcoal-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,charcoal-yellow))))
   `(mingus-stopped-face ((t (:foreground ,charcoal-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,charcoal-yellow))))
   `(nav-face-button-num ((t (:foreground ,charcoal-cyan))))
   `(nav-face-dir ((t (:foreground ,charcoal-green))))
   `(nav-face-hdir ((t (:foreground ,charcoal-red))))
   `(nav-face-file ((t (:foreground ,charcoal-fg))))
   `(nav-face-hfile ((t (:foreground ,charcoal-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,charcoal-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,charcoal-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,charcoal-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,charcoal-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,charcoal-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,charcoal-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,charcoal-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,charcoal-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,charcoal-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,charcoal-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,charcoal-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,charcoal-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,charcoal-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,charcoal-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,charcoal-fg :weight bold))))
   `(org-checkbox ((t (:background ,charcoal-bg+2 :foreground ,charcoal-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,charcoal-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,charcoal-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,charcoal-green+3))))
   `(org-formula ((t (:foreground ,charcoal-yellow-2))))
   `(org-headline-done ((t (:foreground ,charcoal-green+3))))
   `(org-hide ((t (:foreground ,charcoal-bg-1))))
   `(org-level-1 ((t (:foreground ,charcoal-orange))))
   `(org-level-2 ((t (:foreground ,charcoal-green+4))))
   `(org-level-3 ((t (:foreground ,charcoal-blue-1))))
   `(org-level-4 ((t (:foreground ,charcoal-yellow-2))))
   `(org-level-5 ((t (:foreground ,charcoal-cyan))))
   `(org-level-6 ((t (:foreground ,charcoal-green+2))))
   `(org-level-7 ((t (:foreground ,charcoal-red-4))))
   `(org-level-8 ((t (:foreground ,charcoal-blue-4))))
   `(org-link ((t (:foreground ,charcoal-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,charcoal-green+4))))
   `(org-scheduled-previously ((t (:foreground ,charcoal-red))))
   `(org-scheduled-today ((t (:foreground ,charcoal-blue+1))))
   `(org-sexp-date ((t (:foreground ,charcoal-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,charcoal-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,charcoal-orange))))
   `(org-todo ((t (:bold t :foreground ,charcoal-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,charcoal-red :weight bold :underline nil))))
   `(org-column ((t (:background ,charcoal-bg-1))))
   `(org-column-title ((t (:background ,charcoal-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,charcoal-fg :background ,charcoal-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,charcoal-bg :background ,charcoal-red-1))))
   `(org-ellipsis ((t (:foreground ,charcoal-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,charcoal-cyan :underline t))))
;;;;; outline
   `(outline-1 ((t (:foreground ,charcoal-orange))))
   `(outline-2 ((t (:foreground ,charcoal-green+4))))
   `(outline-3 ((t (:foreground ,charcoal-blue-1))))
   `(outline-4 ((t (:foreground ,charcoal-yellow-2))))
   `(outline-5 ((t (:foreground ,charcoal-cyan))))
   `(outline-6 ((t (:foreground ,charcoal-green+2))))
   `(outline-7 ((t (:foreground ,charcoal-red-4))))
   `(outline-8 ((t (:foreground ,charcoal-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,charcoal-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,charcoal-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,charcoal-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,charcoal-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,charcoal-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,charcoal-fg :background ,charcoal-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,charcoal-bg :background ,charcoal-orange))))
   `(proof-error-face ((t (:foreground ,charcoal-fg :background ,charcoal-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,charcoal-bg :background ,charcoal-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,charcoal-bg :background ,charcoal-orange))))
   `(proof-locked-face ((t (:background ,charcoal-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,charcoal-bg :background ,charcoal-orange))))
   `(proof-queue-face ((t (:background ,charcoal-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,charcoal-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,charcoal-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,charcoal-bg))))
   `(proof-warning-face ((t (:foreground ,charcoal-bg :background ,charcoal-yellow-1))))
;;;;; rainbow-delimiters
   '(rainbow-delimiters-depth-1-face ((t (:foreground "orange red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "plum"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "lawn green"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "white"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "seagreen1"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black"))))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,charcoal-blue))))
   `(rcirc-other-nick ((t (:foreground ,charcoal-orange))))
   `(rcirc-bright-nick ((t (:foreground ,charcoal-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,charcoal-blue-2))))
   `(rcirc-server ((t (:foreground ,charcoal-green))))
   `(rcirc-server-prefix ((t (:foreground ,charcoal-green+1))))
   `(rcirc-timestamp ((t (:foreground ,charcoal-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,charcoal-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,charcoal-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,charcoal-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,charcoal-green))))
   `(rpm-spec-doc-face ((t (:foreground ,charcoal-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,charcoal-red))))
   `(rpm-spec-macro-face ((t (:foreground ,charcoal-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,charcoal-red))))
   `(rpm-spec-package-face ((t (:foreground ,charcoal-red))))
   `(rpm-spec-section-face ((t (:foreground ,charcoal-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,charcoal-blue))))
   `(rpm-spec-var-face ((t (:foreground ,charcoal-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,charcoal-orange))))
   `(rst-level-2-face ((t (:foreground ,charcoal-green+1))))
   `(rst-level-3-face ((t (:foreground ,charcoal-blue-1))))
   `(rst-level-4-face ((t (:foreground ,charcoal-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,charcoal-cyan))))
   `(rst-level-6-face ((t (:foreground ,charcoal-green-1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,charcoal-yellow :bold t))))
   `(sh-quoted-exec ((t (:foreground ,charcoal-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,charcoal-red+1 :background ,charcoal-bg+3 :weight bold))))
   `(show-paren-match ((t (:background ,charcoal-bg+3 :weight bold))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,charcoal-red+1 :background ,charcoal-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,charcoal-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,charcoal-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,charcoal-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,charcoal-red)))
      (t
       (:underline ,charcoal-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,charcoal-orange)))
      (t
       (:underline ,charcoal-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,charcoal-yellow)))
      (t
       (:underline ,charcoal-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,charcoal-green)))
      (t
       (:underline ,charcoal-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,charcoal-green+2))))
   `(speedbar-directory-face ((t (:foreground ,charcoal-cyan))))
   `(speedbar-file-face ((t (:foreground ,charcoal-fg))))
   `(speedbar-highlight-face ((t (:foreground ,charcoal-bg :background ,charcoal-green+2))))
   `(speedbar-selected-face ((t (:foreground ,charcoal-red))))
   `(speedbar-separator-face ((t (:foreground ,charcoal-bg :background ,charcoal-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,charcoal-yellow))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,charcoal-fg
                                    :background ,charcoal-bg))))
   `(tabbar-selected ((t (:foreground ,charcoal-fg
                                      :background ,charcoal-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,charcoal-fg
                                        :background ,charcoal-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,charcoal-bg
                                       :background ,charcoal-bg-1))))
   `(term-color-red ((t (:foreground ,charcoal-red-2
                                     :background ,charcoal-red-4))))
   `(term-color-green ((t (:foreground ,charcoal-green
                                       :background ,charcoal-green+2))))
   `(term-color-yellow ((t (:foreground ,charcoal-orange
                                        :background ,charcoal-yellow))))
   `(term-color-blue ((t (:foreground ,charcoal-blue-1
                                      :background ,charcoal-blue-4))))
   `(term-color-magenta ((t (:foreground ,charcoal-magenta
                                         :background ,charcoal-red))))
   `(term-color-cyan ((t (:foreground ,charcoal-cyan
                                      :background ,charcoal-blue))))
   `(term-color-white ((t (:foreground ,charcoal-fg
                                       :background ,charcoal-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,charcoal-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,charcoal-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,charcoal-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,charcoal-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,charcoal-cyan))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,charcoal-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,charcoal-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,charcoal-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,charcoal-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,charcoal-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,charcoal-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,charcoal-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,charcoal-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,charcoal-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,charcoal-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,charcoal-bg+1 :foreground ,charcoal-bg+1))))
   `(whitespace-hspace ((t (:background ,charcoal-bg+1 :foreground ,charcoal-bg+1))))
   `(whitespace-tab ((t (:background ,charcoal-red-1))))
   `(whitespace-newline ((t (:foreground ,charcoal-bg+1))))
   `(whitespace-trailing ((t (:background ,charcoal-red))))
   `(whitespace-line ((t (:background ,charcoal-bg :foreground ,charcoal-magenta))))
   `(whitespace-space-before-tab ((t (:background ,charcoal-orange :foreground ,charcoal-orange))))
   `(whitespace-indentation ((t (:background ,charcoal-yellow :foreground ,charcoal-red))))
   `(whitespace-empty ((t (:background ,charcoal-yellow))))
   `(whitespace-space-after-tab ((t (:background ,charcoal-yellow :foreground ,charcoal-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,charcoal-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,charcoal-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,charcoal-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,charcoal-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,charcoal-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,charcoal-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,charcoal-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,charcoal-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,charcoal-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,charcoal-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,charcoal-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,charcoal-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,charcoal-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,charcoal-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,charcoal-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,charcoal-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,charcoal-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,charcoal-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,charcoal-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,charcoal-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,charcoal-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,charcoal-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,charcoal-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,charcoal-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,charcoal-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,charcoal-green+4))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,charcoal-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,charcoal-bg-1 :foreground ,charcoal-bg-1))))
   ))

;;; Theme Variables
(charcoal-with-color-variables
 (custom-theme-set-variables
  'charcoal
;;;;; ansi-color
  `(ansi-color-names-vector [,charcoal-bg ,charcoal-red ,charcoal-green ,charcoal-yellow
                                          ,charcoal-blue ,charcoal-magenta ,charcoal-cyan ,charcoal-fg])
;;;;; fill-column-indicator
  `(fci-rule-color ,charcoal-bg-05)
;;;;; vc-annotate
  `(vc-annotate-color-map
    '(( 20. . ,charcoal-red-1)
      ( 40. . ,charcoal-red)
      ( 60. . ,charcoal-orange)
      ( 80. . ,charcoal-yellow-2)
      (100. . ,charcoal-yellow-1)
      (120. . ,charcoal-yellow)
      (140. . ,charcoal-green-1)
      (160. . ,charcoal-green)
      (180. . ,charcoal-green+1)
      (200. . ,charcoal-green+2)
      (220. . ,charcoal-green+3)
      (240. . ,charcoal-green+4)
      (260. . ,charcoal-cyan)
      (280. . ,charcoal-blue-2)
      (300. . ,charcoal-blue-1)
      (320. . ,charcoal-blue)
      (340. . ,charcoal-blue+1)
      (360. . ,charcoal-magenta)))
  `(vc-annotate-very-old-color ,charcoal-magenta)
  `(vc-annotate-background ,charcoal-bg-1)
  ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar charcoal-add-font-lock-keywords nil
  "Whether to add font-lock keywords for charcoal color names.
In buffers visiting library `charcoal-theme.el' the charcoal
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar charcoal-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after charcoal activate)
;;   "Maybe also add font-lock keywords for charcoal colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or charcoal-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "charcoal-theme.el")))
;;     (unless charcoal-colors-font-lock-keywords
;;       (setq charcoal-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car charcoal-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc charcoal-colors-alist))))))
;;     (font-lock-add-keywords nil charcoal-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after charcoal activate)
;;   "Also remove font-lock keywords for charcoal colors."
;;   (font-lock-remove-keywords nil charcoal-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'charcoal)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; charcoal-theme.el ends here