;; danneskjold-theme.el --- beautiful high-contrast theme

;; Copyright (c) 2016 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; URL: https://github.com/rails-to-cosmos/
;; Package-Version: 20160311.458

;;; Commentary:

;;; Code:

(deftheme danneskjold
  "Amazing. Beautiful. Contrast.")

(defcustom doom-enable-bold t
  "If nil, bold will remove removed from all faces."
  :group 'doom-themes
  :type 'boolean)

(defcustom doom-enable-italic t
  "If nil, italics will remove removed from all faces."
  :group 'doom-themes
  :type 'boolean)

(let ((c '((class color) (min-colors 89)))
      (bg             "#000000")
      (bg-l           "#222425")
      (fg             "#ffffff")
      (subtle         "#aab6c7")
      (vsubtle        "#556172")
      (bold   doom-enable-bold)
      (italic doom-enable-italic)

      ;; doom-molokai-colors
      ;; https://github.com/hlissner/emacs-doom-theme/blob/master/doom-molokai-theme.el
      (black          "#000000")
      (grey           "#C0C5CF")
      (grey-.5        "#828284")
      (grey-1         "#525254")
      (grey-2         "#39393D")
      (white          "#FFFFFF")
      (yellow         "#E2C770")
      (orange         "#FD971F")
      (red            "#E74C3C")
      (magenta        "#F92672")
      (violet         "#9C91E4")
      (blue           "#268BD2")
      (blue+2         "#727280")
      (cyan           "#66D9EF")
      (green          "#B6E63E")
      (green-3        "#86B20E")
      (dark-cyan      "#8FA1B3")


      ;; danneskjold-colors
      (frost          "#D0E1F9" )
      (comment        "#8593AE")
      (anthracite     "#3a3f4b")
      (sbt-midnight   "#282c34")
      (ada-midnight   "#21252b")
      (sunrise        "#FFDB45")
      (saffron        "#F9BA32")
      (orange-sat     "#FF9009")
      (spring-flower  "#B3DE81")
      (summer-flower  "#013220")
      (twitter        "#4CB5F5")
      (diredcl        "#74749A9AF7F7")
      (dvi            "DarkViolet")
      (waddles        "#FF87BA")
      (krayola        "#E38B75")
      (santa          "#F34A4A")
      (red-forest     "#8b0000"))

  (let* ((search-bg      green)
         (search-fg      black)
         (search-rest-bg violet)
         (search-rest-fg black)
         (highlight      orange)
         (vertical-bar   grey-2)
         (current-line   "#1F1F1F")
         (selection      "#535556")
         (builtin        orange)
         (comments       grey-1)
         (constants      green)
         (delimiters     "#c0c5ce")
         (functions      cyan)
         (keywords       magenta)
         (methods        dark-cyan)
         (operators      violet)
         (type           cyan)
         (strings        green)
         (variables      orange)

         (error-highlight red)

         (linum-bg       current-line)
         (linum-fg       "#3F3F48")
         (linum-hl-fg    orange)
         (linum-hl-bg    current-line)

         (active-minibuffer "#404046")
         (modeline-fg    white)
         (modeline-fg-2  orange)
         (modeline-fg-3  orange)
         (modeline-fg-inactive  "#80858F")
         (modeline-bg    grey-2)
         (modeline-bg-2  grey-2)
         (modeline-bg-3  grey-2)
         (modeline-bg-inactive  current-line)

         (vc-modified    grey-2)
         (vc-added       green-3)
         (vc-deleted     red))
    (custom-theme-set-faces
     'danneskjold
     `(default ((t (:foreground ,fg :background ,bg))))
     `(fringe ((t (:background ,bg))))
     `(region ((t (:background ,anthracite))))
     `(button ((t (:foreground ,frost :underline t :weight normal))))
     `(link ((t (:foreground ,frost :underline t))))
     `(menu ((t (:foreground ,fg :background ,ada-midnight))))

     `(show-paren-match ((t (:background ,twitter :foreground ,white))))

     `(font-lock-string-face ((t (:foreground ,spring-flower))))
     `(font-lock-doc-face ((t (:foreground ,spring-flower))))
     `(font-lock-builtin-face ((t (:foreground ,twitter))))
     `(font-lock-variable-name-face ((t (:foreground ,fg))))
     `(font-lock-keyword-face ((t (:foreground ,frost))))
     `(font-lock-comment-face ((t (:foreground ,comment))))
     `(font-lock-comment-delimiter-face ((t (:foreground ,anthracite))))
     `(font-lock-function-name-face ((t (:foreground ,saffron))))
     `(font-lock-type-face ((t (:foreground ,orange-sat))))
     `(font-lock-constant-face ((t (:foreground ,sunrise))))

     `(mmm-default-submode-face ((t (:background ,ada-midnight))))

     `(header-line ((t (:background ,bg
                                    :foreground ,comment
                                    :underline ,comment
                                    :weight normal))))

     ;; Mode-line
     `(mode-line ((t (:background ,ada-midnight
                                  :foreground ,comment
                                  :box ,(list
                                         :line-width 4
                                         :color ada-midnight)))))
     `(mode-line-inactive ((t (:background ,sbt-midnight
                                           :foreground ,comment
                                           :box ,(list
                                                  :line-width 4
                                                  :color sbt-midnight)))))
     `(mode-line-buffer-id ((t (:foreground ,frost))))

     `(font-lock-warning-face ((t (:foreground ,santa))))
     `(compilation-error ((t (:background ,red-forest))))
     `(compilation-warning ((t (:foreground ,santa))))
     `(compilation-info ((t (:foreground ,spring-flower))))
     `(highlight ((t (:background ,ada-midnight :foreground ,frost))))

     ;; Linum
     `(linum ((t (:foreground ,anthracite))))

     `(widget-field ((t (:foreground ,fg :background ,sbt-midnight))))
     `(widget-button ((t (:foreground ,saffron))))

     ;; Highlight quoted mode-line
     `(highlight-quoted-symbol ((t (:foreground ,waddles))))

     ;; Hl-line and hlinum-activate
     `(linum-highlight-face ((t (:foreground ,anthracite :background ,ada-midnight :weight bold))))
     `(hl-line ((t (:background ,ada-midnight))))

     ;; Diff
     `(diff-header ((t (:foreground ,sunrise))))
     `(diff-file-header ((t (:foreground ,sunrise))))
     `(diff-indicator-removed ((t (:foreground ,bg))))
     `(diff-removed ((t (:foreground ,santa))))
     `(diff-added ((t (:foreground ,spring-flower))))
     `(diff-indicator-added ((t (:foreground ,bg))))
     `(diff-refine-removed ((t (:foreground ,red-forest))))
     `(diff-refine-added ((t (:foreground ,summer-flower))))

     `(diff-context ((t (:foreground ,comment))))

     ;; Magit
     `(magit-diff-added ((t (:foreground ,spring-flower))))
     `(magit-diff-added-highlight ((t (:foreground ,spring-flower))))
     `(magit-diff-removed ((t (:foreground ,santa))))
     `(magit-diff-removed-highlight ((t (:foreground ,santa))))
     `(magit-diff-context ((t (:background ,bg :foreground ,frost))))
     `(magit-diff-context-highlight ((t (:background ,bg :foreground ,frost))))
     `(magit-section-highlight ((t (:background ,bg))))
     `(magit-section-heading ((t (:foreground ,sunrise :inherit nil))))
     `(magit-diff-hunk-heading ((t (:foreground ,twitter :background ,sbt-midnight))))
     `(magit-diff-hunk-heading-highlight ((t (:foreground ,twitter :background ,sbt-midnight))))
     `(magit-diff-lines-heading ((t (:foreground ,frost :background ,sbt-midnight))))
     `(magit-blame-heading ((t (:foreground ,twitter :background ,sbt-midnight))))

     ;; Org-mode
     `(org-tag                      ((,c (:foreground ,yellow :bold nil))))
     `(org-ellipsis                 ((,c (:inherit hs-face))))
     `(org-hide                     ((,c (:foreground ,bg))))
     `(org-table                    ((,c (:foreground ,twitter))))
     `(org-quote                    ((,c (:slant italic :foreground ,grey :background ,current-line))))
     `(org-document-info            ((,c (:foreground ,orange))))
     `(org-document-info-keyword    ((,c (:foreground ,grey-1))))
     `(org-meta-line                ((,c (:foreground ,vsubtle))))
     `(org-block-begin-line         ((,c (:foreground ,vsubtle))))
     `(org-block-end-line           ((,c (:inherit org-block-begin-line))))
     `(org-block-background         ((,c (:background ,current-line))))
     `(org-archived                 ((,c (:foreground ,grey-.5))))
     `(org-document-title           ((,c (:foreground ,twitter))))
     `(org-level-1                  ((,c (:foreground ,santa))))
     `(org-level-2                  ((,c (:foreground ,cyan))))
     `(org-level-3                  ((,c (:foreground ,violet))))
     `(org-level-4                  ((,c (:foreground ,spring-flower))))
     `(org-level-5                  ((,c (:foreground ,yellow))))
     `(org-level-6                  ((,c (:foreground ,waddles))))
     `(org-code                     ((,c (:foreground ,orange))))
     `(org-column                   ((,c (:background ,bg))))
     `(org-column-title             ((,c (:background ,bg :foreground ,comment))))
     `(org-verbatim                 ((,c (:foreground ,green))))
     `(org-formula                  ((,c (:foreground ,yellow))))
     `(org-list-dt                  ((,c (:foreground ,twitter))))
     `(org-footnote                 ((,c (:foreground ,orange))))
     `(org-link                     ((,c (:foreground ,frost :underline t))))
     `(org-date                     ((,c (:foreground ,violet))))
     `(org-todo                     ((,c (:foreground ,yellow))))
     `(org-done                     ((,c (:foreground ,spring-flower))))
     `(org-headline-done            ((,c (:foreground ,grey-.5 :strike-through t :bold nil))))
     `(org-special-keyword          ((,c (:foreground ,orange-sat))))
     `(org-checkbox-statistics-todo ((,c (:inherit org-todo))))
     `(org-checkbox-statistics-done ((,c (:inherit org-done))))

     ;; Hi
     `(hi-green-b ((t (:foreground ,spring-flower))))
     `(hi-yellow-b ((t (:foreground ,sunrise))))
     `(hi-yellow ((t (:foreground ,orange-sat))))
     `(hi-red-b ((t (:foreground ,santa))))

     ;; Perspeen
     `(perspeen-selected-face ((t (:foreground ,frost))))

     ;; Powerline
     `(powerline-active1 ((t (:foreground ,sunrise))))
     `(powerline-active2 ((t (:foreground ,comment))))
     `(powerline-inactive1 ((t (:foreground ,comment))))
     `(powerline-inactive2 ((t (:foreground ,comment))))

     ;; Prodigy
     `(prodigy-red-face ((t (:foreground ,santa))))
     `(prodigy-green-face ((t (:foreground ,spring-flower))))
     `(prodigy-yellow-face ((t (:foreground ,sunrise))))

     ;; Jabber
     `(jabber-title-large ((t (:foreground ,sunrise))))
     `(jabber-title-medium ((t (:foreground ,sunrise))))
     `(jabber-title-small ((t (:foreground ,sunrise))))
     `(jabber-chat-prompt-local ((t (:foreground ,frost))))
     `(jabber-chat-prompt-foreign ((t(:foreground ,sunrise))))
     `(jabber-roster-user-xa ((t (:foreground ,saffron))))
     `(jabber-roster-user-online ((t (:foreground ,spring-flower))))
     `(jabber-roster-user-offline ((t (:foreground ,comment))))
     `(jabber-roster-user-away ((t (:foreground ,frost))))
     `(jabber-rare-time-face ((t (:foreground ,sbt-midnight))))

     ;; SQL*Plus
     `(sqlplus-table-head-face ((t (:foreground ,sunrise))))
     `(sqlplus-table-even-rows-face ((t (:foreground ,fg :background ,ada-midnight))))
     `(sqlplus-table-odd-rows-face ((t (:foreground ,fg))))

     ;; Rainbow-delimiters
     `(rainbow-delimiters-depth-1-face ((t (:foreground ,spring-flower))))
     `(rainbow-delimiters-depth-2-face ((t (:foreground ,santa))))
     `(rainbow-delimiters-depth-3-face ((t (:foreground ,frost))))
     `(rainbow-delimiters-depth-4-face ((t (:foreground ,saffron))))
     `(rainbow-delimiters-depth-5-face ((t (:foreground ,waddles))))
     `(rainbow-delimiters-depth-6-face ((t (:foreground ,frost))))
     `(rainbow-delimiters-depth-7-face ((t (:foreground ,spring-flower))))
     `(rainbow-delimiters-depth-8-face ((t (:foreground ,santa))))
     `(rainbow-delimiters-depth-9-face ((t (:foreground ,frost))))
     `(rainbow-delimiters-depth-10-face ((t (:foreground ,saffron))))

     ;; Company-mode
     `(company-tooltip ((t (:foreground ,fg :background ,ada-midnight))))
     `(company-tooltip-selection ((t (:foreground ,fg :background ,sbt-midnight))))
     `(company-scrollbar-fg ((t (:background ,ada-midnight))))
     `(company-scrollbar-bg ((t (:background ,sbt-midnight))))
     `(company-tooltip-common ((t (:foreground ,saffron))))
     `(company-preview ((t (:background ,sbt-midnight))))
     `(company-preview-common ((t (:background ,sbt-midnight :foreground ,santa))))
     `(company-mouse ((t (:background ,ada-midnight))))

     ;; Elfeed
     `(elfeed-search-feed-face ((t (:foreground ,comment))))
     `(elfeed-search-tag-face ((t (:foreground ,spring-flower))))
     `(elfeed-search-unread-title-face ((t (:foreground ,fg))))
     `(elfeed-search-date-face ((t (:foreground ,diredcl))))

     ;; Flycheck
     `(flycheck-warning ((t (:foreground ,santa :underline t))))

     ;; js2-mode
     `(js2-function-param ((t (:foreground ,saffron))))

     ;; message
     `(message-header-name ((t (:foreground ,comment))))
     `(message-header-subject ((t (:foreground ,fg))))
     `(message-header-to ((t (:foreground ,fg))))
     `(message-header-other ((t (:foreground ,fg))))
     `(shr-link ((t (:foreground ,frost :underline t))))

     ;; erc
     `(erc-timestamp-face ((t (:foreground ,santa))))
     `(erc-prompt-face ((t (:foreground ,spring-flower))))
     `(erc-nick-default-face ((t (:foreground ,frost))))
     `(erc-notice-face ((t (:foreground ,waddles))))
     `(erc-button ((t (:foreground ,frost))))
     `(erc-current-nick-face ((t (:foreground ,santa))))

     ;; eshell
     `(eshell-prompt ((t (:foreground ,santa))))
     `(eshell-ls-executable ((t (:foreground ,spring-flower))))
     `(eshell-ls-directory ((t (:foreground ,twitter))))
     `(eshell-ls-symlink ((t (:foreground ,waddles))))
     `(eshell-ls-readonly ((t (:foreground ,anthracite))))
     `(eshell-ls-missing ((t (:foreground ,santa))))
     `(eshell-ls-special ((t (:foreground ,comment :underline t))))

     ;; dired
     `(dired-directory ((t (:foreground ,twitter))))
     `(dired-git-face ((t (:foreground ,santa))))
     `(dired-ignored ((t (:foreground ,anthracite))))
     `(dired-filetype-omit ((t (:foreground ,anthracite))))
     `(dired-filetype-common ((t (:foreground ,saffron))))
     `(dired-filetype-execute ((t (:foreground ,spring-flower))))
     `(dired-filetype-source ((t (:foreground ,orange-sat))))
     `(dired-filetype-plain ((t (:foreground ,comment))))
     `(dired-filetype-link ((t (:foreground ,twitter :underline t))))
     `(dired-flagged ((t (:foreground ,santa :underline t))))
     `(dired-marked ((t (:foreground ,saffron :underline t))))
     `(dired-subtree-depth-1-face ((t (:background ,"#21252b"))))
     `(dired-subtree-depth-2-face ((t (:background ,"#282c34"))))
     `(dired-subtree-depth-3-face ((t (:background ,bg))))

   ;;; dired+
     `(diredp-dir-heading ((t (:foreground ,santa))))
     `(diredp-dir-name ((t (:foreground ,twitter))))
     `(diredp-file-name ((t (:foreground ,fg))))
     `(diredp-file-suffix ((t (:foreground ,frost))))
     `(diredp-ignored-file-name ((t (:foreground ,comment))))
     `(diredp-symlink ((t (:foreground ,waddles))))
     `(diredp-number ((t (:foreground ,saffron))))

     `(diredp-dir-priv ((t (:foreground ,twitter))))
     `(diredp-read-priv ((t (:foreground ,santa))))
     `(diredp-write-priv ((t (:foreground ,sunrise))))
     `(diredp-exec-priv ((t (:foreground ,spring-flower))))
     `(diredp-no-priv ((t (:foreground ,fg))))
     `(diredp-rare-priv ((t (:foreground ,waddles))))
     `(diredp-flag-mark ((t (:foreground ,bg))))
     `(diredp-flag-mark-line ((t (:foreground ,bg :background ,sunrise))))
     `(diredp-mode-line-marked ((t (:foreground ,sunrise))))
     `(diredp-deletion ((t (:foreground ,bg :background ,santa))))
     `(diredp-deletion-file-name ((t (:foreground ,bg :background ,santa))))
     `(diredp-mode-line-flagged ((t (:foreground ,santa))))

     ;; ido
     `(minibuffer-prompt ((t (:foreground ,comment))))
     `(ido-first-match ((t (:foreground ,frost))))
     `(ido-only-match ((t (:foreground ,frost))))
     `(ido-subdir ((t (:foreground ,frost))))
     `(ido-vertical-match-face ((t (:foreground ,twitter))))

     ;; vertical-border
     `(vertical-border ((t (:foreground "#282a2e"))))

     ;; hackernews
     `(hackernews-score-face ((t (:foreground ,fg))))
     `(hackernews-link-face ((t (:foreground ,spring-flower))))
     `(hackernews-comment-count-face ((t (:foreground ,santa))))))

  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'danneskjold)
;;; danneskjold-theme.el ends here
