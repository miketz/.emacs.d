;;; giorgio-theme.el --- A sunny and calm Emacs theme inspired by the paintings of Giorgio di Chirico  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Dan Dee

;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; URL: https://github.com/monkeyjunglejuice/giorgio-theme-emacs
;; Package-Version:
;; Package-Commit:
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: faces, themes

;; This file is not part of GNU Emacs.

;; This program is free software. It comes without any warranty,
;; to the extent permitted by applicable law. You can redistribute it
;; and/or modify it under the terms of the MIT License.

;;; Commentary:
;; "Giorgio" is a custom theme for Emacs, inspired by the paintings
;; created by the italian artist Giorgio di Chirico between 1913 and 1914.
;; The theme resembles the bright daylight and hard, long shadows of mediterranean
;; cities and aims to be sunny and calm, free from distractions.

;;; Credits:
;; The theme is initially based on the notink-theme by MetroWind

;;; Code:

(deftheme giorgio "A sunny and calm Emacs theme, inspired by the paintings of Giorgio di Chirico.")

;; Colors
(let* ((color-fg           "#33322c")
       (color-fg-alt       "#000000")
       (color-dimmed       "#8b8875")
       (color-bg           "#fff8dc")
       (color-bg-alt       "#eee8cd")
       (color-hl           "#cff5d7")
       (color-bright       "#d46f29")
       (color-middle       "#93c6a8")
       (color-dark         "#008064")
       (color-strong       "#8e210a")
       (color-light        "#ffffff")
       (color-light-red    "#fcd5be")
       (color-light-yellow "#fff4c7"))

  (custom-theme-set-faces 'giorgio
   `(default ((t (:background ,color-bg :foreground ,color-fg))))
   `(cursor ((t (:background ,color-fg))))
   `(region ((t (:foreground ,color-dark :background ,color-hl))))
   `(success ((t (:foreground ,color-dark))))
   `(warning ((t (:underline (:color ,color-bright :style wave)))))
   `(error ((t (:foreground ,color-strong :background ,color-light-red))))
   `(secondary-selection ((t (:background ,color-bg-alt))))
   `(mode-line ((t (:background ,color-fg :foreground ,color-bg :box nil))))
   `(mode-line-buffer-id ((t (:foreground ,color-bg :weight bold))))
   `(mode-line-inactive ((t (:background ,color-dimmed :foreground ,color-bg))))
   `(fringe ((t (:background ,color-bg))))
   `(vertical-border ((t (:foreground ,color-fg-alt :background nil))))
   `(minibuffer-prompt ((t (:inherit comint-highlight-prompt))))

   `(font-lock-face ((t (:foreground ,color-middle))))
   `(font-lock-builtin-face ((t (:foreground ,color-strong))))
   `(font-lock-comment-face ((t (:foreground ,color-dimmed :inherit fixed-pitch-serif :slant italic))))
   `(font-lock-doc-face ((t (:foreground ,color-dark :inherit font-lock-comment-face))))
   `(font-lock-constant-face ((t (:foreground ,color-fg-alt))))
   `(font-lock-function-name-face ((t (:foreground ,color-fg :weight bold :slant italic))))
   `(font-lock-keyword-face ((t (:foreground ,color-bright))))
   `(font-lock-string-face ((t (:foreground ,color-dark :inherit fixed-pitch-serif))))
   `(font-lock-type-face ((t (:weight bold))))
   `(font-lock-variable-name-face ((t (:slant italic))))
   `(font-lock-warning-face ((t (:foreground ,color-strong :background ,color-light-red))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,color-bright))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,color-bright))))

   `(isearch ((t (:foreground ,color-bg :background ,color-bright :underline t))))
   `(isearch-fail ((t (:inherit error))))
   `(lazy-highlight ((t (:foreground ,color-fg :background ,color-hl :underline t))))

   `(link ((t (:foreground ,color-strong :underline t))))
   `(link-visited ((t (:foreground ,color-middle :underline t))))
   `(button ((t (:foreground ,color-strong :underline t))))
   `(help-face-button ((t (:inherit button))))
   `(header-line ((t (:foreground ,color-dimmed :slant italic :inherit fixed-pitch-serif :underline (:color ,color-dimmed)))))
   `(shadow ((t (:foreground ,color-dimmed))))
   `(show-paren-match ((t (:foreground ,color-fg :background ,color-hl :weight bold))))
   `(show-paren-mismatch ((t (:inherit error))))
   `(highlight ((t (:background ,color-hl :underline (:color ,color-fg-alt)))))
   `(hl-line ((t (:underline (:color ,color-dimmed) :extend t))))
   `(widget-field ((t (:foreground ,color-fg-alt :background ,color-bg-alt))))
   `(trailing-whitespace ((t (:background ,color-light-red))))
   `(escape-glyph ((t (:inverse-video t))))

   ;; Shell-mode
   `(sh-heredoc ((t (:foreground nil :inherit font-lock-string-face))))
   `(sh-quoted-exec ((t (:inherit font-lock-function-name-face))))

   ;; Dired
   `(dired-header ((t (:foreground ,color-bright :slant italic))))
   `(dired-directory ((t (:weight bold))))
   `(dired-symlink ((t (:foreground ,color-middle))))
   `(dired-marked ((t (:foreground ,color-hl))))
   `(dired-flagged ((t (:foreground ,color-strong :background ,color-light-red))))
   `(dired-perm-write ((t (:foreground ,color-strong))))
   `(dired-special ((t (:foreground ,color-middle :slant italic))))

   ;; Eshell
   `(eshell-prompt ((t (:inherit comint-highlight-prompt))))
   `(eshell-ls-directory ((t (:inherit dired-directory))))
   `(eshell-ls-archive ((t (:slant italic :inherit dired-directory))))
   `(eshell-ls-symlink ((t (:inherit dired-symlink))))
   `(eshell-ls-executable ((t (:foreground ,color-dark))))
   `(eshell-ls-missing ((t (:inherit error))))
   `(eshell-ls-readonly ((t (:inherit shadow))))
   `(eshell-ls-special ((t (:inherit dired-special))))

   ;; Comint
   `(comint-highlight-prompt ((t (:foreground ,color-bright :slant italic))))
   `(comint-highlight-input ((t (:inherit default))))

   ;; Completions
   `(completions-common-part ((t (:weight bold))))
   `(icomplete-first-match ((t (:background ,color-hl :weight bold))))

   ;; Diff
   `(diff-added ((t (:foreground ,color-dark :background ,color-hl))))
   `(diff-removed ((t (:foreground ,color-strong :background ,color-light-red))))
   `(diff-context ((t (:inherit shadow :background ,color-bg-alt))))
   `(diff-file-header ((t (:bold t :background ,color-light :weight bold))))
   `(diff-header ((t (:background ,color-light :foreground ,color-fg))))

   ;; Package manager
   `(package-description ((t (:inherit font-lock-doc-face))))

   ;; Customization
   `(custom-group-tag ((t (:inherit bold))))
   `(custom-variable-tag ((t (:weight bold))))
   `(custom-variable-obsolete ((t (:foreground ,color-dimmed :inherit custom-variable-tag))))
   `(custom-documentation ((t (:slant italic :inherit fixed-pitch-serif))))
   `(custom-visibility ((t (:inherit custom-documentation :underline t))))
   `(custom-state ((t (:foreground ,color-dark :inherit fixed-pitch-serif))))
   `(custom-button ((t (:inherit button))))
   `(custom-button-unraised ((t (:inherit button))))

   ;; Info
   `(info-menu-star ((t (:foreground ,color-bright))))

   ;; Message
   `(message-header-name ((t (:foreground ,color-dark))))
   `(message-header-other ((t (:foreground ,color-bright))))
   `(message-header-cc ((t (:inherit message-header-other))))
   `(message-header-newsgroups ((t (:inherit message-header-other))))
   `(message-header-xheader ((t (:inherit message-header-other))))
   `(message-header-subject ((t (:inherit default))))
   `(message-header-to ((t (:foreground ,color-dark))))
   `(message-cited-text ((t (:foreground ,color-bright :inherit italic))))
   `(message-mml ((t (:foreground ,color-bright))))

   ;; ERC
   `(erc-notice-face ((t (:foreground ,color-dark :weight unspecified))))
   `(erc-header-line ((t (:inherit header-line))))
   `(erc-timestamp-face ((t (:foreground ,color-bright :weight unspecified))))
   `(erc-current-nick-face ((t (:background ,color-dark :foreground ,color-bg :weight unspecified))))
   `(erc-input-face ((t (:foreground ,color-dark))))
   `(erc-prompt-face ((t (:foreground ,color-bright :background nil :inherit italic :weight unspecified))))
   `(erc-my-nick-face ((t (:foreground ,color-fg))))
   `(erc-pal-face ((t (:foreground ,color-dark :inherit italic))))

   ;; TeX
   `(font-latex-sedate-face ((t (:foreground ,color-dark))))
   `(font-latex-math-face ((t (:inherit default))))
   `(font-latex-script-char-face ((t (:inherit font-latex-math-face))))

   ;; Outline
   `(outline-1 ((t (:weight bold :inherit fixed-pitch-serif :height 1.6))))
   `(outline-2 ((t (:weight bold :inherit fixed-pitch-serif :height 1.4))))
   `(outline-3 ((t (:weight bold :inherit fixed-pitch-serif :height 1.2))))
   `(outline-4 ((t (:weight bold :inherit fixed-pitch-serif))))
   `(outline-5 ((t (:weight bold :inherit fixed-pitch-serif))))
   `(outline-6 ((t (:weight bold :inherit fixed-pitch-serif))))
   `(outline-7 ((t (:weight bold :inherit fixed-pitch-serif))))
   `(outline-8 ((t (:weight bold :inherit fixed-pitch-serif))))

   ;; Org-mode
   `(org-hide ((t (:foreground ,color-bg))))
   `(org-table ((t (:foreground ,color-fg))))
   `(org-code ((t (:foreground ,color-dark))))
   `(org-date ((t (:foreground ,color-bright))))
   `(org-done ((t (:foreground ,color-middle :box t :weight normal))))
   `(org-headline-done ((t (:foreground ,color-dimmed))))
   `(org-todo ((t (:foreground ,color-strong :box t :weight normal))))
   `(org-latex-and-related ((t (:foreground ,color-dark :italic t))))
   `(org-checkbox ((t (:weight normal :foreground ,color-bright))))
   `(org-verbatim ((t (:foreground ,color-dark))))
   `(org-mode-line-clock ((t (:background nil))))
   `(org-document-title ((t (:weight normal :foreground nil))))
   `(org-drawer ((t (:inherit font-lock-comment-face))))
   `(org-block ((t (:foreground ,color-fg :background ,color-bg-alt :extend t))))
   `(org-block-begin-line ((t (:inherit font-lock-comment-face))))
   `(org-block-end-line ((t (:inherit font-lock-comment-face))))
   `(org-meta-line ((t (:inherit font-lock-comment-face))))
   `(org-document-info ((t (:foreground ,color-dark))))
   `(org-archived ((t (:foreground ,color-dimmed))))

   ;; org-tree-slide
   `(org-tree-slide-header-overlay-face ((t (:inherit font-lock-comment-face :foreground nil :background nil))))

   ;; Compilation
   `(compilation-error ((t (:inherit error))))
   `(compilation-warning ((t (:inherit warning))))
   `(compilation-info ((t (:foreground ,color-dark))))

   ;; Whitespace
   `(whitespace-trailing ((t (:background ,color-light-red))))
   `(whitespace-line ((t (:inherit whitespace-trailing))))
   `(whitespace-space (( t(:foreground ,color-middle))))
   `(whitespace-newline ((t (:inherit whitespace-space))))
   `(whitespace-empty ((t (:inherit whitespace-line))))

   ;; Smart parens
   `(sp-pair-overlay-face ((t (:background ,color-light))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,color-fg :weight light))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,color-dimmed :weight light))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,color-dimmed :weight light))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,color-dimmed :weight light))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,color-dimmed :weight light))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,color-dimmed :weight light))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,color-dimmed :weight light))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,color-dimmed :weight light))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,color-dimmed :weight light))))
   `(rainbow-delimiters-unmatched-face ((t (:inherit error))))

   ;; Paren face
   `(parenthesis ((t (:inherit shadow :weight light))))

   ;; Git-commit
   `(git-commit-summary ((t (:inherit default))))
   `(git-commit-comment-heading ((t (:slant italic :inherit font-lock-comment-face))))
   `(git-commit-comment-branch-local ((t (:slant italic :weight bold))))
   `(git-commit-comment-branch-remote ((t (:slant italic :weight bold))))
   `(git-commit-comment-file ((t (:inherit font-lock-string-face))))
   `(git-commit-comment-action ((t (:weight bold :inherit font-lock-comment-face))))

   ;; Magit
   `(magit-branch-local ((t (:foreground ,color-dark :background nil))))
   `(magit-branch-remote ((t (:foreground ,color-dark :background nil))))
   `(magit-tag ((t (:foreground ,color-dark :background nil :inherit italic))))
   `(magit-hash ((t (:foreground ,color-bright))))
   `(magit-section-title ((t (:foreground ,color-fg :background nil))))
   `(magit-section-heading ((t (:background nil :foreground ,color-fg))))
   `(magit-section-highlight ((t (:background nil))))
   `(magit-item-highlight ((t (:foreground ,color-fg :background ,color-bright))))
   `(magit-log-author ((t (:foreground ,color-dark))))
   `(magit-diff-added ((t (:inherit diff-added))))
   `(magit-diffstat-added ((t (:foreground ,color-dark))))
   `(magit-diff-added-highlight ((t (:inherit magit-diff-added))))
   `(magit-diff-removed ((t (:inherit diff-removed))))
   `(magit-diffstat-removed ((t (:foreground ,color-strong))))
   `(magit-diff-removed-highlight ((t (:inherit magit-diff-removed))))
   `(magit-diff-context ((t (:inherit diff-context))))
   `(magit-diff-context-highlight ((t (:inherit magit-diff-context))))
   `(magit-popup-argument ((t (:inherit font-lock-function-name-face))))
   `(magit-popup-disabled-argument ((t (:inherit font-lock-comment-face))))
   `(magit-process-ok ((t (:inherit success))))
   `(magit-diff-hunk-heading ((t (:background unspecified :foreground unspecified :inherit header-line))))
   `(magit-diff-hunk-heading-highlight ((t (:background unspecified :foreground unspecified :inherit magit-diff-hunk-heading))))
   `(magit-filename ((t (:inherit git-commit-comment-file))))

   ;; Git-gutter-fringe
   `(git-gutter-fr:modified ((t (:foreground ,color-dark))))
   `(git-gutter-fr:added ((t (:foreground ,color-dark))))
   `(git-gutter-fr:deleted ((t (:foreground ,color-dark))))

   ;; Company
   `(company-preview ((t (:foreground ,color-fg))))
   `(company-preview-common ((t (:foreground ,color-fg :background nil))))
   `(company-tooltip-search ((t (:inherit lazy-highlight))))
   `(company-tooltip-search-selection ((t (:inherit company-tooltip-search))))
   `(company-tooltip ((t (:foreground ,color-fg :background ,color-light))))
   `(company-tooltip-annotation ((t (:foreground ,color-dark))))
   `(company-tooltip-common ((t (:foreground ,color-middle))))
   `(company-tooltip-common-selection ((t (:foreground ,color-dark))))
   `(company-tooltip-selection ((t (:background ,color-hl))))
   `(company-scrollbar-bg ((t (:background ,color-light))))
   `(company-scrollbar-fg ((t (:background ,color-middle))))

   ;; Flymake
   `(flymake-error ((t (:inherit error))))
   `(flymake-warning ((t (:inherit warning))))
   `(flymake-note ((t (:foreground ,color-dark))))

   ;; Flycheck
   `(flycheck-error ((t (:inherit error))))
   `(flycheck-fringe-error ((t (:inherit error))))
   `(flycheck-warning ((t (:inherit warning))))
   `(flycheck-fringe-warning ((t (:foreground ,color-bright))))
   `(flycheck-info ((t (:background ,color-middle))))
   `(flycheck-fringe-info ((t (:foreground ,color-dark :background ,color-hl))))

   ;; LSP
   `(lsp-headerline-breadcrumb-path-face ((t (:foreground ,color-fg))))
   `(lsp-headerline-breadcrumb-path-error-face ((t (:inherit error))))
   `(lsp-headerline-breadcrumb-separator-face ((t (:foreground ,color-fg))))

   ;; Eglot
   `(eglot-highlight-symbol-face ((t (:inherit lazy-highlight))))

   ;; CSV
   `(csv-separator-face ((t (:foreground ,color-strong))))

   ;; CSS
   `(css-selector ((t (:weight bold))))
   `(css-property ((t (:inherit font-lock-builtin-face))))

   ;; Web-mode
   `(web-mode-html-tag-bracket-face ((t (:inherit shadow))))
   `(web-mode-html-tag-face ((t (:weight bold :inherit shadow))))
   `(web-mode-html-attr-name-face ((t (:inherit shadow :slant italic))))
   `(web-mode-css-selector-face ((t (:inherit css-selector))))
   `(web-mode-css-property-name-face ((t (:inherit css-property))))
   `(web-mode-doctype-face ((t (:inherit shadow))))
   `(web-mode-css-color-face ((t (:foreground ,color-fg))))

   ;; Slime
   `(slime-repl-inputed-output-face ((t (:foreground ,color-dark))))
   `(slime-repl-output-mouseover-face ((t (:foreground ,color-bright :box nil))))
   `(slime-repl-input-face ((t (:inherit default))))
   `(sldb-section-face ((t (:foreground ,color-dimmed :weight bold))))

   ;; Tuareg
   `(tuareg-font-lock-interactive-output-face ((t (:foreground ,color-dark))))
   `(tuareg-font-lock-interactive-error-face ((t (:inherit error))))
   `(tuareg-font-lock-interactive-directive-face ((t (:foreground ,color-middle))))
   `(tuareg-font-lock-operator-face ((t (:foreground ,color-fg-alt))))
   `(tuareg-font-lock-module-face ((t (:inherit shadow))))
   `(tuareg-font-lock-governing-face ((t (:foreground ,color-bright :weight bold))))
   `(tuareg-font-lock-label-face ((t (:inherit font-lock-builtin-face))))
   `(tuareg-font-lock-line-number-face ((t (:inherit linum))))
   `(tuareg-font-double-colon-face ((t (:inherit tuareg-font-lock-governing-face))))
   `(tuareg-font-lock-error-face ((t (:inherit error))))

   ;; Merlin
   `(merlin-compilation-error-face ((t (:inherit error))))
   `(merlin-type-face ((t (:background ,color-hl))))

   ;; Merlin-eldoc
   `(merlin-eldoc-occurrences-face ((t (:inherit lazy-highlight))))

   ;; Utop
   `(utop-frozen ((t (:inherit default))))
   `(utop-prompt ((t (:inherit comint-highlight-prompt))))
   `(utop-error  ((t (:inherit error))))

   ;; Selectrum
   `(selectrum-mouse-highlight ((t (:background nil :underline t :extend t))))
   `(selectrum-prescient-primary-highlight ((t (:inherit completions-common-part))))

   ;; Marginalia
   `(marginalia-archive ((t (:inherit nil))))
   `(marginalia-key ((t (:inherit nil))))
   `(marginalia-number ((t (:inherit nil))))
   `(marginalia-file-priv-dir ((t (:weight bold))))
   `(marginalia-file-priv-read ((t (:foreground ,color-fg))))
   `(marginalia-file-priv-write ((t (:foreground ,color-strong))))
   `(marginalia-file-priv-exec ((t (:foreground ,color-dark))))

   ;; Consult
   `(consult-preview-line ((t (:inherit highlight))))
   `(consult-preview-cursor ((t (:background ,color-bg :underline nil))))

   ;; Helm
   `(helm-candidate-number ((t (:foreground ,color-dimmed :background nil))))
   `(helm-source-header ((t (:inherit font-lock-comment-face :background unspecified :foreground unspecified))))
   `(helm-selection ((t (:inherit highlight))))
   `(helm-prefarg ((t (:foreground ,color-dark))))
   `(helm-ff-file ((t (:inherit default))))
   `(helm-ff-directory ((t (:inherit dired-directory :foreground unspecified))))
   `(helm-ff-executable ((t (:inherit eshell-ls-executable :foreground unspecified))))
   `(helm-ff-file-extension ((t (:foreground nil :background nil))))
   `(helm-ff-invalid-symlink ((t (:slant italic :inherit error))))
   `(helm-ff-symlink ((t (:inherit dired-symlink))))
   `(helm-ff-prefix ((t (:background nil))))
   `(helm-ff-dotted-directory ((t (:background nil :foreground ,color-middle))))
   `(helm-M-x-key ((t (:foreground ,color-bright))))
   `(helm-buffer-file ((t (:foreground ,color-fg))))
   `(helm-buffer-archive ((t (:inherit helm-buffer-file))))
   `(helm-buffer-directory ((t (:inherit dired-directory))))
   `(helm-buffer-not-saved ((t (:inherit helm-buffer-file :foreground unspecified :background ,color-strong))))
   `(helm-buffer-saved-out ((t (:inherit helm-buffer-not-saved))))
   `(helm-buffer-modified ((t (:foreground ,color-dark))))
   `(helm-buffer-process ((t (:foreground ,color-dark))))
   `(helm-buffer-size ((t (:foreground ,color-dark))))
   `(helm-match ((t (:inherit completions-common-part))))

   ;; Adoc-mode
   `(markup-meta-hide-face ((t (:height 1.0 :foreground ,color-bright))))
   `(markup-meta-face ((t (:height 1.0 :foreground ,color-dark :family nil))))
   `(markup-reference-face ((t (:underline nil :foreground ,color-dark))))
   `(markup-gen-face ((t (:inherit default :foreground nil))))
   `(markup-passthrough-face ((t (:inherit markup-dark))))
   `(markup-replacement-face ((t (:family nil :foreground ,color-dark))))
   `(markup-list-face ((t (:weight bold))))
   `(markup-secondary-text-face ((t (:height 1.0 :foreground ,color-dark))))
   `(markup-verbatim-face ((t (:foreground ,color-dark))))
   `(markup-code-face ((t (:inherit markup-verbatim-face))))
   `(markup-typewriter-face ((t (:inherit nil))))
   `(markup-complex-replacement-face ((t (:background ,color-light :foreground ,color-fg))))
   `(markup-title-0-face ((t (:height 1.4 :inherit markup-gen-face))))
   `(markup-title-1-face ((t (:height 1.3 :inherit markup-gen-face))))
   `(markup-title-2-face ((t (:height 1.2 :inherit markup-gen-face))))
   `(markup-title-3-face ((t (:height 1.1 :inherit markup-gen-face))))
   `(markup-title-4-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-5-face ((t (:height 1.0 :inherit markup-gen-face))))

   ;; Highlight-indent-guides
   `(highlight-indent-guides-odd-face ((t (:background ,color-bright))))
   `(highlight-indent-guides-even-face ((t (:background nil))))

   ;; Notmuch
   `(notmuch-search-unread-face ((t (:foreground ,color-bright))))
   `(notmuch-tag-face ((t (:foreground ,color-dark))))
   `(notmuch-tree-match-author-face ((t (:foreground ,color-middle))))
   `(notmuch-tree-no-match-face ((t (:foreground ,color-dark))))
   `(notmuch-tree-match-tag-face ((t (:inherit notmuch-tree-match-author-face))))
   `(notmuch-tag-unread-face ((t (:foreground ,color-fg :background ,color-middle))))
   `(notmuch-message-summary-face ((t (:foreground ,color-dark))))

   ;; Telega
   `(telega-msg-heading ((t (:foreground ,color-dark :background nil :inherit nil))))
   `(telega-msg-inline-reply ((t (:foreground ,color-bright :inherit nil))))
   `(telega-entity-type-texturl ((t (:inherit nil :foreground ,color-dark))))

   ;; Beancount
   `(beancount-date ((t (:inherit italic :foreground nil))))
   `(beancount-account ((t (:inherit default))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'giorgio)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; giorgio-theme.el ends here
