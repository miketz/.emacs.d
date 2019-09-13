;;; ultimate-theme.el --- Light pastelish -*- lexical-binding: t -*-

;;; Commentary:
;;; Lexical binding is not required for this theme.  It is only used as a
;;; micro-optimization for variable lookups in the let statement.

;;; Code:

(deftheme ultimate "Ultimate color theme")

(let* ((class        t)
       (todo--fg "black")
       (todo--bg "white")
       (i 0)
       ;; Color Palette
       (bg           "#D5D1B3")
       (fg           "#000000")
       (bg-highlight "#C5C1A3")
       (bg-purple    "#440033")
       (fg-purple    "#FFC0CB")
       (bg-green     "#004400")
       (fg-green     "#98FB98")
       (bg-yellow    "#3A3A00")
       (fg-yellow    "#FFFF00")
       (bg-red       "#300000")
       (fg-red       "#FF0000")
       (faint        "#4D4D3D")
       (fainter      "#3F3F35")
       (faint-less   "#8D8D8D")
       (keyword      "navy blue")
       (var          "blue")
       (highlight    "#FFA366")
       (popup-bg     "#222222")
       (scrollb-bg   "#000000")
       (scrollb-fg   "#999999")
       (mode-line-fg "#8DEECD")
       (mode-line-bg "#000000")
       (ml-inact-fg  "gray")
       (ml-inact-bg  "#000000")
       (ml-bufferid  "#F0DFAF")
       (ivy-line-bg  "#000000")
       (isearch-fg   "#FFFF00")
       (isearch-bg   "#000000")
       (rain-1       "#FF4500")
       (rain-1-bg    bg)
       (rain-2       "#00FFFF")
       (rain-2-bg    bg)
       (rain-3       "#FFFF00")
       (rain-3-bg    bg)
       (rain-4       "#DDA0DD")
       (rain-4-bg    bg)
       (rain-5       "#7CFC00")
       (rain-5-bg    bg)
       (rain-6       "#FFA500")
       (rain-6-bg    bg)
       (rain-7       "#FFFFFF")
       (rain-7-bg    bg)
       (rain-8       "#FF69B4")
       (rain-8-bg    "#101010")
       (rain-9       "#CDAA7D")
       (rain-9-bg    bg)
       (rain-fg-u    "#A0522D")
       (rain-bg-u    "#000000")
       (zenburn-fg+1     "#FFFFEF")
       (zenburn-fg       "#DCDCCC")
       (zenburn-fg-1     "#656555")
       (zenburn-bg-2     "#000000")
       (zenburn-bg-1     "#2B2B2B")
       (zenburn-bg-05    "#383838")
       (zenburn-bg       "#3F3F3F")
       (zenburn-bg+05    "#494949")
       (zenburn-bg+1     "#4F4F4F")
       (zenburn-bg+2     "#5F5F5F")
       (zenburn-bg+3     "#6F6F6F")
       (zenburn-red+1    "#DCA3A3")
       (zenburn-red      "#CC9393")
       (zenburn-red-1    "#BC8383")
       (zenburn-red-2    "#AC7373")
       (zenburn-red-3    "#9C6363")
       (zenburn-red-4    "#8C5353")
       (zenburn-orange   "#DFAF8F")
       (zenburn-yellow   "#F0DFAF")
       (zenburn-yellow-1 "#E0CF9F")
       (zenburn-yellow-2 "#D0BF8F")
       (zenburn-green-1  "#5F7F5F")
       (zenburn-green    "#7F9F7F")
       (zenburn-green+1  "#8FB28F")
       (zenburn-green+2  "#9FC59F")
       (zenburn-green+3  "#AFD8AF")
       (zenburn-green+4  "#BFEBBF")
       (zenburn-cyan     "#93E0E3")
       (zenburn-blue+1   "#94BFF3")
       (zenburn-blue     "#8CD0D3")
       (zenburn-blue-1   "#7CB8BB")
       (zenburn-blue-2   "#6CA0A3")
       (zenburn-blue-3   "#5C888B")
       (zenburn-blue-4   "#4C7073")
       (zenburn-blue-5   "#366060")
       (zenburn-magenta  "#DC8CC3"))

  ;; lightyellow2=#EEEED1
  ;; snow3 gray80 lightyellow3

  (custom-theme-set-variables
   'ultimate

   `(fci-rule-color ,faint)


   `(evil-emacs-state-cursor    '(bar "blue"))
   `(evil-normal-state-cursor   '(hollow "black"))
   `(evil-insert-state-cursor   '(bar "black"))
   `(evil-visual-state-cursor   '(hollow "black"))
   `(evil-operator-state-cursor '(box "red"))
   `(evil-replace-state-cursor  '(hbar "orange red"))
   `(evil-motion-state-cursor   '(box "black"))


   ;; ibuffer
   `(ibuffer-filter-group-name-face '((,class :weight bold
                                              :foreground ,fg
                                              :background "gray")))

   ;; pos-tip. Helper package for tooltip
   `(pos-tip-foreground-color ,fg-green)
   `(pos-tip-background-color ,bg-green))

  (custom-theme-set-faces
   'ultimate

   `(default ((,class (:foreground ,fg :background ,bg))))

   `(cursor ((,class (:background "blue"))))

   `(show-paren-match ((,class (:slant italic
                                       :bold t
                                       :strike-through t
                                       :background nil))))

   ;; line that separates vertically split windows.
   `(vertical-border ((,class (:foreground "gray25"))))

   `(mode-line
     ((,class (:foreground ,mode-line-fg ;"#8FB28F"
                           :background ,mode-line-bg ;"#2B2B2B"
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((,class (:foreground ,ml-bufferid ;"#F0DFAF"
                                               :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,ml-inact-fg ;"#5F7F5F"
                           :background ,ml-inact-bg ;"#383838"
                           :box (:line-width -1 :style released-button)))))

   `(line-number ((,class '(:background "black"
                                        :foreground "gray50"))))
   `(line-number-current-line ((,class (:inherit line-number
                                                 :foreground "#77AA55"
                                                 :background "#171717"))))

   `(region
     ((,class (:background ,highlight))))     ;69685E

   ;; TODO
   `(fringe ((,class (:background "black"))))

   ;; TODO font lock
   ;; `(font-lock-builtin-face ((,class (:foreground ,zenburn-fg :weight bold))))
   `(font-lock-comment-face ((,class (:foreground "dark green"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "dark green"))))
   ;; `(font-lock-constant-face ((,class (:foreground ,zenburn-green+4))))
   `(font-lock-doc-face ((,class (:foreground "dark red"))))
   `(font-lock-function-name-face ((,class :foreground "black"
                                           :background "lightsteelblue1"
										   :weight normal
										   ;; :box (:line-width -1 :color ,faint-less)
                                           )))
   `(font-lock-keyword-face ((,class (:foreground ,keyword :weight bold))))
   ;; `(font-lock-negation-char-face ((,class (:foreground ,zenburn-yellow :weight bold))))
   ;; `(font-lock-preprocessor-face ((,class (:foreground ,zenburn-blue+1))))
   ;; `(font-lock-regexp-grouping-construct ((,class (:foreground ,zenburn-yellow :weight bold))))
   ;; `(font-lock-regexp-grouping-backslash ((,class (:foreground ,zenburn-green :weight bold))))
   ;; `(font-lock-string-face ((,class (:foreground ,zenburn-red))))
   `(font-lock-type-face ((,class (:foreground "blue" :background ,bg-highlight))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   ;; `(font-lock-warning-face ((,class (:foreground ,zenburn-yellow-2 :weight bold))))

   `(minibuffer-prompt ((,class (:foreground ,fg :background ,bg-highlight))))

   ;; hl-fill-column
   `(hl-fill-column-face ((,class (:foreground ,fg-red
											   :background ,bg-red))))

   ;; avy
   `(avy-lead-face ((,class (:foreground "orange" :background "black" :weight normal :slant normal))))
   `(avy-lead-face-0 ((,class (:foreground "orange" :background "black" :weight normal :slant normal))))
   ;; `(avy-lead-face-1 ((,class (:foreground "green")))) ;; not used?

   ;; info
   `(info-xref-visited ((,class (:foreground ,fg-purple))))
   `(info-xref ((,class (:foreground ,fg-green))))
   `(Info-quoted ((,class (:inherit font-lock-constant-face)))) ; avoid font change.

   ;; slime
   `(slime-repl-inputed-output-face ((,class (:foreground ,fg-purple))))
   `(sldb-condition-face ((,class (:foreground ,fg-red
											   :background ,bg-red))))
   `(sldb-section-face ((,class (:foreground ,fg-yellow
											 :background ,bg-yellow
											 :box (:line-width -1 :style released-button)))))
   ;; sldb-reference-face
   ;; sldb-catch-tag-face
   ;; sldb-local-value-face
   ;; sldb-local-name-face
   ;; sldb-detailed-frame-line-face
   ;; sldb-non-restartable-frame-line-face
   ;; sldb-restartable-frame-line-face
   ;; sldb-frame-line-face
   ;; sldb-restart-number-face
   ;; sldb-restart-face
   ;; sldb-restart-type-face
   ;; sldb-frame-label-face
   ;; sldb-topline-face
   ;; slime-reader-conditional-face
   ;; slime-repl-output-mouseover-face
   ;; slime-repl-result-face
   ;; slime-repl-input-face
   ;; slime-repl-output-face
   ;; slime-repl-prompt-face
   ;; slime-inspector-type-face
   ;; slime-inspector-action-face
   ;; slime-inspector-value-face
   ;; slime-inspector-label-face
   ;; slime-inspector-topline-face
   ;; slime-apropos-label
   ;; slime-apropos-symbol
   ;; slime-highlight-face
   ;; slime-final-deprecation-warning-face
   ;; slime-late-deprecation-warning-face
   ;; slime-early-deprecation-warning-face
   ;; slime-note-face
   ;; slime-style-warning-face
   ;; slime-warning-face
   ;; slime-error-face


   ;; replace.el
   `(match ((,class (:background "#000000" :foreground ,fg-yellow :weight normal))))

   ;; num3
   ;; '(num3-face-odd ((,class)))
   ;; '(num3-face-even ((,class (:underline t :background "black"))))
   `(num3-face-even ((,class (:underline nil
										 :background "#000000"
										 :foreground ,fg-green
										 :bold nil))))

   ;; highlight-indent-guides
   `(highlight-indent-guides-character-face ((,class (:foreground ,fainter))))

   ;; default emacs completion.
   `(completions-common-part ((,class (:foreground ,faint-less))))
   `(completions-first-difference ((,class (:foreground ,fg-green))))

   ;; swiper, ivy, counsel
   `(swiper-line-face ((,class (:background "gray30"))))
   ;; face-1 fills in the space between matches. 2-4 are for matches.
   `(swiper-match-face-1 ((,class (:foreground ,faint-less :background "black"))))
   `(swiper-match-face-2 ((,class (:foreground ,rain-1 :background "black"))))
   ;; NOTE: face-3, 4 don't work when out-of-order matching is used.
   ;; TODO: make bug report to swiper about face 3,4
   `(swiper-match-face-3 ((,class (:foreground ,rain-2 :background "black"))))
   `(swiper-match-face-4 ((,class (:foreground ,rain-3 :background "black"))))
   `(ivy-action ((,class (:foreground ,fg-green))))
   ;; `(ivy-confirm-face ((,class (:foreground "yellow" :italic t))))
   `(ivy-current-match ((,class (:inherit swiper-line-face))))
   ;; `(ivy-cursor ((,class (:foreground "white"))))
   ;; `(ivy-highlight-face ((,class (:background "white"))))
   `(ivy-match-required-face ((,class (:foreground ,fg-red :background ,bg-red))))
   `(ivy-minibuffer-match-face-1 ((,class (:inherit swiper-match-face-1))))
   `(ivy-minibuffer-match-face-2 ((,class (:inherit swiper-match-face-2))))
   `(ivy-minibuffer-match-face-3 ((,class (:inherit swiper-match-face-3))))
   `(ivy-minibuffer-match-face-4 ((,class (:inherit swiper-match-face-4))))
   ;; `(ivy-minibuffer-match-highlight ((,class (:background "white"))))
   ;; `(ivy-modified-buffer ((,class (:foreground "white"))))
   ;; `(ivy-prompt-match ((,class (:foreground "white"))))
   ;; `(ivy-remote ((,class (:foreground "white"))))
   ;; `(ivy-subdir ((,class (:foreground "white"))))
   ;; `(ivy-virtual ((,class (:foreground "white"))))

   ;; ace-window
   `(aw-leading-char-face ; ace-window character.
     ((,class (:foreground ,fg-green
						   ;; :background "black"
						   :height 300)))) ; big font
   `(aw-background-face ((,class (:foreground ,faint-less))))

   ;; ido
   `(ido-first-match ((,class (:foreground ,fg-yellow :background ,bg-yellow))))
   `(ido-only-match ((,class (:foreground ,fg-green :background ,bg-green))))
   `(ido-subdir ((,class (:foreground ,fg-purple :background ,bg-purple))))

   ;; isearch
   `(isearch ((,class (:background ,isearch-bg
								   :foreground ,isearch-fg
								   :weight bold
								   :underline nil))))
   ;; the non-selected matches from isearch
   `(lazy-highlight ((,class (:background ,bg-purple
										  :foreground ,fg-purple
										  :weight normal
										  :underline t))))

   `(cider-result-overlay-face ((,class (:background ,bg-green
                                                     :foreground ,fg-green
                                                     :box (:line-width -1 :color "black")))))

   `(eros-result-overlay-face ((,class (:inherit cider-result-overlay-face))))

   `(header-line
     ((,class (:foreground ,fg-yellow
                           :background ,bg-yellow
                           :weight normal
                           :box (:line-width -1 :style released-button)))))

   ;; magit
   `(magit-section-highlight ((,class (:background ,faint))))

   ;; tooltip
   `(tooltip ((,class (:background ,popup-bg :foreground ,fg))))

   ;; js2
   `(js2-function-call ((,class (:inherit font-lock-function-name-face))))
   `(js2-object-property ((,class (:inherit font-lock-variable-name-face))))
   `(js2-function-param ((,class (:inherit font-lock-variable-name-face))))
   `(js2-warning ((,class :underline (:color "yellow" :style wave))))
   `(js2-error ((,class :underline (:color "red" :style wave))))
   `(js2-external-variable ((,class (:underline (:color "orange" :style wave)))))
   `(js2-jsdoc-tag ((,class (:foreground "lime green"))))
   `(js2-jsdoc-type ((,class (:inherit font-lock-type-face))))
   `(js2-jsdoc-value ((,class (:inherit font-lock-constant-face))))

   ;; js2-highlight-vars
   `(js2-highlight-vars-face ((,class (:foreground ,fg-green :background ,bg-green))))
   `(js2-highlight-vars-second-face ((,class (:foreground ,fg-purple :background ,bg-purple))))

   ;; web-mode
   `(web-mode-current-element-highlight-face ((,class (:background ,faint))))
   `(web-mode-current-column-highlight-face ((,class (:background ,faint))))

   ;; company
   `(company-tooltip ((,class (:background ,popup-bg :foreground ,fg))))
   `(company-scrollbar-bg ((,class (:background ,scrollb-bg))))
   `(company-scrollbar-fg ((,class (:background ,scrollb-fg))))
   `(company-tooltip-common ((,class (:foreground ,faint-less))))
   `(company-tooltip-selection ((,class (:background ,highlight))))
   ;; company-echo
   ;; company-echo-common
   ;; company-preview
   ;; company-preview-common
   ;; company-preview-search
   ;; company-template-field
   ;; company-tooltip-annotation
   ;; company-tooltip-annotation-selection
   ;; company-tooltip-common-selection
   ;; company-tooltip-mouse
   ;; company-tooltip-search
   ;; company-tooltip-search-selection

   ;; compile
   `(compilation-info ((,class (:foreground ,fg-green :underline t))))
   `(compilation-line-number ((,class (:inherit font-lock-keyword-face
                                                :weight normal))))

   ;; deadgrep
   `(deadgrep-meta-face ((,class (:foreground "gray"))))
   `(deadgrep-filename-face ((,class (:inherit compilation-info))))
   `(deadgrep-search-term-face ((,class (:foreground ,fg-green :background ,bg-green))))
   `(deadgrep-regexp-metachar-face ((,class (:foreground ,fg-purple :background ,bg-green))))
   `(deadgrep-match-face ((,class (:inherit match))))

   ;; erc
   `(erc-notice-face ((,class (:foreground ,faint-less))))
   `(erc-input-face ((,class (:foreground "tan")))) ; my own text.
   `(erc-timestamp-face ((,class (:foreground ,fg-green))))
   ;; erc-keyword-face
   ;; erc-fool-face
   ;; erc-pal-face
   ;; erc-dangerous-host-face
   ;; erc-current-nick-face
   ;; erc-hl-nicks-nick-base-face
   ;; erc-button
   ;; erc-underline-face
   ;; erc-inverse-face
   ;; erc-bold-face
   ;; erc-nick-msg-face
   ;; erc-nick-default-face
   ;; erc-my-nick-face
   ;; erc-error-face
   ;; erc-action-face
   ;; erc-command-indicator-face
   ;; erc-prompt-face
   ;; erc-header-line
   ;; erc-direct-msg-face
   ;; erc-my-nick-prefix-face
   ;; erc-nick-prefix-face
   ;; erc-default-face

   ;; erc-hl-nicks
   ;; erc-hl-nicks-nick-USERNAME1-face ; dynamically created faces? handle in a loop?
   ;; erc-hl-nicks-nick-USERNAME2-face ; dynamically created faces? handle in a loop?

   ;; minesweeper
   `(minesweeper-neighbor ((,class (:background "black"))))
   `(minesweeper-blank ((,class (:foreground ,faint-less))))
   `(minesweeper-marked ((,class (:foreground ,fg))))
   `(minesweeper-0 ((,class (:foreground ,faint-less))))
   `(minesweeper-1 ((,class (:foreground "cyan";"deepskyblue"
                             ))))
   `(minesweeper-2 ((,class (:foreground "lawn green";"yellowgreen"
                             ))))
   `(minesweeper-3 ((,class (:foreground ,rain-4
                             ))))
   `(minesweeper-4 ((,class (:foreground ,rain-1))))
   `(minesweeper-5 ((,class (:foreground ,rain-3))))
   `(minesweeper-6 ((,class (:foreground ,rain-6))))
   `(minesweeper-7 ((,class (:foreground ,rain-7))))
   `(minesweeper-8 ((,class (:foreground ,rain-8))))
   `(minesweeper-explode ((,class (:foreground "black"
                                               :background "red"
                                               :weight bold))))
   `(minesweeper-mismarked ((,class (:background ,highlight
                                                 :foreground "orange"
                                                 :weight bold))))


   ;; ediff. temporarily borrowing from zenburn.
   ;; TODO: customize colors for ultimate
   `(ediff-current-diff-A ((,class (:foreground ,zenburn-fg :background ,zenburn-red-4))))
   `(ediff-current-diff-Ancestor ((,class (:foreground ,zenburn-fg :background ,zenburn-red-4))))
   `(ediff-current-diff-B ((,class (:foreground ,zenburn-fg :background ,zenburn-green-1))))
   `(ediff-current-diff-C ((,class (:foreground ,zenburn-fg :background ,zenburn-blue-5))))
   `(ediff-even-diff-A ((,class (:background ,zenburn-bg+1))))
   `(ediff-even-diff-Ancestor ((,class (:background ,zenburn-bg+1))))
   `(ediff-even-diff-B ((,class (:background ,zenburn-bg+1))))
   `(ediff-even-diff-C ((,class (:background ,zenburn-bg+1))))
   `(ediff-fine-diff-A ((,class (:foreground ,zenburn-fg :background ,zenburn-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((,class (:foreground ,zenburn-fg :background ,zenburn-red-2 weight bold))))
   `(ediff-fine-diff-B ((,class (:foreground ,zenburn-fg :background ,zenburn-green :weight bold))))
   `(ediff-fine-diff-C ((,class (:foreground ,zenburn-fg :background ,zenburn-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((,class (:background ,zenburn-bg+2))))
   `(ediff-odd-diff-Ancestor ((,class (:background ,zenburn-bg+2))))
   `(ediff-odd-diff-B ((,class (:background ,zenburn-bg+2))))
   `(ediff-odd-diff-C ((,class (:background ,zenburn-bg+2))))

   ;; markdown-mode
   `(markdown-code-face ((,class (:inherit font-lock-constant-face)))) ; avoid font change.

   ;; org
   `(org-hide ((,class (:foreground ,faint))))

   `(rainbow-delimiters-depth-1-face ((t (:foreground "black" :background ,bg-highlight :weight bold))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "#C0DfDf" :weight normal))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background ,bg-highlight :weight normal))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "#DfD0D5" :weight normal))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "#EfEaBd" :weight normal))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background ,bg ;"#EEEEFF"
                                                      :weight normal))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground "gray52" :weight normal))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3" :background ,bg-highlight :weight normal))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "gray50" ;:background "#fff7ca"
                                                      :weight normal))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black" :weight normal))))))

(provide-theme 'ultimate)

;;; ultimate-theme.el ends here