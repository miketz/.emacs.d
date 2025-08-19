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
       (bg-purple    "#FFC0CB")
       (fg-purple    "#440033")
       (bg-green     "#98FB98")
       (fg-green     "black")
       (bg-yellow    "#FFFF00")
       (fg-yellow    "black")
       (bg-red       "#FF4500")
       (bg-red2      "#FFB5B5")
       (fg-red       "#300000")
       (faint        "#F0DBBD")
       (fainter      "#DDD9BB")
       (faint-less   "#B5B193")
       (faint-lesser "#858163")
       (keyword      "navy blue")
       (var          "blue")
       (highlight    "#FFA366")
       (popup-bg     "powder blue")
       (scrollb-bg   bg)
       (scrollb-fg   "royalblue")
       (mode-line-fg "#8DEECD")
       (mode-line-bg "#000000")
       (ml-inact-fg  "gray")
       (ml-inact-bg  "#000000")
       (ml-bufferid  "#F0DFAF")
       (ivy-line-bg  "#000000")
       (isearch-fg   "#000000")
       (isearch-bg   "#FFFF00")
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
       (rain-bg-u    "#000000"))

  ;; lightyellow2=#EEEED1
  ;; snow3 gray80 lightyellow3

  (custom-theme-set-variables
   'ultimate

   `(fci-rule-color ,faint)


   `(evil-emacs-state-cursor    '(bar "blue"))
   `(evil-normal-state-cursor   '(box "dark green"))
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

   `(line-number ((,class :background "#656143"
                          :foreground "#AAAAAA")))
   `(line-number-current-line ((,class :inherit line-number
                                       :foreground "#d0c66c")))

   `(region
     ((,class (:background ,highlight))))     ;69685E

   `(fringe ((,class (:background "#959173"))))

   ;; window-divider. built-in mode. useful when modeline is disabled
   `(window-divider ((,class (:foreground "black"))))
   `(window-divider-first-pixel ((,class (:inherit window-divider))))
   `(window-divider-last-pixel ((,class (:inherit window-divider))))

   ;; TODO font lock
   `(font-lock-builtin-face ((,class :foreground "dodgerblue4"
                                     :weight bold)))
   `(font-lock-comment-face ((,class (:foreground "dark green"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "dark green"))))
   ;; `(font-lock-constant-face ((,class (:foreground ,zenburn-green+4))))
   `(font-lock-doc-face ((,class :foreground "dark green"
                                 :background "#DDDDDD";;,faint-less
                                 )))
   `(font-lock-function-name-face ((,class :foreground "black"
                                           :background "lightsteelblue1"
										   ;; :weight normal
										   ;; :box (:line-width -1 :color ,faint-less)
                                           )))
   `(font-lock-keyword-face ((,class (:foreground ,keyword :weight bold))))
   `(font-lock-negation-char-face ((,class :foreground "#Ae0000"
                                           :background "#ffe0eb"
                                           ;; :box (:line-width -1 :style pressed-button)
                                           :weight bold)))
   `(font-lock-preprocessor-face ((,class (:foreground "#5b1503"))))
   ;; `(font-lock-regexp-grouping-construct ((,class (:foreground ,zenburn-yellow :weight bold))))
   ;; `(font-lock-regexp-grouping-backslash ((,class (:foreground ,zenburn-green :weight bold))))
   `(font-lock-string-face ((,class (:foreground "dark red"))))
   `(font-lock-type-face ((,class (:foreground "blue" :background ,bg-highlight))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   ;; `(font-lock-warning-face ((,class (:foreground ,zenburn-yellow-2 :weight bold))))


   ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ;; ~ START faces supported with treesit
   ;; ~ Maybe not intended to be treesit only, but it seems that way.
   `(font-lock-function-call-face ((,class :inherit font-lock-function-name-face
                                           :foreground "black"
                                           :background "#ffd7e2")))
   `(font-lock-variable-use-face ((,class :inherit default)))
   ;; `(font-lock-operator-face ((,class :inherit font-lock-keyword-face
   ;;                                    :weight normal)))
   `(font-lock-property-name-face ((,class :inherit font-lock-variable-name-face
                                   :background "lightsteelblue1")))

   ;; font-lock-property-use-face
   ;; `(font-lock-punctuation-face ((,class nil)))
   ;; `(font-lock-bracket-face)
   ;; `(font-lock-delimiter-face)
   `(font-lock-escape-face ((t :foreground "black"
                               :background "lightsteelblue1")))

   ;; `(font-lock-misc-punctuation-face)
   ;; `(font-lock-number-face ((,class :foreground "#50b5b5" ;"#BEBEA1"
   ;;                                  ;; :background ,fn-def-bg
   ;;                                  )))
   ;; `(font-lock-regexp-face)

   ;; ~ END treesit faces
   ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   `(minibuffer-prompt ((,class (:foreground ,fg :background ,bg-highlight))))

   ;; hl-fill-column
   `(hl-fill-column-face ((,class (:foreground ,fg-red
											   :background ,bg-red))))

   ;; avy
   `(avy-lead-face ((,class :background "orange";"yellow"
                            :foreground "black")))
   `(avy-lead-face-0 ((,class :background "cyan"
                              :foreground "black")))
   `(avy-lead-face-1 ((,class :background "green" ;; not lettersused?
                              :foreground "black")))
   `(avy-lead-face-2 ((,class :background "#FF4500" ;; not used?
                              :foreground "black")))
   `(avy-goto-char-timer-face ((,class :background "DarkSeaGreen2")))
   `(avy-background-face ((,class :foreground ,faint-lesser)))

   ;; info
   `(info-xref-visited ((,class (:foreground "medium sea green"))))
   `(info-xref ((,class (:foreground "dark blue"))))
   `(Info-quoted ((,class (:inherit font-lock-constant-face)))) ; avoid font change.

   ;; slime
   `(slime-repl-inputed-output-face ((,class (:foreground ,fg-purple))))
   `(sldb-condition-face ((,class (:foreground ,fg-red
											   :background ,bg-red))))
   `(sldb-section-face ((,class (:foreground ,fg-yellow
											 :background ,bg-yellow
											 :box (:line-width -1 :style released-button)))))
   `(sldb-restartable-frame-line-face ((,class (:foreground ,fg-green
                                                            :background ,bg-green))))
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
   `(match ((,class :foreground ,fg-yellow :background ,bg-yellow)))

   ;; num3
   ;; '(num3-face-odd ((,class)))
   ;; '(num3-face-even ((,class (:underline t :background "black"))))
   `(num3-face-even ((,class (:underline nil
										 :background "#252525"
										 :foreground "#82FA32"
										 :bold nil))))

   ;; highlight-indent-guides
   `(highlight-indent-guides-character-face ((,class (:foreground ,fainter))))

   ;; default emacs completion.
   `(completions-common-part ((,class (:foreground ,faint-lesser))))
   `(completions-first-difference ((,class :foreground ,fg-green
                                           :background ,bg-green)))

   ;; swiper, ivy, counsel
   `(swiper-line-face ((,class (:background ,highlight ;,bg-highlight ;"gray30"
                                ))))
   ;; face-1 fills in the space between matches. 2-4 are for matches.
   `(swiper-match-face-1 ((,class :foreground "white";"#F9F5D7"
                                  :background "#A5A183";,bg-highlight
                                  )))
   `(swiper-match-face-2 ((,class (:foreground "black" :background ,rain-1))))
   ;; NOTE: face-3, 4 don't work when out-of-order matching is used.
   ;; TODO: make bug report to swiper about face 3,4
   `(swiper-match-face-3 ((,class (:foreground "black" :background ,rain-2))))
   `(swiper-match-face-4 ((,class (:foreground "black" :background ,rain-3))))
   ;; the non-selected lines in the minibuffer
   `(swiper-background-match-face-1 ((,class :inherit swiper-match-face-1)))
   `(swiper-background-match-face-2 ((,class :inherit swiper-match-face-2)))
   `(swiper-background-match-face-3 ((,class :inherit swiper-match-face-3)))
   `(swiper-background-match-face-4 ((,class :inherit swiper-match-face-4)))
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
   ;; `(ivy-virtual ((,class (:foreground "white" :background ,bg))))

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


   ;; ido-grid
   `(ido-grid-common-match ((,class :foreground ,fg-green
                                    :background ,bg-green)))
   ;; `(ido-grid-match ((,class :foreground "pink")))
   `(ido-grid-match-1 ((,class :inherit completions-common-part)))
   ;; `(ido-grid-match-2 ((,class :foreground "purple" :background "yellow")))
   ;; `(ido-grid-match-3 ((,class :foreground "black" :background "white")))

   ;; icomplete, fido
   `(icomplete-first-match ((,class (:foreground ,fg-yellow :background ,bg-yellow))))

   ;; isearch
   `(isearch ((,class (:background ,isearch-bg
								   :foreground ,isearch-fg
								   :weight bold
								   :underline nil))))
   ;; the non-selected matches from isearch
   `(lazy-highlight ((,class (:inherit isearch
									   ;:weight normal
                                       ))))

   `(cider-result-overlay-face ((,class (:background ,bg-green
                                                     :foreground ,fg-green
                                                     :box (:line-width -1 :color "black")))))

   `(eros-result-overlay-face ((,class (:inherit cider-result-overlay-face))))

   `(header-line
     ((,class (:foreground ,fg-yellow
                           :background ,bg-yellow
                           ;; :weight normal
                           :box (:line-width -1 :style released-button)))))

   ;; magit
   `(magit-section-highlight ((,class (:background "#C5FfC5";,faint
                                       ))))
   `(magit-log-author ((,class (:foreground "deepskyblue4"))))
   `(magit-log-date ((,class (:foreground "#803030"))))
   `(magit-hash ((,class (:foreground "#803030"))))

   ;; tooltip
   `(tooltip ((,class (:background ,popup-bg :foreground ,fg))))

   ;; js2
   `(js2-function-call ((,class (:inherit font-lock-function-name-face))))
   `(js2-object-property ((,class (:inherit font-lock-variable-name-face))))
   `(js2-function-param ((,class (:inherit font-lock-variable-name-face))))
   `(js2-warning ((,class :underline (:color "yellow" :style wave )
                          :background "black"
                          :foreground "yellow")))
   `(js2-error ((,class :underline (:color "red" :style wave)
                        :background "black"
                        :foreground "red")))
   `(js2-external-variable ((,class :underline (:color "orange" :style wave)
                                    :background "black"
                                    :foreground "orange")))
   `(js2-jsdoc-tag ((,class (:foreground "lime green"))))
   `(js2-jsdoc-type ((,class (:inherit font-lock-type-face))))
   `(js2-jsdoc-value ((,class (:inherit font-lock-constant-face))))

   ;; js2-highlight-vars
   `(js2-highlight-vars-face ((,class (:foreground ,fg-green :background ,bg-green))))
   `(js2-highlight-vars-second-face ((,class (:foreground ,fg-purple :background ,bg-purple))))

   ;; web-mode
   `(web-mode-current-element-highlight-face ((,class (:background ,faint))))
   `(web-mode-current-column-highlight-face ((,class (:background ,faint))))
   `(web-mode-html-attr-name-face ((,class :foreground ,fg)))
   `(web-mode-html-tag-face ((,class :foreground "#1059ff")))
   `(web-mode-doctype-face ((,class :foreground ,fg
                                    :background "gray")))

   ;; company
   `(company-tooltip ((,class (:background ,bg-highlight :foreground ,fg))))
   `(company-scrollbar-bg ((,class (:background ,scrollb-bg))))
   `(company-scrollbar-fg ((,class (:background ,scrollb-fg))))
   `(company-tooltip-common ((,class (:foreground "#F8F8F8"))))
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
   ;; complication-warning inherits from warning. waring is used for mode names by ibufffer. like Elisp/d.
   `(warning ((,class :foreground ,fg-yellow :background ,bg-yellow)))
   `(compilation-error ((,class :foreground "red"
                                :background "#ffe0eb"
                                :weight bold)))

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
   `(erc-notice-face ((,class (:foreground ,faint-lesser))))
   `(erc-input-face ((,class (:foreground "DeepSkyBlue4")))) ; my own text.
   `(erc-timestamp-face ((,class :foreground ,faint-lesser
                                 :background ,fainter)))
   ;; erc-keyword-face
   ;; erc-fool-face
   ;; erc-pal-face
   ;; erc-dangerous-host-face
   `(erc-current-nick-face ((,class :foreground ,fg)))
   ;; erc-hl-nicks-nick-base-face
   ;; erc-button
   ;; erc-underline-face
   ;; erc-inverse-face
   ;; erc-bold-face
   `(erc-nick-msg-face ((,class :foreground ,fg-red :background ,bg-red)))
   `(erc-direct-msg-face ((,class :foreground ,fg-red :background ,bg-red2)))
   ;; erc-nick-default-face
   ;; erc-my-nick-face
   ;; erc-error-face
   ;; erc-action-face
   ;; erc-command-indicator-face
   ;; erc-prompt-face
   ;; erc-header-line
   ;; erc-my-nick-prefix-face
   ;; erc-nick-prefix-face
   ;; erc-default-face

   ;; erc-hl-nicks
   ;; erc-hl-nicks-nick-USERNAME1-face ; dynamically created faces? handle in a loop?
   ;; erc-hl-nicks-nick-USERNAME2-face ; dynamically created faces? handle in a loop?

   ;; minesweeper
   `(minesweeper-neighbor ((,class (:background "#EEEEEE"))))
   `(minesweeper-blank ((,class (:foreground ,faint-lesser))))
   `(minesweeper-marked ((,class (:foreground ,fg))))
   `(minesweeper-0 ((,class (:foreground ,faint-lesser))))
   `(minesweeper-1 ((,class (:foreground "blue";"deepskyblue"
                             ))))
   `(minesweeper-2 ((,class (:foreground "forest green";"yellowgreen"
                             ))))
   `(minesweeper-3 ((,class (:foreground "purple"
                             ))))
   `(minesweeper-4 ((,class (:foreground "red"))))
   `(minesweeper-5 ((,class (:foreground "darkorange" :background "#555555"))))
   `(minesweeper-6 ((,class (:foreground "yellow" :background "black"))))
   `(minesweeper-7 ((,class (:foreground "white" :background "black"))))
   `(minesweeper-8 ((,class (:foreground ,rain-8 :background "black"))))
   `(minesweeper-explode ((,class (:foreground "black"
                                               :background "red"
                                               :weight bold))))
   `(minesweeper-mismarked ((,class (:background "black"
                                                 :foreground "orange"
                                                 :weight bold))))


   ;; ediff. temporarily borrowing from zenburn.
   ;; TODO: customize colors for ultimate
   ;; `(ediff-current-diff-A ((,class (:foreground ,zenburn-fg :background ,zenburn-red-4))))
   ;; `(ediff-current-diff-Ancestor ((,class (:foreground ,zenburn-fg :background ,zenburn-red-4))))
   ;; `(ediff-current-diff-B ((,class (:foreground ,zenburn-fg :background ,zenburn-green-1))))
   ;; `(ediff-current-diff-C ((,class (:foreground ,zenburn-fg :background ,zenburn-blue-5))))
   ;; `(ediff-even-diff-A ((,class (:background ,zenburn-bg+1))))
   ;; `(ediff-even-diff-Ancestor ((,class (:background ,zenburn-bg+1))))
   ;; `(ediff-even-diff-B ((,class (:background ,zenburn-bg+1))))
   ;; `(ediff-even-diff-C ((,class (:background ,zenburn-bg+1))))
   `(ediff-fine-diff-A ((,class (:background "red" :weight bold))))
   ;; `(ediff-fine-diff-Ancestor ((,class (:foreground ,zenburn-fg :background ,zenburn-red-2 weight bold))))
   `(ediff-fine-diff-B ((,class (:background "green" :weight bold))))
   ;; `(ediff-fine-diff-C ((,class (:foreground ,zenburn-fg :background ,zenburn-blue-3 :weight bold ))))
   ;; `(ediff-odd-diff-A ((,class (:background ,zenburn-bg+2))))
   ;; `(ediff-odd-diff-Ancestor ((,class (:background ,zenburn-bg+2))))
   ;; `(ediff-odd-diff-B ((,class (:background ,zenburn-bg+2))))
   ;; `(ediff-odd-diff-C ((,class (:background ,zenburn-bg+2))))


   ;; diff
   `(diff-indicator-removed ((,class :background "red")))
   `(diff-removed ((,class :background "#FF8888")))
   `(diff-refine-removed ((,class :background "#Ff8080")))
   `(diff-added ((,class :background "#eeFFdd")))
   `(diff-indicator-added ((,class :background "green")))
   `(diff-refine-added ((,class :background ,bg-green)))

   ;; rg
   `(rg-filename-face ((,class :inherit font-lock-constant-face)))
   `(rg-match-face ((,class :foreground ,fg-yellow :background ,bg-yellow)))
   `(rg-match-position-face ((,class :foreground ,fg)))
   `(rg-info-face ((,class :inherit font-lock-function-name-face)))

   ;; markdown-mode
   `(markdown-code-face ((,class (:inherit font-lock-constant-face)))) ; avoid font change.
   `(markdown-markup-face ((,class :foreground "black"
                                   :background ,faint)))

   ;; org
   `(org-hide ((,class (:foreground ,faint))))

   ;; Emacs built-in display-fill-column-indicator.
   ;; replaces `fci-mode' and it's color variable `fci-rule-color'
   `(fill-column-indicator ((,class :foreground ,fainter)))

   ;; package
   `(package-status-new ((,class :weight bold
                                 :background ,bg-green
                                 :foreground ,fg-green)))

   `(rainbow-delimiters-depth-1-face ((t (:foreground ,fg :background "#FF6520" :weight bold))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,fg :background "#75FFFF" :weight bold))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,fg :background "yellow" :weight bold))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,fg :background "MediumPurple1" :weight bold))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,fg :background "#7CFC00" :weight bold))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,fg :background "orange" :weight bold))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,fg :background "pink" :weight bold))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,fg :background "dodger blue" :weight bold))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,fg :background "#CDAA7D" :weight bold))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black" :weight bold))))

   ;; custom faces for my config
   `(my-tilde-face ((,class :foreground "blue"
                            :background ,bg)))))

(provide-theme 'ultimate)

;;; ultimate-theme.el ends here