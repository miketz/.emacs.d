;;; charcoal-theme.el --- Chalkish charcoal colors. -*- lexical-binding: t -*-

;;; Commentary:
;;; Supports rgb, 256, 16, and 8 color environments.  The main focus is on the
;;; full 16777216 color rgb version.  Colors are not restricted in the full
;;; rgb version for the sake of consistency with the lesser versions.

;;; full color tested on ms-widnows and linux-X11
;;; 256 color tested on linux in gnome-terminal
;;; 16 color tested on ms-windows in cmd.exe
;;; 8 color tested on linux in text mode
;;; 8 color is messed up on linux in xterm.  TODO: handle it

;;; Lexical binding is not required for this theme.  It is only used as a
;;; micro-optimization for variable lookups in the let statement.  There should
;;; be no difference in behavior if dynamic binding is used.

;;; Code:

(deftheme charcoal "Charcoal color theme")

(defvar charcoal-color-cnt (display-color-cells)
  "The color count of the computer.
Used to decide the most colorful version of the theme that can be used.

Override this value to try out a lesser version of the theme.
Example:
    (let ((charcoal-color-cnt 8))
      (load-theme 'charcoal t))")

(let* ((class        t)
       (i            (cond ((>= charcoal-color-cnt 16777216) 0)
                           ((>= charcoal-color-cnt 256)      1)
                           ((>= charcoal-color-cnt 16)       2)
                           ((>= charcoal-color-cnt 8)        3)
                           (t                                3)))
       (todo--fg     "#FFFFFF") ; temp color where I haven't decided yet
       (todo--bg     "#000000") ; temp color where I haven't decided yet
       ;; Color Palette       full      256       16        8
       (bg           (aref `["#35352B" "#262626" "#000000" "#000000"] i))
       (fg           (aref `["#EEEED1" "#FFFFD7" "#BEBEBE" "#FFFFFF"] i))
       (bg-purple    (aref `["#440033" "#5F005F" "#8B008B" "#000000"] i))
       (fg-purple    (aref `["#FFC0CB" "#FFAFD7" "#FF00FF" "#FF00FF"] i))
       (bg-green     (aref `["#004400" "#005F00" "#228B22" "#000000"] i))
       (fg-green     (aref `["#98FB98" "#00FF87" "#00FF00" "#00FF00"] i))
       (bg-yellow    (aref `["#3A3A00" "#5F5F00" "#A0522D" "#000000"] i))
       (fg-yellow    (aref `["#FFFF00" "#FFFF00" "#FFFFFF" "#FFFF00"] i))
       (bg-red       (aref `["#300000" ,todo--bg "#000000" ,todo--bg] i))
       (fg-red       (aref `["#FF0000" ,todo--fg "#FF0000" ,todo--fg] i))
       (bg-blue      (aref `["#021458" ,todo--bg ,todo--bg ,todo--bg] i))
       (fg-blue      (aref `["#30cf9f" ,todo--fg ,todo--fg ,todo--fg] i))
       (faint        (aref `["#4D4D3D" "#303030" "#666666" "#0000FF"] i))
       (fainter      (aref `["#3F3F35" ,todo--fg "#0000CD" ,todo--fg] i))
       (faint-less   (aref `["#8D8D8D" "#6C6C6C" "#666666" "#0000FF"] i))
       (keyword      (aref `["#EEDD82" "#FFAFFF" "#FFFFFF" "#FFFFFF"] i))
       (var          (aref `["#66CDAA" "#D75FAF" "#00CED1" "#00FFFF"] i))
       (highlight    (aref `["#8B5742" "#4E4E4E" "#228B22" "#FF00FF"] i))
       (popup-bg     (aref `["#222222" "#000000" "#0000CD" "#FFFF00"] i))
       (scrollb-bg   (aref `["#000000" "#080808" "#666666" "#0000FF"] i))
       (scrollb-fg   (aref `["#999999" "#BCBCBC" "#FFFFFF" "#FFFFFF"] i))
       (mode-line-fg (aref `["#8FB28F" ,fg       "#BEBEBE" "#FFFFFF"] i))
       (mode-line-bg (aref `["#151515" "#121212" "#0000CD" "#FFFF00"] i))
       (ml-inact-fg  (aref `["#5F7F5F" ,fg       "#BEBEBE" "#FFFFFF"] i))
       (ml-inact-bg  (aref `["#383838" "#3A3A3A" "#000000" "#0000FF"] i))
       (ml-bufferid  (aref `["#F0DFAF" "#FFFFAF" "#BEBEBE" "#FFFFFF"] i))
       (ivy-line-bg  (aref `["#000000" ,todo--bg "#0000CD" ,todo--bg] i))
       (isearch-fg   (aref `["#FFFF00" ,fg-yellow ,fg-yellow ,fg-yellow] i))
       (isearch-bg   (aref `["#000000" ,bg-yellow ,bg-yellow ,bg-yellow] i))
       (fn-def-fg    (aref `["#AFEEEE" "#AFFFFF" "#00ced1" ,todo--fg] i))
       (fn-def-bg    (aref `["#000000" "#000000" ,todo--bg ,todo--bg] i))
       (fn-call-fg   (aref `["#EFB0BB" "#ffafd7" "#ff0000" ,todo--fg] i))
       (fn-call-bg   (aref `["#232319" "#000000" ,todo--bg ,todo--bg] i))
       (linenum-fg   (aref `["#595959" "#626262" ,todo--bg ,todo--bg] i))
       (linenum-bg   (aref `["#231808" "#000000" ,todo--bg ,todo--bg] i))
       (linenumcur-fg (aref `["#77AA55" "#77AA55" ,todo--fg ,todo--fg] i))
       (linenumcur-bg (aref `["#231808" "#000000" ,todo--bg ,todo--bg] i))
       (rain-1       (aref `["#FF4500" "#FF0000" "#FF0000" "#FF0000"] i))
       (rain-1-bg    (aref `[,bg       ,bg       ,bg       ,bg      ] i))
       (rain-2       (aref `["#00FFFF" "#00FFFF" "#00FFFF" "#00FFFF"] i))
       (rain-2-bg    (aref `[,bg       ,bg       ,bg       ,bg      ] i))
       (rain-3       (aref `["#FFFF00" "#FFFF00" "#A0522D" "#FFFF00"] i))
       (rain-3-bg    (aref `[,bg       ,bg       ,bg       ,bg      ] i))
       (rain-4       (aref `["#DDA0DD" "#D75FFF" "#FF00FF" "#FF00FF"] i))
       (rain-4-bg    (aref `[,bg       ,bg       ,bg       ,bg      ] i))
       (rain-5       (aref `["#7CFC00" "#00FF00" "#00FF00" "#00FF00"] i))
       (rain-5-bg    (aref `[,bg       ,bg       ,bg       ,bg      ] i))
       (rain-6       (aref `["#FFA500" "#FF8700" "#FFFFFF" "#FFFFFF"] i))
       (rain-6-bg    (aref `[,bg       ,bg       ,bg       ,bg      ] i))
       (rain-7       (aref `["#FFFFFF" "#FFFFFF" "#B22222" "#0000FF"] i))
       (rain-7-bg    (aref `[,bg       ,bg       ,bg       ,bg      ] i))
       (rain-8       (aref `["#FF69B4" "#FF87FF" "#00CED1" "#000000"] i))
       (rain-8-bg    (aref `["#101010" "#080808" ,bg       "#FF00FF"] i))
       (rain-9       (aref `["#CDAA7D" "#CDCD00" "#0000FF" "#000000"] i))
       (rain-9-bg    (aref `[,bg       ,bg       ,bg       "#0000FF"] i))
       (rain-fg-u    (aref `["#A0522D" "#AF0000" "#FFFFFF" "#FFFFFF"] i))
       (rain-bg-u    (aref `["#000000" "#080808" "#FF0000" "#FF0000"] i))
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
   'charcoal

   `(fci-rule-color ,faint)

   `(evil-emacs-state-cursor    '(bar "cyan"))
   `(evil-normal-state-cursor   '(hollow "spring green"))
   `(evil-insert-state-cursor   '(bar "spring green"))
   `(evil-visual-state-cursor   '(hollow "orange"))
   `(evil-operator-state-cursor '(box "red"))
   `(evil-replace-state-cursor  '(hbar "orange red"))
   `(evil-motion-state-cursor   '(box "spring green"))

   ;; ibuffer
   `(ibuffer-filter-group-name-face '((,class :weight bold
                                              :foreground ,fg
                                              :background "black")))

   ;; indent-bars
   ;; This is hard to configure in a considerate way in a theme. Becuase
   ;; `indent-bars-color-by-depth' mixes several things in 1 var. A boolean
   ;; feature toggle (ie non-nil), color settings, and blend.
   ;; since this is my personal theme, just do it how i want it.
   `(indent-bars-color-by-depth
     ;; match with `rainbow-delimiters' faces.
     '(:palette (,rain-2 ; 2nd color first as first level is 0 and not drawn.
                 ,rain-3 ,rain-4 ,rain-5 ,rain-6 ,rain-7 ,rain-8 ,rain-9
                 ,rain-1 ; red last to match rainbow-delimiters after wrap around
                 )
                :blend 0.3))

   ;; Duplicating the default vc-annotate colors for now.
   ;; TODO: tailor them for the bg.
   `(vc-annotate-color-map '((20 . "#FF3F3F")
                             (40 . "#FF6C3F")
                             (60 . "#FF993F")
                             (80 . "#FFC63F")
                             (100 . "#FFF33F")
                             (120 . "#DDFF3F")
                             (140 . "#B0FF3F")
                             (160 . "#83FF3F")
                             (180 . "#56FF3F")
                             (200 . "#3FFF56")
                             (220 . "#3FFF83")
                             (240 . "#3FFFB0")
                             (260 . "#3FFFDD")
                             (280 . "#3FF3FF")
                             (300 . "#3FC6FF")
                             (320 . "#3F99FF")
                             (340 . "#3F6CFF")
                             (360 . "MediumOrchid2"))) ;"#3F3FFF"
   `(vc-annotate-very-old-color "DarkOrchid2")

   ;; popup box of treesitter-context package
   `(treesitter-context-background-color "#000000")
   `(treesitter-context-border-color "#337744")

   ;; pos-tip. Helper package for tooltip
   `(pos-tip-foreground-color ,fg-green)
   `(pos-tip-background-color ,bg-green))

  (custom-theme-set-faces
   'charcoal

   `(default ((,class (:foreground ,fg :background ,bg))))

   `(cursor ((,class (:background "spring green"))))

   `(show-paren-match ((,class (:slant italic
                                       :bold t
                                       :strike-through t
                                       ;; :background nil
                                       ))))

   ;; line that separates vertically split windows.
   `(vertical-border ((,class (:foreground ,(if (display-graphic-p)
                                               "gray25"
                                             "gray30")))))


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

   `(line-number ((,class ,@(cond ((= charcoal-color-cnt 16)
                                     `(:inherit mode-line))
                                  (t `(:background ,linenum-bg
                                       :foreground ,linenum-fg
                                       ))))))
   `(line-number-current-line ((,class (:inherit line-number
                                                 :foreground ,linenumcur-fg
                                                 :background ,linenumcur-bg
                                                 ))))

   `(region
     ((,class (:background ,highlight))))     ;69685E

   ;; TODO
   `(fringe ((,class (:background "black"))))

   ;; TODO font lock
   ;; `(font-lock-builtin-face ((,class (:foreground ,zenburn-fg :weight bold))))
   `(font-lock-comment-face ((,class (:foreground "#8FB28F"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "medium spring green"))))
   `(font-lock-constant-face ((,class :foreground ,fg
              :background "#232319"
              :weight bold)))
   `(font-lock-doc-face ((,class (:foreground "darkolivegreen3"))))
   `(font-lock-function-name-face ((,class :foreground ,fn-def-fg ;;"pale turquoise"
                                           :background ,fn-def-bg ;;"black"
										   :weight normal
										   ;; :box (:line-width -1 :color ,faint-less)
                                           )))
   ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ;; ~ START faces supported with treesit
   ;; ~ Maybe not intended to be treesit only, but it seems that way.
   `(font-lock-function-call-face ((,class :foreground ,fn-call-fg
                                           :background ,fn-call-bg)))
   `(font-lock-variable-use-face ((,class :inherit default)))
   `(font-lock-operator-face ((,class :inherit font-lock-keyword-face
                                      :weight normal)))
   `(font-lock-property-name-face ((,class
                                    ;; :inherit font-lock-variable-name-face
                                    :foreground "burlywood3"
                                    ;; :background ,fn-call-bg
                                    )))
   `(font-lock-property-use-face ((,class
                                   :inherit font-lock-property-name-face
                                   ;; :slant italic
                                   )))
   ;; `(font-lock-punctuation-face ((,class nil)))
   ;; `(font-lock-bracket-face)
   ;; `(font-lock-delimiter-face)
   `(font-lock-escape-face ((,class :foreground "hot pink")))
   ;; `(font-lock-misc-punctuation-face)
   `(font-lock-number-face ((,class :foreground "#50b5b5" ;"#BEBEA1"
                                    ;; :background ,fn-def-bg
                                    )))
   ;; `(font-lock-regexp-face)
   ;; ~ END treesit faces
   ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   `(font-lock-keyword-face ((,class (:foreground ,keyword :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground "hot pink" :weight bold))))
   ;; `(font-lock-preprocessor-face ((,class (:foreground ,zenburn-blue+1))))
   ;; `(font-lock-regexp-grouping-construct ((,class (:foreground ,zenburn-yellow :weight bold))))
   ;; `(font-lock-regexp-grouping-backslash ((,class (:foreground ,zenburn-green :weight bold))))
   `(font-lock-string-face ((,class (:foreground "LightSalmon"))))
   ;; `(font-lock-type-face ((,class (:foreground ,zenburn-blue-1))))
   `(font-lock-variable-name-face ((,class (:foreground
                                            ;; ,fg
                                            ;; ,fn-def-fg
                                            ;; "burlywood3"
                                            ;; "#BBAA99"
                                            ,var
                                            ;; :background ,bg ;;"#202020"
                                                        ))))
   ;; `(font-lock-warning-face ((,class (:foreground ,zenburn-yellow-2 :weight bold))))

   `(minibuffer-prompt ((,class (:foreground ,fg-purple))))

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
   `(highlight-indent-guides-character-face ((,class (:foreground ,faint))))

   ;; default emacs completion.
   `(completions-common-part ((,class (:foreground ,faint-less))))
   `(completions-first-difference ((,class (:foreground ,fg-green))))

   ;; swiper, ivy, counsel
   `(swiper-line-face ((,class (:background ,(if (= charcoal-color-cnt 16)
												 highlight
											   "gray30")
											,@(cond ((= charcoal-color-cnt 16)
													 `(:foreground ,fg))
													(t nil))))))
   ;; face-1 fills in the space between matches. 2-4 are for matches.
   `(swiper-match-face-1 ((,class (:foreground ,faint-less :background "black"))))
   `(swiper-match-face-2 ((,class (:foreground ,rain-1 :background "black"))))
   ;; NOTE: face-3, 4 don't work when out-of-order matching is used.
   ;; TODO: make bug report to swiper about face 3,4
   `(swiper-match-face-3 ((,class (:foreground ,rain-2 :background "black"))))
   `(swiper-match-face-4 ((,class (:foreground ,rain-3 :background "black"))))
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
   ;; `(ivy-virtual ((,class (:foreground "white"))))

   ;; ace-window
   `(aw-leading-char-face ; ace-window character.
     ((,class (:foreground ,fg-green
						   ;; :background "black"
						   :height 300)))) ; big font
   `(aw-background-face ((,class (:foreground ,faint-less))))

   ;; ido
   `(ido-first-match ((,class (:foreground ,fg-yellow :background "#000000";,bg-yellow
                                           ))))
   `(ido-only-match ((,class (:foreground ,fg-green :background ,bg-green))))
   `(ido-subdir ((,class (:foreground ,fg-purple :background ,bg-purple))))

   ;; ido-grid
   `(ido-grid-common-match ((,class :foreground ,fg-green)))
   ;; `(ido-grid-match ((,class :foreground "pink")))
   `(ido-grid-match-1 ((,class :inherit completions-common-part)))
   ;; `(ido-grid-match-2 ((,class :foreground "purple" :background "yellow")))
   ;; `(ido-grid-match-3 ((,class :foreground "black" :background "white")))


   ;; isearch
   `(isearch ((,class (:background "orange";,isearch-bg
								   :foreground "black";,isearch-fg
								   :weight bold
								   :underline nil))))
   ;; the non-selected matches from isearch
   `(lazy-highlight ((,class (:background ,isearch-fg ;,bg-purple
										  :foreground ,isearch-bg ;,fg-purple
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
   `(magit-reflog-checkout ((,class (:foreground "orangered"))))
   `(magit-section-highlight ((,class (:background ,faint))))

   ;; tooltip
   `(tooltip ((,class (:background ,popup-bg :foreground ,fg))))

   ;; js2
   `(js2-function-call ((,class :foreground ,fn-call-fg
                                :background ,fn-call-bg)))
   `(js2-object-property ((,class :inherit font-lock-variable-name-face
                                  :foreground "burlywood3"
                                  :background ,fn-call-bg)))
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

   ;; eglot
   `(eglot-highlight-symbol-face ((,class (:foreground ,fg-purple :background ,bg-purple
                                                       ;; :weight bold
                                                       ;; :box (:line-width -1 :color "purple")
                                                       ))))

   ;; web-mode
   `(web-mode-current-element-highlight-face ((,class (:background ,faint))))
   `(web-mode-current-column-highlight-face ((,class (:background ,faint))))
   `(web-mode-function-call-face ((,class :foreground ,fn-call-fg
                                          :background ,fn-call-bg)))

   ;; company
   `(company-tooltip ((,class (:background ,popup-bg :foreground ,fg))))
   `(company-tooltip-scrollbar-track ((,class (:background ,scrollb-bg))))
   `(company-tooltip-scrollbar-thumb ((,class (:background ,scrollb-fg))))
   ;; obsolete company scrollbar faces. Keep them in case of older company versions
   `(company-scrollbar-bg ((,class (:inherit company-tooltip-scrollbar-track))))
   `(company-scrollbar-fg ((,class (:inherit company-tooltip-scrollbar-thumb))))

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


   ;; inherited by face hl-line in hl-line-mode
   `(highlight ((,class :background ,faint)))

   ;; hl-line. normally inheris highlight face, but override.
   `(hl-line ((,class :background ,fainter)))

   ;; ediff. temporarily borrowing from zenburn.
   ;; TODO: customize colors for charcoal
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


   ;; helm. temporarily borrowing from zenburn.
   `(helm-header
     ((t (:foreground ,zenburn-green
                      :background ,zenburn-bg
                      :underline nil
                      :box nil
                      :extend t))))
   `(helm-source-header
     ((t (:foreground ,zenburn-yellow
                      :background ,zenburn-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)
                      :extend t))))
   `(helm-selection ((t (:background ,zenburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,zenburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,zenburn-bg :background ,zenburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,zenburn-green+4 :background ,zenburn-bg-1))))
   `(helm-separator ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-time-zone-current ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(helm-time-zone-home ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,zenburn-orange :background ,zenburn-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,zenburn-magenta :background ,zenburn-bg))))
   `(helm-bookmark-info ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(helm-bookmark-man ((t (:foreground ,zenburn-yellow :background ,zenburn-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,zenburn-magenta :background ,zenburn-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-buffer-process ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(helm-buffer-size ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
   `(helm-ff-directory ((t (:foreground ,zenburn-cyan :background ,zenburn-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,zenburn-fg :background ,zenburn-bg :weight normal))))
   `(helm-ff-file-extension ((t (:foreground ,zenburn-fg :background ,zenburn-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,zenburn-red :background ,zenburn-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,zenburn-yellow :background ,zenburn-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,zenburn-bg :background ,zenburn-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))
   `(helm-grep-file ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(helm-grep-finish ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(helm-grep-lineno ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-match ((t (:foreground ,zenburn-orange :background ,zenburn-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))


   ;; smerge-mode
   `(smerge-base ((,class (:background "#404000"))))

   ;; markdown-mode
   `(markdown-code-face ((,class (:inherit font-lock-constant-face)))) ; avoid font change.

   ;; used in adoc-mode
   '(markup-meta-face ((t (:foreground "yellow green"))))
   '(markup-internal-reference-face ((t (:inherit markup-reference-face))))
   '(markup-meta-hide-face ((t (:foreground "powder blue"))))

   ;; org
   `(org-hide ((,class (:foreground ,faint))))

   ;; Emacs built-in display-fill-column-indicator.
   ;; replaces `fci-mode' and it's color variable `fci-rule-color'
   `(fill-column-indicator ((,class :foreground ,faint)))

   ;; ert
   `(ert-test-result-expected ((,class :foreground ,fg-green :background ,bg-green)))
   `(ert-test-result-unexpected ((,class :foreground ,fg-red :background ,bg-red)))

   ;; leerzeichen
   `(leerzeichen ((t (:foreground ,faint-less)))) ;;"yellow4" ;"#A8A800"

   ;; package
   `(package-status-new ((,class :weight bold
                                 :background ,bg-green
                                 :foreground ,fg-green)))

   ;; tree-sitter-hl
   `(tree-sitter-hl-face:function.call
     ((,class :foreground ,fn-call-fg
              :background ,fn-call-bg)))
   `(tree-sitter-hl-face:operator
     ((,class :inherit font-lock-keyword-face
              :weight normal)))
   `(tree-sitter-hl-face:label
     ((,class :foreground ,fg
              :background "#232319"
              :weight bold)))

   ;; menu bar faces. Only relevant in terminal mode?
   `(menu ((,class :background ,faint-less :foreground "black")))
   `(tty-menu-enabled-face ((,class :background "black")))
   `(tty-menu-disabled-face ((,class :background "black" :foreground "red")))
   `(tty-menu-selected-face ((,class :background ,(if (<= charcoal-color-cnt 256)
                                                      "blue"
                                                    fainter))))

   ;; swift-mode
   `(swift-mode:function-call-face ((,class :foreground ,fn-call-fg
                                            :background ,fn-call-bg)))


   ;; profile-dotemacs.el
   ;; `(profile-dotemacs-time-face ((,class :background "OrangeRed1")))
   `(profile-dotemacs-low-percentage-face ((,class :foreground ,faint)))
   ;; `(profile-dotemacs-highlight-face ((,class :background "blue")))

   ;; klondike solitaire
   `(klondike-heart-diamond-color ((,class :foreground "orange")))
   `(klondike-stack-numbering ((,class :foreground "green")))
   `(klondike-stack-selecting ((,class :foreground "red" :background "black")))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,class (:foreground ,rain-1))))
   `(rainbow-delimiters-depth-2-face
     ((,class (:foreground ,rain-2))))
   `(rainbow-delimiters-depth-3-face
     ((,class (:foreground ,rain-3))))
   `(rainbow-delimiters-depth-4-face
     ((,class (:foreground ,rain-4))))
   `(rainbow-delimiters-depth-5-face
     ((,class (:foreground ,rain-5))))
   `(rainbow-delimiters-depth-6-face
     ((,class (:foreground ,rain-6))))
   `(rainbow-delimiters-depth-7-face
     ((,class (:foreground ,rain-7))))
   `(rainbow-delimiters-depth-8-face
     ((,class (:foreground ,rain-8 :background ,rain-8-bg))))
   `(rainbow-delimiters-depth-9-face
     ((,class (:foreground ,rain-9))))
   `(rainbow-delimiters-unmatched-face
     ((,class (:foreground ,rain-fg-u :background ,rain-bg-u))))))

(provide-theme 'charcoal)

;;; charcoal-theme.el ends here
