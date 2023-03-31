;;; sv-theme.el --- set visual theme in the only unique way

;; Copyright  2022 Mitchell Marquez

;; Author: Mitchell Marquez <dr.m.perseos@gmail.com>
;; Version: 0.8
;; Package-Requires: ((autothemer))
;; Keywords: theme, colorscheme, high-contrast, dark theme
;; URL: https://git.mitchmarq42.xyz/mitch/vimcolors

;;; Commentary:
;; When I first started using `nvim', I copied my `nano' config which was very
;; cringe and basic.  And so I scoured the internet for themes.  And discovered
;; something truly disturbing: Every single `nvim' and `emacs' theme looks
;; exactly the same, and they're all gray-on-gray cringe.  I prefer my cringe to
;; really pop rather than ooze before my eyes, so I somehow found the `nvim'
;; theme that gradually became this.

;;; Code:
(require 'autothemer)

;; set font... but actually don't
;; Taken from https://web.archive.org/web/20210622224446/https://www.manueluberti.eu/emacs/2017/02/26/dynamicfonts/

(autothemer-deftheme
 sv "Based on my nvim theme. Because everything else looks the same."

 ;; Specify the color classes used by the theme
 ((
   ((class color) (min-colors #xFFFFFF)) ;; truecolor
   ((class color) (min-colors #xFF    )) ;; 256color
   ((class color) (min-colors 16 )) ;; 16color
   ((class color) (min-colors 8 )) ;; 8color
   )

  ;; Specify the color palette for each of the classes above.
  (sv-fg      "PaleGoldenrod")
  (sv-black   "black" 'nil)
  (sv-red     "red" )
  (sv-green   "ForestGreen")
  (sv-yellow  "gold1" "white")
  (sv-blue    "blue" )
  (sv-magenta "magenta")
  (sv-cyan    "cyan")
  (sv-white   "white")
  (sv-pink    "pink2")
  (sv-light-black   "grey19" "grey19" "gray")
  (sv-light-red     "orange")
  (sv-light-green   "green3")
  (sv-light-yellow  "PaleGoldenrod")
  (sv-light-blue    "RoyalBlue")
  (sv-light-magenta "DarkMagenta")
  (sv-light-cyan    "turquoise3" "lightseagreen")
  (sv-light-white   "grey69")
  (sv-visual-bg     "DarkBlue" "midnightblue")
  (sv-dark-gray     "grey7")
  (sv-dark-yellow   "DarkGoldenrod1")
  (sv-dark-red      "DarkRed")
  (sv-mid-gray      "grey33")
  (sv-mid-violet    "BlueViolet")
  (sv-mid-red       "red3"))

 ;; specifications for Emacs faces.
 (
  (default (:background sv-black :foreground sv-fg))
  ;; (default (:background sv-black :foreground sv-white))
  ;; (default (:slant 'normal))
  (bold (:weight 'bold :foreground sv-white))
  (tabulated-list-fake-header (:inherit 'bold :underline 't :overline 't))
  (cursor (:inverse-video t))
  (highlight (:background sv-visual-bg))
  (region (:inherit 'highlight))
  (link (:foreground sv-light-blue :underline 't))
  (link-visited (:foreground sv-mid-violet :underline 't))
  (whitespace-line (:background sv-dark-red))
  (line-number (:background 'unspecified :inherit 'font-lock-comment-delimiter-face))
  (line-number-current-line (:inherit
			     'line-number
			     :foreground sv-yellow
			     :weight 'bold))
  (linum (:inherit 'line-number))
  (linum-relative-current-face (:inherit 'line-number-current-line))
  (nlinum-relative-current-face (:inherit 'line-number-current-line))
  (ivy-current-match (:inherit 'highlight :extend t))
  (ivy-separator (:inherit 'marginalia-documentation))
  (marginalia-modified (:foreground sv-light-magenta))
  (show-paren-match (:background sv-light-cyan))
  ;; font-lock faces. The defaults that are important to get right.
  (font-lock-comment-face (:foreground sv-green :slant 'oblique))
  (font-lock-comment-delimiter-face (:foreground sv-light-black))
  (font-lock-constant-face (:foreground sv-light-white :weight 'normal))
  (font-lock-string-face (:foreground sv-light-blue :slant 'italic))
  (font-lock-builtin-face (:foreground sv-light-white :weight 'bold))
  (font-lock-keyword-face (:foreground sv-mid-red :weight 'normal))
  (font-lock-type-face (:foreground sv-light-red :weight 'bold))
  (font-lock-function-name-face (:foreground sv-red :weight 'normal))
  (font-lock-variable-name-face (:foreground sv-light-cyan :weight 'normal))
  (font-lock-negation-char-face (:foreground sv-visual-bg :weight 'bold))
  ;; other things
  (transient-heading (:inherit 'default
			       :foreground sv-magenta
			       :weight 'bold))
  (rainbow-delimiters-depth-1-face (:foreground sv-light-magenta))
  (rainbow-delimiters-depth-2-face (:foreground sv-magenta :weight 'normal))
  (vertical-border (:foreground sv-mid-violet :weight 'bold))
  (fringe (:inherit 'default))
  (yascroll:thumb-fringe (:foreground sv-green :background sv-green))
  (yascroll:thumb-text-area (:foreground sv-green :background sv-green))
  (whitespace-space (:foreground sv-black))
  (whitespace-tab (:foreground sv-black))
  (whitespace-newline (:foreground sv-black))
  (org-meta-line (:inherit 'font-lock-comment-delimiter-face))
  (org-document-title (:foreground sv-light-cyan :weight 'bold :height 1.1))
  (org-block (:background sv-dark-gray :extend t :inherit 'fixed-pitch))
  (outline-1 (:foreground sv-red :weight 'bold :height 1.1)) ; :slant 'italic
  (outline-2 (:foreground sv-red :weight 'bold :height 1.1)) ; :slant 'italic
  (outline-3 (:foreground sv-red :weight 'bold :height 1.1)) ; :slant 'italic
  (outline-4 (:foreground sv-red :weight 'bold :height 1.1)) ; :slant 'italic
  (outline-5 (:foreground sv-red :weight 'bold :height 1.1)) ; :slant 'italic
  (outline-6 (:foreground sv-red :weight 'bold :height 1.1)) ; :slant 'italic
  (outline-7 (:foreground sv-red :weight 'bold :height 1.1)) ; :slant 'italic
  (outline-8 (:foreground sv-red :weight 'bold :height 1.1)) ; :slant 'italic
  (outline-9 (:foreground sv-red :weight 'bold :height 1.1)) ; :slant 'italic
  (helpful-heading (:foreground sv-white :height 1.1 :weight 'bold))
  (isearch (:foreground sv-dark-yellow
			:background sv-light-magenta
			:weight 'bold))
  (lazy-highlight (:inherit 'isearch))
  (completions-highlight (:background sv-mid-violet))
  (corfu-default (:background sv-mid-gray))
  (corfu-current (:inherit 'completions-highlight))
  ;; ansi colors
  (ansi-color-black (:foreground sv-black :background sv-light-black))
  (ansi-color-red (:foreground sv-red :background sv-light-red))
  (ansi-color-green (:foreground sv-green :background sv-light-green))
  (ansi-color-yellow (:foreground sv-yellow :background sv-light-yellow))
  (ansi-color-blue (:foreground sv-blue :background sv-light-blue))
  (ansi-color-magenta (:background sv-magenta
				   :foreground sv-mid-violet))
  (ansi-color-cyan (:foreground sv-cyan :background sv-light-cyan))
  (ansi-color-white (:foreground sv-white :background sv-light-white))
  (ansi-color-bright-black (:background sv-black
					:foreground sv-light-black))
  (ansi-color-bright-red (:background sv-red :foreground sv-light-red))
  (ansi-color-bright-green (:background sv-green
					:foreground sv-light-green))
  (ansi-color-bright-yellow (:background sv-yellow
					 :foreground sv-light-yellow))
  (ansi-color-bright-blue (:background sv-blue :foreground sv-light-blue))
  (ansi-color-bright-magenta (:foreground sv-magenta
					  :background sv-mid-violet))
  (ansi-color-bright-cyan (:background sv-cyan :foreground sv-light-cyan))
  (ansi-color-bright-white (:background sv-white
					:foreground sv-light-white))
  (eat-term-bold (:foreground 'unspecified :weight 'bold))
  ;; eshell colors
  (epe-dir-face (:foreground sv-cyan))
  (eshell-syntax-highlighting-shell-command-face (:foreground
						  sv-light-green))
  (eshell-syntax-highlighting-lisp-function-face (:foreground
						  sv-light-green))
  (eshell-syntax-highlighting-alias-face (:foreground
					  sv-light-green))
  (eshell-syntax-highlighting-invalid-face (:weight 'bold
						    :foreground sv-red))
  (epe-status-face (:inherit 'font-lock-constant-face))
  (eshell-syntax-highlighting-file-arg-face (:foreground
					     sv-magenta
					     :underline 't))
  ;; magit colors
  (magit-section-heading (:inherit 'outline-1))
  (magit-diff-file-heading (:foreground sv-cyan :weight 'bold))
  (magit-branch-remote (:foreground sv-green :weight 'bold))
  (magit-branch-local (:foreground sv-cyan))
  )
 )

(provide-theme 'sv)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))
;;; sv-theme.el ends here
