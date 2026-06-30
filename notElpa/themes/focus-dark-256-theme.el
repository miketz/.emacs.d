;;; focus-dark-256-theme.el --- Minimal theme -*- lexical-binding: t -*-

;;; Commentary:
;;; Based on ideas from https://tonsky.me/blog/syntax-highlighting/
;;; If everything is highlighted, nothing is highlighted.
;;;
;;; DO highlight
;;; function defs
;;; var defs (not var use)
;;; literal values (num, string), constants
;;; comments
;;;
;;; DON'T highlight
;;; keywords: return, class, if etc
;;;
;;; dim
;;; {}
;;; package namespace preciding Thing: foo.bar.baz.Thing
;;;
;;; For 256 color display.
;;;
;;; if light theme, use background highlights more to overcome contrast issues

;;; Code:
(deftheme focus-dark-256 "focus-dark-256 color theme")


(let* (;; 256 color pallete
       ;; 0-15 ANSI colors and bright variants
       (black "#000000")
       (red "#cd0000")
       (green "#00cd00")
       (yellow "#cdcd00")
       (blue "#0000ee")
       (magenta "#cd00cd")
       (cyan "#00cdcd")
       (white "#e5e5e5")
       (brightblack "#7f7f7f")
       (brightred "#ff0000")
       (brightgreen "#00ff00")
       (brightyellow "#ffff00")
       (brightblue "#5c5cff")
       (brightmagenta "#ff00ff")
       (brightcyan "#00ffff")
       (brightwhite "#ffffff")
       ;; 16-231 6x6x6 rgb color cube
       (color-16 "#000000")
       (color-17 "#00005f")
       (color-18 "#000087")
       (color-19 "#0000af")
       (color-20 "#0000d7")
       (color-21 "#0000ff")
       (color-22 "#005f00")
       (color-23 "#005f5f")
       (color-24 "#005f87")
       (color-25 "#005faf")
       (color-26 "#005fd7")
       (color-27 "#005fff")
       (color-28 "#008700")
       (color-29 "#00875f")
       (color-30 "#008787")
       (color-31 "#0087af")
       (color-32 "#0087d7")
       (color-33 "#0087ff")
       (color-34 "#00af00")
       (color-35 "#00af5f")
       (color-36 "#00af87")
       (color-37 "#00afaf")
       (color-38 "#00afd7")
       (color-39 "#00afff")
       (color-40 "#00d700")
       (color-41 "#00d75f")
       (color-42 "#00d787")
       (color-43 "#00d7af")
       (color-44 "#00d7d7")
       (color-45 "#00d7ff")
       (color-46 "#00ff00")
       (color-47 "#00ff5f")
       (color-48 "#00ff87")
       (color-49 "#00ffaf")
       (color-50 "#00ffd7")
       (color-51 "#00ffff")
       (color-52 "#5f0000")
       (color-53 "#5f005f")
       (color-54 "#5f0087")
       (color-55 "#5f00af")
       (color-56 "#5f00d7")
       (color-57 "#5f00ff")
       (color-58 "#5f5f00")
       (color-59 "#5f5f5f")
       (color-60 "#5f5f87")
       (color-61 "#5f5faf")
       (color-62 "#5f5fd7")
       (color-63 "#5f5fff")
       (color-64 "#5f8700")
       (color-65 "#5f875f")
       (color-66 "#5f8787")
       (color-67 "#5f87af")
       (color-68 "#5f87d7")
       (color-69 "#5f87ff")
       (color-70 "#5faf00")
       (color-71 "#5faf5f")
       (color-72 "#5faf87")
       (color-73 "#5fafaf")
       (color-74 "#5fafd7")
       (color-75 "#5fafff")
       (color-76 "#5fd700")
       (color-77 "#5fd75f")
       (color-78 "#5fd787")
       (color-79 "#5fd7af")
       (color-80 "#5fd7d7")
       (color-81 "#5fd7ff")
       (color-82 "#5fff00")
       (color-83 "#5fff5f")
       (color-84 "#5fff87")
       (color-85 "#5fffaf")
       (color-86 "#5fffd7")
       (color-87 "#5fffff")
       (color-88 "#870000")
       (color-89 "#87005f")
       (color-90 "#870087")
       (color-91 "#8700af")
       (color-92 "#8700d7")
       (color-93 "#8700ff")
       (color-94 "#875f00")
       (color-95 "#875f5f")
       (color-96 "#875f87")
       (color-97 "#875faf")
       (color-98 "#875fd7")
       (color-99 "#875fff")
       (color-100 "#878700")
       (color-101 "#87875f")
       (color-102 "#878787")
       (color-103 "#8787af")
       (color-104 "#8787d7")
       (color-105 "#8787ff")
       (color-106 "#87af00")
       (color-107 "#87af5f")
       (color-108 "#87af87")
       (color-109 "#87afaf")
       (color-110 "#87afd7")
       (color-111 "#87afff")
       (color-112 "#87d700")
       (color-113 "#87d75f")
       (color-114 "#87d787")
       (color-115 "#87d7af")
       (color-116 "#87d7d7")
       (color-117 "#87d7ff")
       (color-118 "#87ff00")
       (color-119 "#87ff5f")
       (color-120 "#87ff87")
       (color-121 "#87ffaf")
       (color-122 "#87ffd7")
       (color-123 "#87ffff")
       (color-124 "#af0000")
       (color-125 "#af005f")
       (color-126 "#af0087")
       (color-127 "#af00af")
       (color-128 "#af00d7")
       (color-129 "#af00ff")
       (color-130 "#af5f00")
       (color-131 "#af5f5f")
       (color-132 "#af5f87")
       (color-133 "#af5faf")
       (color-134 "#af5fd7")
       (color-135 "#af5fff")
       (color-136 "#af8700")
       (color-137 "#af875f")
       (color-138 "#af8787")
       (color-139 "#af87af")
       (color-140 "#af87d7")
       (color-141 "#af87ff")
       (color-142 "#afaf00")
       (color-143 "#afaf5f")
       (color-144 "#afaf87")
       (color-145 "#afafaf")
       (color-146 "#afafd7")
       (color-147 "#afafff")
       (color-148 "#afd700")
       (color-149 "#afd75f")
       (color-150 "#afd787")
       (color-151 "#afd7af")
       (color-152 "#afd7d7")
       (color-153 "#afd7ff")
       (color-154 "#afff00")
       (color-155 "#afff5f")
       (color-156 "#afff87")
       (color-157 "#afffaf")
       (color-158 "#afffd7")
       (color-159 "#afffff")
       (color-160 "#d70000")
       (color-161 "#d7005f")
       (color-162 "#d70087")
       (color-163 "#d700af")
       (color-164 "#d700d7")
       (color-165 "#d700ff")
       (color-166 "#d75f00")
       (color-167 "#d75f5f")
       (color-168 "#d75f87")
       (color-169 "#d75faf")
       (color-170 "#d75fd7")
       (color-171 "#d75fff")
       (color-172 "#d78700")
       (color-173 "#d7875f")
       (color-174 "#d78787")
       (color-175 "#d787af")
       (color-176 "#d787d7")
       (color-177 "#d787ff")
       (color-178 "#d7af00")
       (color-179 "#d7af5f")
       (color-180 "#d7af87")
       (color-181 "#d7afaf")
       (color-182 "#d7afd7")
       (color-183 "#d7afff")
       (color-184 "#d7d700")
       (color-185 "#d7d75f")
       (color-186 "#d7d787")
       (color-187 "#d7d7af")
       (color-188 "#d7d7d7")
       (color-189 "#d7d7ff")
       (color-190 "#d7ff00")
       (color-191 "#d7ff5f")
       (color-192 "#d7ff87")
       (color-193 "#d7ffaf")
       (color-194 "#d7ffd7")
       (color-195 "#d7ffff")
       (color-196 "#ff0000")
       (color-197 "#ff005f")
       (color-198 "#ff0087")
       (color-199 "#ff00af")
       (color-200 "#ff00d7")
       (color-201 "#ff00ff")
       (color-202 "#ff5f00")
       (color-203 "#ff5f5f")
       (color-204 "#ff5f87")
       (color-205 "#ff5faf")
       (color-206 "#ff5fd7")
       (color-207 "#ff5fff")
       (color-208 "#ff8700")
       (color-209 "#ff875f")
       (color-210 "#ff8787")
       (color-211 "#ff87af")
       (color-212 "#ff87d7")
       (color-213 "#ff87ff")
       (color-214 "#ffaf00")
       (color-215 "#ffaf5f")
       (color-216 "#ffaf87")
       (color-217 "#ffafaf")
       (color-218 "#ffafd7")
       (color-219 "#ffafff")
       (color-220 "#ffd700")
       (color-221 "#ffd75f")
       (color-222 "#ffd787")
       (color-223 "#ffd7af")
       (color-224 "#ffd7d7")
       (color-225 "#ffd7ff")
       (color-226 "#ffff00")
       (color-227 "#ffff5f")
       (color-228 "#ffff87")
       (color-229 "#ffffaf")
       (color-230 "#ffffd7")
       (color-231 "#ffffff")
       ;; 232-255 grayscale ramp
       (color-232 "#080808")
       (color-233 "#121212")
       (color-234 "#1c1c1c")
       (color-235 "#262626")
       (color-236 "#303030")
       (color-237 "#3a3a3a")
       (color-238 "#444444")
       (color-239 "#4e4e4e")
       (color-240 "#585858")
       (color-241 "#626262")
       (color-242 "#6c6c6c")
       (color-243 "#767676")
       (color-244 "#808080")
       (color-245 "#8a8a8a")
       (color-246 "#949494")
       (color-247 "#9e9e9e")
       (color-248 "#a8a8a8")
       (color-249 "#b2b2b2")
       (color-250 "#bcbcbc")
       (color-251 "#c6c6c6")
       (color-252 "#d0d0d0")
       (color-253 "#dadada")
       (color-254 "#e4e4e4")
       (color-255 "#eeeeee")
       ;; vars for this theme. use the 256 color pallete
       (class t)
       (fg color-254);"#EEEED1"
       (bg-2 color-234)
       (bg-1 color-235)
       (bg color-236) ;"#35352B"
       (bg+1 color-237)
       (bg+2 color-238)
       (dim+4 color-251) ;"#CECEB1"
       (dim+3 color-250) ;"#BEBEA1"
       (dim+2 color-248) ;"#AEAE91"
       (dim+1 color-247) ;"#9E9E81"
       (dim color-245) ;"#8E8E71"
       (dim-1 color-244) ;"#7E7E61"
       (dim-2 color-242) ;"#6E6E51"
       (dim-3 color-240) ;"#5E5E41"
       (dim-4 color-239) ;"#4E4E31"
       (rain-1 color-203) ;"#FF4500"
       (rain-1-bg bg)
       (rain-2 color-51) ;"#00FFFF"
       (rain-2-bg bg)
       (rain-3 brightyellow) ;"#FFFF00"
       (rain-3-bg bg)
       (rain-4 color-177) ;"#DDA0DD"
       (rain-4-bg bg)
       (rain-5 color-118) ;"#7CFC00"
       (rain-5-bg bg)
       (rain-6 color-214) ;"#FFA500"
       (rain-6-bg bg)
       (rain-7 color-231) ;"#FFFFFF"
       (rain-7-bg bg)
       (rain-8 color-205) ;"#FF69B4"
       (rain-8-bg color-233) ;"#101010"
       (rain-9 color-180) ;"#CDAA7D"
       (rain-9-bg bg)
       (rain-fg-u color-130) ;"#A0522D"
       (rain-bg-u black)) ;"#000000"

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Vars
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (custom-theme-set-variables
   'focus-dark-256
   ;; indent-bars
   ;; This is hard to configure in a considerate way in a theme. Becuase
   ;; `indent-bars-color-by-depth' mixes several things in 1 var. A boolean
   ;; feature toggle (ie non-nil), color settings, and blend.
   ;; since this is my personal theme, just do it how i want it.
   `(indent-bars-color-by-depth nil)
   `(indent-bars-color '(,brightwhite :face-bg nil :blend 0.075))

   `(evil-emacs-state-cursor    '(bar ,red))
   `(evil-normal-state-cursor   '(hollow ,color-84))
   `(evil-insert-state-cursor   '(bar ,color-84))
   `(evil-visual-state-cursor   '(hollow ,color-208))
   `(evil-operator-state-cursor '(box ,red))
   `(evil-replace-state-cursor  '(hbar ,color-203))
   `(evil-motion-state-cursor   '(box ,color-84)))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Faces
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (custom-theme-set-faces
   'focus-dark-256

   `(default ((,class :foreground ,fg :background ,bg)))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; highlights
   `(font-lock-function-name-face ((,class :foreground ,color-159 :background ,black :weight normal)))
   `(font-lock-variable-name-face ((,class :foreground ,color-79;"#66CDAA"
                                           )))
   `(js2-function-param ((,class :inherit font-lock-variable-name-face)))

   `(font-lock-comment-face ((,class :foreground ,color-71;"#6FAB6F"
                                     :background ,bg+1;"#393f2f"
                                     )))
   `(font-lock-comment-delimiter-face ((,class :inherit font-lock-comment-face)))
   `(font-lock-doc-face ((,class :inherit font-lock-comment-face)))
   `(font-lock-doc-markup-face ((,class :inherit font-lock-doc-face :weight bold)))
   `(font-lock-string-face ((,class :foreground ,color-216;"#ffa07a"
                                    :background ,bg+1;"#45453B"
                                    )))
   ;; `(font-lock-builtin-face ((,class (:foreground "dark blue" :background ,bg :weight normal))))
   `(font-lock-constant-face ((,class :foreground ,fg :weight normal)))
   `(font-lock-number-face ((,class :foreground ,color-73;"#50b5b5"
                                    ))) ;treesit

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; disable highlights. use regular fg/bg
   `(font-lock-keyword-face ((,class :foreground ,dim-2)))
   `(font-lock-type-face ((,class :foreground ,dim+2)))
   `(font-lock-function-call-face ((,class :foreground ,color-174))) ;treesit
   `(font-lock-variable-use-face ((,class :inherit default))) ;treesit
   `(font-lock-operator-face ((,class :inherit font-lock-keyword-face :foreground ,dim+4))) ;treesit
   `(font-lock-property-name-face ((,class :foreground ,fg))) ;treesit
   `(font-lock-property-use-face ((,class :inherit font-lock-property-name-face))) ;treesit
   `(font-lock-delimiter-face ((,class :foreground ,dim+1))) ;treesit
   `(font-lock-escape-face ((,class :inherit font-lock-string-face
                                    :foreground ,color-202;"#FF6F00"
                                    ))) ;treesit
   `(font-lock-regexp-face ((,class :foreground ,fg))) ;treesit
   `(font-lock-negation-char-face ((,class (:foreground ,color-203;"#ff6347"
                                                        :weight bold))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; dim. don't dim for now actually as the face control is not fine grained enough.
   `(font-lock-bracket-face ((,class :foreground ,dim))) ;treesit
   `(font-lock-punctuation-face ((,class :foreground ,fg))) ;treesit
   `(font-lock-misc-punctuation-face ((,class :foreground ,fg))) ;treesit



   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; misc. correct things that don't look right

   ;; rainbow-delimiters.
   ;; In theory we don't want to highlight delimiters in this theme. But if
   ;; you're using rainbow-delimiters mode that means you do want them highlighted.
   ;; May as well brighten them up. Turn off rainbow-delimiters-mode to remove color.
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,rain-1)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,rain-2)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,rain-3)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,rain-4)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,rain-5)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,rain-6)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,rain-7)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,rain-8 :background ,rain-8-bg)))
   `(rainbow-delimiters-depth-9-face ((,class :foreground ,rain-9)))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,rain-fg-u :background ,rain-bg-u)))

   ;; mode line
   `(mode-line
     ((,class (:foreground "#8FB28F"
                           :background "#2B2B2B"
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((,class (:foreground "#F0DFAF"
                                               :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground "#5F7F5F"
                           :background "#383838"
                           :box (:line-width -1 :style released-button)))))

   ;; num3
   `(num3-face-even ((,class (:background "#000000" :foreground "#98FB98"))))

   ;; display-line-numbers. native implementation
   `(line-number ((,class (:background "#231808" :foreground "#595959"))))
   `(line-number-current-line ((,class (:inherit line-number
                                                 :foreground "#77AA55"
                                                 :background "#231808"))))
   `(line-number-major-tick ((,class :foreground "#595959" :background "#3b0000")))
   `(line-number-minor-tick ((,class :foreground "#595959" :background "black")))

   ;; faces.el
   `(show-paren-match ((,class (:slant italic
                                       :weight bold
                                       :strike-through t))))

   ;; tab-line.  like web browser tabs.
   `(tab-line ((,class ;:inherit variable-pitch :height 0.9
                       :background "#305555")))
   `(tab-line-tab ((,class :inherit tab-line :foreground "white" :background "#705050")))
   `(tab-line-tab-current ((,class :inherit tab-line-tab :background "#301010"
                                   :foreground "dark orange")))
   `(tab-line-tab-inactive ((,class :inherit tab-line-tab)))
   ;; TODO: set a better alternating face.
   `(tab-line-tab-inactive-alternate ((,class :inherit tab-line-tab-inactive :foreground "cyan")))
   `(tab-line-tab-modified ((,class :foreground "red" )))
   `(tab-line-highlight ((,class :foreground "yellowgreen")))
   ;; tab-line-close-highlight
   ;; TODO: figure out a way to *not* override :foreground of tab-line-tab-current
   `(tab-line-tab-special ((,class :foreground "#BDBDFD" :slant italic)))
   ;; tab-line-tab-group


   ;; tab-bar.  tabs for window configurations.
   `(tab-bar ((,class ;:inherit variable-pitch :height 0.9
               :background "#103535")))
   `(tab-bar-tab ((,class :inherit tab-bar :foreground "dark orange" :background "#202020")))
   `(tab-bar-tab-inactive ((,class :inherit tab-bar :foreground "white" :background "#606060")))
   ;; tab-bar-tab-group-current
   ;; tab-bar-tab-group-inactive
   ;; tab-bar-tab-ungrouped
   ;; tab-bar-tab-highlight

   ))

(provide-theme 'focus-dark-256)

;;; focus-dark-256-theme.el ends here