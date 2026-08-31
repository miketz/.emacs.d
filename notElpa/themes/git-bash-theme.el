;;; git-bash-theme.el --- Minimal theme -*- lexical-binding: t -*-

;;; Commentary:
;;; Theme for the git bash terminal on MS Windows.
;;; 16 colors available.

;;; Code:
(deftheme git-bash "git-bash color theme")


(let* (;; 16 color pallete on git bash terminal in windows
       (black "#000000")
       (blue "#0000cd")
       (green "#228b22")
       (cyan "#00ced1")
       (red "#b22222")
       (magenta "#8b008b")
       (brown "#a0522d")
       (lightgray "#bebebe")
       (darkgray "#666666")
       (lightblue "#0000ff")
       (lightgreen "#00ff00")
       (lightcyan "#00ffff")
       (lightred "#ff0000")
       (lightmagenta "#ff00ff")
       (yellow "#ffff00")
       (white "#ffffff")
       ;; vars for this theme. use the 16 color pallete
       (class t)
       (fg white)
       (bg-2 darkgray)
       (bg-1 darkgray)
       (bg darkgray)
       (bg+1 darkgray)
       (bg+2 darkgray)
       (dim+4 lightgray)
       (dim+3 lightgray)
       (dim+2 lightgray)
       (dim+1 lightgray)
       (dim lightgray)
       (dim-1 lightgray)
       (dim-2 lightgray)
       (dim-3 lightgray)
       (dim-4 lightgray)
       (rain-1 red)
       (rain-1-bg bg)
       (rain-2 lightblue)
       (rain-2-bg bg)
       (rain-3 brown)
       (rain-3-bg bg)
       (rain-4 lightmagenta)
       (rain-4-bg bg)
       (rain-5 green)
       (rain-5-bg bg)
       (rain-6 yellow)
       (rain-6-bg bg)
       (rain-7 magenta)
       (rain-7-bg black)
       (rain-8 lightcyan)
       (rain-8-bg black)
       (rain-9 cyan)
       (rain-9-bg black)
       (rain-fg-u darkgray)
       (rain-bg-u red))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Vars
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (custom-theme-set-variables
   'git-bash
   ;; indent-bars
   ;; This is hard to configure in a considerate way in a theme. Becuase
   ;; `indent-bars-color-by-depth' mixes several things in 1 var. A boolean
   ;; feature toggle (ie non-nil), color settings, and blend.
   ;; since this is my personal theme, just do it how i want it.
   `(indent-bars-color-by-depth nil)
   `(indent-bars-color '(,dim :face-bg nil :blend 0.075))

   `(evil-emacs-state-cursor    '(bar ,red))
   `(evil-normal-state-cursor   '(hollow ,green))
   `(evil-insert-state-cursor   '(bar ,green))
   `(evil-visual-state-cursor   '(hollow ,yellow))
   `(evil-operator-state-cursor '(box ,red))
   `(evil-replace-state-cursor  '(hbar ,red))
   `(evil-motion-state-cursor   '(box ,yellow)))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Faces
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (custom-theme-set-faces
   'git-bash

   `(default ((,class :foreground ,fg :background ,bg)))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; highlights
   `(font-lock-function-name-face ((,class :foreground ,lightblue :background ,black :weight normal)))
   `(font-lock-variable-name-face ((,class :foreground ,blue
                                           )))
   `(js2-function-param ((,class :inherit font-lock-variable-name-face)))

   `(font-lock-comment-face ((,class :foreground ,lightgreen
                                     :background ,bg+1
                                     )))
   `(font-lock-comment-delimiter-face ((,class :inherit font-lock-comment-face)))
   `(font-lock-doc-face ((,class :inherit font-lock-comment-face)))
   `(font-lock-doc-markup-face ((,class :inherit font-lock-doc-face :weight bold)))
   `(font-lock-string-face ((,class :foreground ,yellow
                                    :background ,bg+1
                                    )))
   ;; `(font-lock-builtin-face ((,class (:foreground "dark blue" :background ,bg :weight normal))))
   `(font-lock-constant-face ((,class :foreground ,fg :weight normal)))
   `(font-lock-number-face ((,class :foreground ,lightblue
                                    ))) ;treesit

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; disable highlights. use regular fg/bg
   `(font-lock-keyword-face ((,class :foreground ,dim-2)))
   `(font-lock-type-face ((,class :foreground ,dim+2)))
   `(font-lock-function-call-face ((,class :foreground ,magenta))) ;treesit
   `(font-lock-variable-use-face ((,class :inherit default))) ;treesit
   `(font-lock-operator-face ((,class :inherit font-lock-keyword-face :foreground ,dim+4))) ;treesit
   `(font-lock-property-name-face ((,class :foreground ,fg))) ;treesit
   `(font-lock-property-use-face ((,class :inherit font-lock-property-name-face))) ;treesit
   `(font-lock-delimiter-face ((,class :foreground ,dim+1))) ;treesit
   `(font-lock-escape-face ((,class :inherit font-lock-string-face
                                    :foreground ,red
                                    ))) ;treesit
   `(font-lock-regexp-face ((,class :foreground ,fg))) ;treesit
   `(font-lock-negation-char-face ((,class (:foreground ,red
                                                        :weight bold))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; dim. don't dim for now actually as the face control is not fine grained enough.
   `(font-lock-bracket-face ((,class :foreground ,dim-2))) ;treesit
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
     ((,class (:foreground ,fg
                           :background ,lightblue
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((,class (:foreground ,fg
                                               :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,dim
                           :background ,black
                           :box (:line-width -1 :style released-button)))))

   ;; num3
   `(num3-face-even ((,class (:background ,black :foreground ,lightcyan))))

   ;; display-line-numbers. native implementation
   `(line-number ((,class (:background ,black ;"#231808"
                                       :foreground ,dim
                                       ))))
   `(line-number-current-line ((,class (:inherit line-number
                                                 ;; 107, 71
                                                 :foreground ,dim
                                                 ))))
   `(line-number-major-tick ((,class :foreground ,dim
                                     :background ,black
                                     )))
   `(line-number-minor-tick ((,class :foreground ,dim :background ,black)))

   ;; faces.el
   `(show-paren-match ((,class (:slant italic
                                       :weight bold
                                       :strike-through t))))

   ;; ;; tab-line.  like web browser tabs.
   ;; `(tab-line ((,class ;:inherit variable-pitch :height 0.9
   ;;                     :background ,color-23;"#305555"
   ;;                     )))
   ;; `(tab-line-tab ((,class :inherit tab-line :foreground ,brightwhite
   ;;                         :background ,color-95 ;"#705050"
   ;;                         )))
   ;; `(tab-line-tab-current ((,class :inherit tab-line-tab
   ;;                                 ;; 52
   ;;                                 :background ,color-52 ;"#301010"
   ;;                                 :foreground ,color-208 ;"#ff8c00"
   ;;                                 )))
   ;; `(tab-line-tab-inactive ((,class :inherit tab-line-tab)))
   ;; ;; TODO: set a better alternating face.
   ;; `(tab-line-tab-inactive-alternate ((,class :inherit tab-line-tab-inactive :foreground ,brightcyan
   ;;                                            )))
   ;; `(tab-line-tab-modified ((,class :foreground ,brightred)))
   ;; `(tab-line-highlight ((,class :foreground ,color-148 ;#9acd32 "yellowgreen"
   ;;                               )))
   ;; ;; tab-line-close-highlight
   ;; ;; TODO: figure out a way to *not* override :foreground of tab-line-tab-current
   ;; `(tab-line-tab-special ((,class :foreground ,color-147 ;"#BDBDFD"
   ;;                                 :slant italic)))
   ;; ;; tab-line-tab-group


   ;; ;; tab-bar.  tabs for window configurations.
   ;; `(tab-bar ((,class ;:inherit variable-pitch :height 0.9
   ;;             :background ,color-17 ;"#103535"
   ;;             )))
   ;; `(tab-bar-tab ((,class :inherit tab-bar :foreground ,color-208;"dark orange"
   ;;                        :background ,color-234 ;"#202020"
   ;;                        )))
   ;; `(tab-bar-tab-inactive ((,class :inherit tab-bar :foreground ,brightwhite
   ;;                                 :background ,color-59 ;"#606060"
   ;;                                 )))
   ;; ;; tab-bar-tab-group-current
   ;; ;; tab-bar-tab-group-inactive
   ;; ;; tab-bar-tab-ungrouped
   ;; ;; tab-bar-tab-highlight

   ))

(provide-theme 'git-bash)

;;; git-bash-theme.el ends here