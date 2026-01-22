;;; focus-dark-theme.el --- Minimal theme -*- lexical-binding: t -*-

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
;; keywords: return, class, if etc
;;;
;;; dim
;;; {}
;;; package namespace preciding Thing: foo.bar.baz.Thing
;;;
;;; if light theme, use background highlights more to overcome contrast issues

;;; Code:
(deftheme focus-dark "focus-dark color theme")


(let* ((class t)
       (fg "#EEEED1")
       (bg "#35352B")
       (rain-1 "#FF4500")
       (rain-1-bg bg)
       (rain-2 "#00FFFF")
       (rain-2-bg bg)
       (rain-3 "#FFFF00")
       (rain-3-bg bg)
       (rain-4 "#DDA0DD")
       (rain-4-bg bg)
       (rain-5 "#7CFC00")
       (rain-5-bg bg)
       (rain-6 "#FFA500")
       (rain-6-bg bg)
       (rain-7 "#FFFFFF")
       (rain-7-bg bg)
       (rain-8 "#FF69B4")
       (rain-8-bg "#101010")
       (rain-9 "#CDAA7D")
       (rain-9-bg bg)
       (rain-fg-u "#A0522D")
       (rain-bg-u "#000000"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Vars
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (custom-theme-set-variables
   'focus-dark
   ;; indent-bars
   ;; This is hard to configure in a considerate way in a theme. Becuase
   ;; `indent-bars-color-by-depth' mixes several things in 1 var. A boolean
   ;; feature toggle (ie non-nil), color settings, and blend.
   ;; since this is my personal theme, just do it how i want it.
   `(indent-bars-color-by-depth nil)
   `(indent-bars-color '("#FFFFFF" :face-bg nil :blend 0.075))

   `(evil-emacs-state-cursor    '(bar "red" ;"cyan"
                                  ))
   `(evil-normal-state-cursor   '(hollow "spring green"))
   `(evil-insert-state-cursor   '(bar "spring green"))
   `(evil-visual-state-cursor   '(hollow "orange"))
   `(evil-operator-state-cursor '(box "red"))
   `(evil-replace-state-cursor  '(hbar "orange red"))
   `(evil-motion-state-cursor   '(box "spring green")))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Faces
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (custom-theme-set-faces
   'focus-dark

   `(default ((,class :foreground ,fg :background ,bg)))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; highlights
   `(font-lock-function-name-face ((,class :foreground "#AFEEEE" :background "#000000" :weight normal)))
   `(font-lock-variable-name-face ((,class :foreground "#FF4500";"#66CDAA";"#Cb4040";"#FF4500" :background ,bg
                                           )))
   `(js2-function-param ((,class :inherit font-lock-variable-name-face)))

   `(font-lock-comment-face ((,class :foreground "#8FB28F" )))
   `(font-lock-comment-delimiter-face ((,class :inherit font-lock-comment-face)))
   `(font-lock-doc-face ((,class :inherit font-lock-comment-face)))
   `(font-lock-doc-markup-face ((,class :inherit font-lock-doc-face :weight bold)))
   `(font-lock-string-face ((,class :foreground "LightSalmon")))
   ;; `(font-lock-builtin-face ((,class (:foreground "dark blue" :background ,bg :weight normal))))
   `(font-lock-constant-face ((,class :foreground ,fg :background "#232319" :weight bold)))
   `(font-lock-number-face ((,class :foreground "#50b5b5" :background ,bg))) ;treesit



   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; disable highlights. use regular fg/bg
   `(font-lock-keyword-face ((,class :foreground ,fg :background ,bg)))
   `(font-lock-type-face ((,class :foreground ,fg :background ,bg)))
   `(font-lock-function-call-face ((,class :foreground ,fg :background ,bg))) ;treesit
   `(font-lock-variable-use-face ((,class :inherit default))) ;treesit
   `(font-lock-operator-face ((,class :inherit font-lock-keyword-face))) ;treesit
   `(font-lock-property-name-face ((,class :foreground ,fg :background ,bg))) ;treesit
   `(font-lock-property-use-face ((,class :inherit font-lock-property-name-face))) ;treesit
   `(font-lock-delimiter-face ((,class :foreground ,fg :background ,bg))) ;treesit
   `(font-lock-escape-face ((,class :inherit font-lock-string-face :weight bold))) ;treesit
   `(font-lock-regexp-face ((,class :foreground ,fg))) ;treesit
   `(font-lock-negation-char-face ((,class (:foreground ,fg :weight bold))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; dim. don't dim for now actually as the face control is not fine grained enough.
   `(font-lock-bracket-face ((,class :foreground ,fg :background ,bg))) ;treesit
   `(font-lock-punctuation-face ((,class :foreground ,fg :background ,bg))) ;treesit
   `(font-lock-misc-punctuation-face ((,class :foreground ,fg :background ,bg))) ;treesit

   ))

(provide-theme 'focus-dark)

;;; focus-dark-theme.el ends here