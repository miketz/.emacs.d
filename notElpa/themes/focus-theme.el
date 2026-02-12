;;; focus-theme.el --- Minimal theme -*- lexical-binding: t -*-

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
(deftheme focus "focus color theme")


(let ((class t)
      (fg "#000000")
      (bg "#FFFFFF")
      (dim-fg "#888888")
      (rain-bg "#D5D1B3")
      (rain-bg-highlight "#C5C1A3"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Vars
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (custom-theme-set-variables
   'focus
   ;; indent-bars
   ;; This is hard to configure in a considerate way in a theme. Becuase
   ;; `indent-bars-color-by-depth' mixes several things in 1 var. A boolean
   ;; feature toggle (ie non-nil), color settings, and blend.
   ;; since this is my personal theme, just do it how i want it.
   `(indent-bars-color-by-depth nil)
   `(indent-bars-color '("black" :face-bg nil :blend 0.07)))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Faces
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (custom-theme-set-faces
   'focus

   `(default ((,class :foreground ,fg :background ,bg)))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; highlights
   `(font-lock-function-name-face ((,class :foreground "blue" :background "light cyan" :weight bold)))
   `(font-lock-variable-name-face ((,class :foreground "dark red" :background "#FFe0e0")))
   `(js2-function-param ((,class :inherit font-lock-variable-name-face)))

   `(font-lock-comment-face ((,class :foreground "dark green" :background "honeydew")))
   `(font-lock-comment-delimiter-face ((,class :inherit font-lock-comment-face)))
   `(font-lock-doc-face ((,class :inherit font-lock-comment-face)))
   `(font-lock-doc-markup-face ((,class :inherit font-lock-doc-face :weight bold)))
   `(font-lock-string-face ((,class :foreground "chocolate4" :background "papaya whip";"papaya whip";"#fffcf2"
                                    )))
   `(font-lock-builtin-face ((,class (:foreground "dark blue" :weight normal))))
   `(font-lock-constant-face ((,class :weight bold)))
   `(font-lock-number-face ((,class :foreground "blue"))) ;treesit



   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; disable highlights. use regular fg/bg
   `(font-lock-keyword-face ((,class :foreground ,fg)))
   `(font-lock-type-face ((,class :foreground ,fg)))
   `(font-lock-function-call-face ((,class :foreground ,fg ;:background "MistyRose1"
                                           ))) ;treesit
   `(font-lock-variable-use-face ((,class :inherit default))) ;treesit
   `(font-lock-operator-face ((,class :inherit font-lock-keyword-face))) ;treesit
   `(font-lock-property-name-face ((,class :foreground ,fg))) ;treesit
   `(font-lock-property-use-face ((,class :inherit font-lock-property-name-face))) ;treesit
   `(font-lock-delimiter-face ((,class :foreground ,fg))) ;treesit
   `(font-lock-escape-face ((,class :inherit font-lock-string-face :weight bold))) ;treesit
   `(font-lock-regexp-face ((,class :foreground ,fg))) ;treesit
   `(font-lock-negation-char-face ((,class (:foreground "red" :weight bold))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; rainbow-delimiters.
   ;; In theory we don't want to highlight delimiters in this theme. But if
   ;; you're using rainbow-delimiters mode that means you do want them highlighted.
   ;; May as well brighten them up. Turn off rainbow-delimiters-mode to remove color.
   `(rainbow-delimiters-depth-1-face ((,class (:foreground "black" :background ,rain-bg-highlight :weight bold))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground "black" :background "#C0DfDf" :weight bold))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground "red" :background ,rain-bg-highlight :weight bold))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground "purple" :background "#DfD0D5" :weight bold))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground "black" :background "#EfEaBd" :weight bold))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground "magenta" :background ,rain-bg ;"#EEEEFF"
                                                      :weight bold))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground "gray52" :weight bold))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground "indianred3" :background ,rain-bg-highlight :weight bold))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground "orange" :background "gray50")))) ;:background "#fff7ca"
   `(rainbow-delimiters-unmatched-face ((,class (:foreground "yellow" :background "black" :weight bold))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; dim. don't dim for now actually as the face control is not fine grained enough.
   `(font-lock-bracket-face ((,class :foreground ,fg))) ;treesit
   `(font-lock-punctuation-face ((,class :foreground ,fg))) ;treesit
   `(font-lock-misc-punctuation-face ((,class :foreground ,fg))) ;treesit

   ))

(provide-theme 'focus)

;;; focus-theme.el ends here