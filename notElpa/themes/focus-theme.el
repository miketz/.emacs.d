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
      (dim-fg "#888888"))

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
   `(font-lock-variable-name-face ((,class :foreground "dark red" :background ,bg)))

   `(font-lock-comment-face ((,class :foreground "dark green" :background "honeydew")))
   `(font-lock-comment-delimiter-face ((,class :inherit font-lock-comment-face)))
   `(font-lock-doc-face ((,class :inherit font-lock-comment-face)))
   `(font-lock-string-face ((,class :foreground "chocolate4" :background "papaya whip")))
   `(font-lock-builtin-face ((,class (:foreground "dark blue" :background ,bg :weight normal))))
   `(font-lock-constant-face ((,class :foreground "blue" :background ,bg :weight bold)))
   `(font-lock-number-face ((,class :foreground "blue" :background ,bg))) ;treesit



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

   `(rainbow-delimiters-depth-1-face ((,class :foreground ,fg :background ,bg)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,fg :background ,bg)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,fg :background ,bg)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,fg :background ,bg)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,fg :background ,bg)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,fg :background ,bg)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,fg :background ,bg)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,fg :background ,bg)))
   `(rainbow-delimiters-depth-9-face ((,class :foreground ,fg :background ,bg)))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,fg :background "red")))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; dim. don't dim for now actually.
   `(font-lock-bracket-face ((,class :foreground ,fg :background ,bg))) ;treesit
   `(font-lock-punctuation-face ((,class :foreground ,fg :background ,bg))) ;treesit
   `(font-lock-misc-punctuation-face ((,class :foreground ,fg :background ,bg))) ;treesit


   ;; swiper, ivy, counsel
   `(swiper-line-face ((,class (:background "#FFA366" ;,bg-highlight ;"gray30"
                                ))))
   ;; face-1 fills in the space between matches. 2-4 are for matches.
   `(swiper-match-face-1 ((,class :foreground "white";"#F9F5D7"
                                  :background "#A5A183";,bg-highlight
                                  )))
   `(swiper-match-face-2 ((,class (:foreground "black" :background "#FF4500"))))
   ;; NOTE: face-3, 4 don't work when out-of-order matching is used.
   ;; TODO: make bug report to swiper about face 3,4
   `(swiper-match-face-3 ((,class (:foreground "black" :background "#00FFFF"))))
   `(swiper-match-face-4 ((,class (:foreground "black" :background "#FFFF00"))))
   ;; the non-selected lines in the minibuffer
   `(swiper-background-match-face-1 ((,class :inherit swiper-match-face-1)))
   `(swiper-background-match-face-2 ((,class :inherit swiper-match-face-2)))
   `(swiper-background-match-face-3 ((,class :inherit swiper-match-face-3)))
   `(swiper-background-match-face-4 ((,class :inherit swiper-match-face-4)))
   `(ivy-action ((,class (:foreground "black"))))
   ;; `(ivy-confirm-face ((,class (:foreground "yellow" :italic t))))
   `(ivy-current-match ((,class (:inherit swiper-line-face))))
   ;; `(ivy-cursor ((,class (:foreground "white"))))
   ;; `(ivy-highlight-face ((,class (:background "white"))))
   `(ivy-match-required-face ((,class (:foreground "#300000" :background "#FF4500"))))
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

   )
  )

(provide-theme 'focus)

;;; focus-theme.el ends here