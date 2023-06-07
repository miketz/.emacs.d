;;; -*- lexical-binding: t; -*-
;; Copyright (c) 2022
;; Author: Jeff Weisberg <tcp4me.com!jaw>
;; Created: 2022-Jun-20 10:35 (EDT)
;; Function: emacs themes. so cool!
;; URL: https://github.com/jaw0/jaw-theme

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(deftheme jaw-theme-light    "Jeff's Light theme")
(deftheme jaw-theme-dark     "Jeff's Dark theme")
(deftheme jaw-theme-terminal "Jeff's Terminal theme")

;;; customizations:
;;    jaw-face-default-font	- default font, eg. "Fira Code-12"
;;    jaw-face-comment-font	- comment font, eg: "mononoki-12"
;;    jaw-face-variable-font	- variable-width font, eg: "ETBembo"
;;    jaw-theme-light-bg-color	- background color
;;    jaw-theme-dark-bg-color	- background color
;;
;;    font may be specified as string "font-12" or list (:font "ETBembo"  :height 1.20)
;;    using a list, ith a floatingpt height keeps C-xC+/- zoom working

(let* (
       (fontlist (lambda (f)
                   (if (boundp f)
                       (let ((fv (symbol-value f)))
                         (if (consp fv) fv
                           (list :font fv)))
                     nil)))

       ;; change the modeline color if running as root
       (modeline-color (if (eq (user-uid) 0) "darkred" "darkgreen"))
       ;; configured parameter overrides fonts
       (comment-font   (funcall fontlist 'jaw-face-comment-font))
       (default-font   (funcall fontlist 'jaw-face-default-font))
       (variable-font  (funcall fontlist 'jaw-face-variable-font))

       ;; because some screens do poorly with "almost white"
       (bg-light-color (if (boundp 'jaw-theme-light-bg-color) jaw-theme-light-bg-color "#fffff4"))
       (bg-dark-color  (if (boundp 'jaw-theme-dark-bg-color)  jaw-theme-dark-bg-color  "#331"))
       ;; tab-bar has a bug - bigger font overflows [bug#55915]
       (bigger         (if (>= emacs-major-version 29) (list :height 1.25) nil))
       (line21	       (if (>= emacs-major-version 28) (cons 2 1) 1))
       (modebox   '(:box (:line-width 1 :style released-button)))
       (color     '((class color))) ; color xterm
       (graphic   '((class color) (min-colors 300)))

       (common  (list
                 `(cursor    ((,graphic (:background "#f66"))))
                 `(mouse     ((,graphic (:background "#f66"))))
                 `(variable-pitch ((,graphic ,@variable-font)))
                 `(tab-bar-buffer ((,graphic (:weight bold
                                                      :foreground "#AFF"))))
                 `(tab-bar-emacs  ((,graphic (:slant italic :weight light
                                                     :foreground "#FDC" ,@comment-font))))
                 `(tab-bar        ((,color   (:background ,modeline-color
                                                        :foreground "white" ,@bigger
                                                        :box (:line-width 1
                                                              :color "#242"
                                                              :style pressed-button)))))
                 `(tab-bar-tab    ((,graphic (:inherit 'tab-bar :background "#888"
                                                       :box (:line-width ,line21
                                                             :color "black"
                                                             :style released-button)))))
                 `(vertical-border     ((,color (:foreground ,modeline-color))))
                 `(cperl-hash-face     ((,color (:inherit font-lock-variable-name-face))))
                 `(cperl-array-face    ((,color (:inherit font-lock-variable-name-face))))
                 `(eshell-prompt-user  ((,color (:inherit 'ansi-color-magenta :weight semi-bold :height 1.1))))
                 `(eshell-prompt-root  ((,color (:inherit 'ansi-color-red     :weight semi-bold :height 1.1))))
                 `(eshell-prompt-dir   ((,color (:inherit 'ansi-color-blue :slant oblique :weight semi-bold
                                                          :height 1.1))))
                 `(trailing-whitespace ((,color (:background "#ffff88"
                                                             :underline "#ff8888")))))))

  ;; ################################################################
  ;; theme for terminal, mostly using terminal default colors, which might be light | dark
  (apply
   'custom-theme-set-faces
   'jaw-theme-terminal

   (if (eq (user-uid) 0)
       `(mode-line ((,color (:background "darkred" :foreground "#fff" ,@modebox))))
     `(mode-line ((,color   (:background "#aaddaa" :foreground "#000" ,@modebox)))))

   `(region    ((,color (:background "pink"))))
   `(mode-line ((,color (:background ,modeline-color :foreground "#fff" ,@modebox))))
   `(font-lock-comment-face ((,color (:foreground "#c77"))))
   `(minibuffer-prompt      ((,color (:foreground "#00c" :background "#fff"))))

   common)

  ;; ################################################################

  (apply
   'custom-theme-set-faces
   'jaw-theme-light

   `(default   ((,graphic (:background ,bg-light-color :foreground "#606060" ,@default-font))
                ;; invalid background uses terminal's background unchanged
                (,color   (:background "none"     :foreground "#444"    ,@default-font))))
   `(region    ((,color   (:background "pink"))))
   `(fringe    ((,graphic (:background "#f4f4e8"))))
   `(font-lock-comment-face ((,color   (:foreground "#b55" :slant italic ,@comment-font))))
   `(comment-keyword-tags   ((,graphic (:inherit font-lock-comment-face :weight bold
                                                 :box '(:line-width 1 :color "#fcc")
                                                 :foreground "#c66" :background "#f8fcfc"))))

   (if (eq (user-uid) 0)
       `(mode-line ((,color (:background "darkred" :foreground "#fff" ,@modebox))))
     `(mode-line ((,color   (:background "#aaddaa" :foreground "#000" ,@modebox)))))
   `(mode-line-inactive ((,graphic :background "#d8d8c8"  ,@modebox)))
   `(header-line ((,color :background "#ffe" :foreground "#996"
                          :slant italic :inherit font-lock-comment-face ,@modebox)))

   `(mode-line-directory-face ((,color ,@comment-font :foreground "#966" :slant italic)))

   `(org-level-1 ((,color (:inherit 'default :foreground "#6666ff" :weight semi-bold :height 1.5))))
   `(org-level-2 ((,color (:inherit 'default :foreground "#c07048" :weight semi-bold :height 1.4))))
   `(org-level-3 ((,color (:inherit 'default :foreground "#a020f0" :weight semi-bold :height 1.3))))
   `(org-level-4 ((,color (:inherit 'default :foreground "#228b22" :weight semi-bold :height 1.2))))
   `(org-level-5 ((,color (:inherit 'default :foreground "#008b8b" :weight semi-bold :height 1.2 :slant oblique))))
   `(org-level-6 ((,color (:inherit 'default :foreground "#aa8844" :weight semi-bold :height 1.1 :slant oblique))))
   `(org-level-7 ((,color (:inherit 'default :foreground "#8b2252" :weight semi-bold :height 1.1 :slant oblique))))
   `(org-level-8 ((,color (:inherit 'default :foreground "#483d8b" :weight semi-bold :height 1.1 :slant oblique))))
   `(org-todo           ((,color (:foreground "#ee3c3c"))))
   `(org-done           ((,color (:foreground "#368F0C"))))
   `(org-meta-line      ((,color (:foreground "#8888cc" :inherit font-lock-comment-face))))
   `(magit-hash         ((,color (:foreground "#aa7722"))))
   `(magit-log-author   ((,color (:foreground "#cc6644"))))
   `(magit-log-date     ((,color (:foreground "#cc6644"))))
   `(term-color-magenta ((,color (:foreground "magenta3"))))
   `(term-color-blue    ((,color (:foreground "blue2"))))
   `(term-color-red     ((,color (:foreground "red3"))))
   `(ansi-color-magenta ((,color (:foreground "magenta3"))))
   `(ansi-color-red     ((,color (:foreground "red3"))))
   `(ansi-color-blue    ((,color (:foreground "blue2"))))
   `(dired-subtree-depth-1-face ((,color (:background "#f8f8ff"))))
   `(dired-subtree-depth-2-face ((,color (:background "#e8ffe8"))))
   `(dired-subtree-depth-3-face ((,color (:background "#f0ffff"))))
   `(dired-subtree-depth-4-face ((,color (:background "#fff0ff"))))
   `(dired-subtree-depth-5-face ((,color (:background "#ffffd0"))))
   `(dired-subtree-depth-6-face ((,color (:background "#f0f0f0"))))
   `(all-the-icons-ibuffer-size-face ((,color (:foreground "#b55"))))
   `(all-the-icons-ibuffer-file-face ((,color (:foreground "#b55"))))

   `(window-divider     ((,color (:foreground ,modeline-color))))
   `(window-divider-first-pixel ((,color (:foreground ,bg-light-color))))
   `(window-divider-last-pixel  ((,color (:foreground ,bg-light-color))))

   common)

  ;; ################################################################

  (apply
   'custom-theme-set-faces
   'jaw-theme-dark

   `(default   ((,graphic (:background "#383810" :foreground "#e0e0e8" ,@default-font))
                (,color   (:background "#000"    :foreground "#eee"    ,@default-font))))
   `(region    ((,color   (:background "#466"))))
   `(fringe    ((,graphic (:background "#343410"))))
   `(font-lock-comment-face ((,color   (:foreground "#fcc" :slant italic ,@comment-font))))
   `(comment-keyword-tags   ((,graphic (:inherit font-lock-comment-face :weight bold
                                                 :box (:line-width 1 :color "#977")
                                                 :foreground "#f99" :background "#244"))))

   `(mode-line ((,color (:background ,modeline-color :foreground "#fff" ,@modebox))))
   `(mode-line-inactive ((,graphic :background "#606040" ,@modebox)))
   `(header-line ((,color :background "#442" :foreground "#cc9" :slant italic
                          :inherit font-lock-comment-face
                          :box (:line-width 1 :color "#555"))))

   `(mode-line-directory-face ((,color ,@comment-font :foreground "#ebb" :slant italic)))

   `(org-level-1 ((,color (:inherit 'default :foreground "#87cefa" :weight semi-bold :height 1.5))))
   `(org-level-2 ((,color (:inherit 'default :foreground "#eedd82" :weight semi-bold :height 1.4))))
   `(org-level-3 ((,color (:inherit 'default :foreground "#00ffff" :weight semi-bold :height 1.3))))
   `(org-level-4 ((,color (:inherit 'default :foreground "#ffcccc" :weight semi-bold :height 1.2))))
   `(org-level-5 ((,color (:inherit 'default :foreground "#98fb98" :weight semi-bold :height 1.2 :slant oblique))))
   `(org-level-6 ((,color (:inherit 'default :foreground "#7fffd4" :weight semi-bold :height 1.1 :slant oblique))))
   `(org-level-7 ((,color (:inherit 'default :foreground "#ffa07a" :weight semi-bold :height 1.1 :slant oblique))))
   `(org-level-8 ((,color (:inherit 'default :foreground "#b0c4de" :weight semi-bold :height 1.1 :slant oblique))))
   `(org-todo           ((,color (:foreground "#ff6644"))))
   `(org-done           ((,color (:foreground "#66ee44"))))
   `(org-meta-line      ((,color (:foreground "#aaaadd" :inherit font-lock-comment-face))))
   `(magit-hash         ((,color (:foreground "#ee9966"))))
   `(magit-log-author   ((,color (:foreground "#eeaa99"))))
   `(magit-log-date     ((,color (:foreground "#eeaa99"))))
   `(term-color-magenta ((,color (:foreground "#dd88dd"))))
   `(term-color-blue    ((,color (:foreground "#8888dd"))))
   `(term-color-red     ((,color (:foreground "#ff6666"))))
   `(ansi-color-magenta ((,color (:foreground "#dd88dd"))))
   `(ansi-color-red     ((,color (:foreground "#ff6666"))))
   `(ansi-color-blue    ((,color (:foreground "#8888dd"))))
   `(dired-subtree-depth-1-face ((,color (:background "#383850"))))
   `(dired-subtree-depth-2-face ((,color (:background "#305030"))))
   `(dired-subtree-depth-3-face ((,color (:background "#304848"))))
   `(dired-subtree-depth-4-face ((,color (:background "#503050"))))
   `(dired-subtree-depth-5-face ((,color (:background "#604020"))))
   `(dired-subtree-depth-6-face ((,color (:background "#505050"))))
   `(all-the-icons-ibuffer-size-face ((,color (:foreground "#fcc"))))
   `(all-the-icons-ibuffer-file-face ((,color (:foreground "#fcc"))))

   `(window-divider     ((,color (:foreground ,modeline-color))))
   `(window-divider-first-pixel ((,color (:foreground ,bg-dark-color))))
   `(window-divider-last-pixel  ((,color (:foreground ,bg-dark-color))))

   common))

;; fix the mouse color
(defun jaw-theme-set-mouse-color (theme)
  (when (string-match "jaw-theme" (symbol-name theme))
    (set-mouse-color "#f66")))

(when window-system
  (advice-add 'enable-theme :after #'jaw-theme-set-mouse-color))

(provide-theme 'jaw-theme-light)
(provide-theme 'jaw-theme-dark)
(provide-theme 'jaw-theme-terminal)
(provide 'jaw-theme)
