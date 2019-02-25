;;; -*- lexical-binding: t -*-

(defun my-rainbow-parens-dark-bg ()
  "Colors for parens that are easy to distinguish from each other when against a dark bg."
  (interactive)
  (custom-theme-set-faces
   (car custom-enabled-themes)
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "orange red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "plum"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "lawn green"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "white"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "seagreen1"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black"))))))

(defun my-rainbow-parens-dark-bg-bold ()
  "Colors for parens that are easy to distinguish from each other when against a dark bg. "
  (interactive)
  (custom-theme-set-faces
   (car custom-enabled-themes)
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "orange red" :bold t))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan" :bold t))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow" :bold t))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "plum" :bold t))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "lawn green" :bold t))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orange" :bold t))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "white" :bold t))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "seagreen1" :bold t))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3" :bold t))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black" :bold t))))))

(defun my-rainbow-parens-light-bg ()
  (interactive)
  (custom-theme-set-faces
   (car custom-enabled-themes)
   '(rainbow-delimiters-depth-1-face ((t (:foreground "black"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#09a509"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#3388ff"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "black" :background "red"))))))

(defun my-rainbow-parens-light-bg2 ()
  "Colored parens with highlighting."
  (interactive)
  (custom-theme-set-faces
   (car custom-enabled-themes)
   '(rainbow-delimiters-depth-1-face ((t (:foreground "black" :background "gray94" :weight normal))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "light cyan" :bold nil))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background "#faEaEa" :bold nil))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "lavenderblush" :bold nil))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "lemon chiffon" :bold nil))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background "#EEEEFF" :bold nil))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52" :bold nil))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3" :bold nil))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "#fff7ca" :bold nil))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black" :bold nil))))))

(defun my-rainbow-parens-light-bg3 ()
  "Colored parens with highlighting."
  (interactive)
  (custom-theme-set-faces
   (car custom-enabled-themes)
   '(rainbow-delimiters-depth-1-face ((t (:foreground "black" :background "gray94" :weight bold))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "light cyan" :bold t))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background "#faEaEa" :bold t))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "lavenderblush" :bold t))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "lemon chiffon" :bold t))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background "#EEEEFF" :bold t))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52" :bold t))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3" :bold t))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "#fff7ca" :bold t))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black" :bold t))))))




(defun my-color-badger ()
  (interactive)
  (load-theme 'badger t)
  (custom-theme-set-faces
   'badger
   ;; separates windows.
   '(vertical-border ((t (:foreground "gray15"))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   `(font-lock-comment-face
     ((t (:foreground "dark cyan" :slant italic))))
   '(region ((t :background "#7F073F")))
   `(mode-line
     ((t (:foreground "#00AF00"
                      :background "#150505"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "dark gray"
                      :background  "#090202";"#051515"
                      :box (:line-width -1 :style pressed-button)))))))

(defun my-color-deeper-blue ()
  (interactive)
  (load-theme 'deeper-blue t)
  ;; (when my-use-evil-p
  ;;   (my-cursor-stuff-darkBg)) ;;TODO: move into `custom-set-faces'

  (my-rainbow-parens-dark-bg)

  ;;wrap mods in `custom-theme-set-faces' so they can be rolled back with `disable-theme'
  (custom-theme-set-faces
   'deeper-blue


   '(leerzeichen ((t (:foreground "gray40"           ;"#A8A800"
                                  :background "black" ;"#D4D4C8"
                                  :italic nil
                                  :bold nil))))

   ;; separates windwos.
   '(vertical-border ((t (:foreground "gray20"))))

   '(my-tilde-face
     ((t (:foreground "gray35"))))

   '(org-agenda-calendar-event ((t (:background "black"))))

   ;; `(mode-line ((t (:background "gray40"
   ;;                              :foreground "black"
   ;;                              :box (:line-width -1
   ;;                                                :style released-button)))))
   `(mode-line ((t (:background "black"
                                              :foreground "slategray"
                                              :box (:line-width -1
                                                                :color "gray20"
                                                                :style released-button)))))
   `(mode-line-buffer-id ((t (:weight bold
                                           :background nil
                                           :foreground "slategray"))))
   `(mode-line-inactive ((t (:background "black"
                                              :foreground "gray30"
                                              :box (:line-width -1
                                                                :color "black"
                                                                :style released-button)))))


   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))))


;;; loads the default emacs theme. Makes a few mods too.
(defun my-color-default ()
  (interactive)
  ;;default is the lack of themes, so disable any enabled themes.
  (dolist (thm custom-enabled-themes)
    (disable-theme thm))
  ;;(set-background-color "ivory2")

  ;; (my-cursor-stuff-lightBg)
  ;; (let ((cur '(box "blue")))
  ;;   (setq evil-normal-state-cursor cur)
  ;;   (setq evil-visual-state-cursor '(hollow "blue"))
  ;;   (setq evil-operator-state-cursor cur))

  ;; (my-set-font :sym 'consolas
  ;;              :weight 'bold
  ;;              :height 125
  ;;              :resize-window t)
  (progn ; use bold font.
    ;; (custom-set-faces
    ;;  '(default ((t (:weight bold)))))

    ;; `set-face-attribute' is better than `custom-set-faces'.
    ;; it doesn't wipe out all the attributes.
    (mapc (lambda (face)
            (set-face-attribute face
                                nil ;; all frames
                                :weight 'bold))
          (face-list))
    ;; refresh screen.
    (when (fboundp 'my-w32-run) ; TODO: make it work on non-Windows machines.
      (my-w32-run 'restore-curr-frame)
      (my-w32-run 'max)))

  ;;(set-face-background hl-line-face "#EEFFEE")
  ;;(my-set-font :weight 'bold)

  ;; TODO: find a way to set these faces so they can be rolled back. Like when
  ;; set for a specific theme with `custom-theme-set-faces'.
  (custom-set-faces
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   `(ace-jump-face-foreground
     ((t (:foreground "black"
                      :background "cyan"
                      :slant normal
                      :weight bold
                      :inverse-video nil))))

   '(rainbow-delimiters-depth-1-face ((t (:foreground "black" :background "gray94" :weight normal))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "light cyan" :bold nil))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background "#faEaEa" :bold nil))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "lavenderblush" :bold nil))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "lemon chiffon" :bold nil))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background "#EEEEFF" :bold nil))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52" :bold nil))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3" :bold nil))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "#fff7ca" :bold nil))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black" :bold nil))))))

(defun my-color-default-fancy ()
  (interactive)
  (my-color-default)

  (set-background-color "floral white")

  (my-set-alpha 80)
  ;; (progn
  ;;   (setq my--curr-alpha 80)
  ;;   (set-frame-parameter (selected-frame) 'alpha `(,my--curr-alpha
  ;;                                                  ,my--curr-alpha)))

  ;; (progn
  ;;   (require 'highlight-tail)
  ;;   (setq highlight-tail-colors '(("lawn green" . 0)
  ;;                                 ("yellow" . 40)))
  ;;   (setq highlight-tail-steps 20       ;80
  ;;         highlight-tail-timer 0.04     ;0.04
  ;;         )
  ;;   (setq highlight-tail-posterior-type t) ;(setq highlight-tail-posterior-type 'const)
  ;;   (highlight-tail-mode)
  ;;   ;;(highlight-tail-reload)
  ;;   )
  )

(defun my-color-dichromacy ()
  (interactive)
  (load-theme 'dichromacy t)
  ;; (my-cursor-stuff :color-emacs "red" :color-evil "blue")

  (my-rainbow-parens-light-bg2)

  (custom-theme-set-faces
   'dichromacy
   ;;`(default ((t (:foreground "black" :background ,mayan-smoke))))
   `(default ((t (:foreground "black" :background ,"white"))))
   `(mode-line
     ((t (:foreground "black"
                      :background "#CCCCCC"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "#666666"
                      :background "#EEEEEE"
                      :box (:line-width -1 :style released-button)))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil)))))

  ;; (my-set-font :weight 'bold
  ;;              :height 140)
  ;;(set-background-color "floral white")
  )

(defun my-color-firebelly ()
  (interactive)
  (my-load-theme-make-bold-like-zenburn 'firebelly) ;;(load-theme 'firebelly t)

  (custom-theme-set-variables
   'firebelly
   `(fci-rule-color "#343434"))

  (my-rainbow-parens-dark-bg)

  (custom-theme-set-faces
   'firebelly

   `(avy-lead-face
     ((t (:foreground "orange"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))

   '(highlight-indent-guides-odd-face
     ((t (:background "#242424"
                      :weight bold))))
   '(highlight-indent-guides-even-face
     ((t (:background "#202020"
                      :weight bold))))

   ;; override the hi-yellow face for printf escapes.
   '(hi-yellow
     ((t (:foreground "hotpink"
                      :weight bold))))

   `(org-level-2 ((t (:foreground "gray"))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   '(sldb-section-face
     ((t (:foreground "light sky blue"))))
   ;; swiper/ivy faces TODO: fix more ivy/swiper faces.
   '(ivy-current-match
     ((t (:background "black" :foreground "light sky blue"))))

   '(font-lock-variable-name-face
     ((t (:foreground "#924040"))))
   '(my-tilde-face
     ((t (:foreground "#523030"))))

   `(font-lock-comment-face
     ((t (:foreground "RosyBrown4" :slant italic))))
   `(font-lock-comment-delimiter-face
     ((t (:foreground "RosyBrown3"))))

   `(fringe
     ((t (:foreground "burlywood"
                      :background "black"))))
   `(region
     ((t (:background "#494949"))))     ;"#49483E"
   ))

(defun my-color-gandalf ()
  (interactive)
  (load-theme 'gandalf t)
  ;; (my-cursor-stuff-lightBg)
  ;; (let ((cur '(box "blue")))
  ;;   (setq evil-normal-state-cursor cur)
  ;;   (setq evil-visual-state-cursor '(hollow "blue"))
  ;;   (setq evil-operator-state-cursor cur))

  (my-rainbow-parens-light-bg2)

  (custom-theme-set-faces
   'gandalf
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   `(region
     ((t (:background "goldenrod"))))
   `(fringe
     ((t (:foreground "black"
                      :background "gray"))))

   `(ace-jump-face-foreground
     ((t (:foreground "black"
                      :background "yellow"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(mode-line
     ((t (:foreground "black"
                      :background "gray"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "#505050"
                      :background "#e3e3e3"
                      :box (:line-width -1 :style released-button)))))

   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))))

(defun my-color-github ()
  (interactive)
  (load-theme 'github t)

  ;; (my-cursor-stuff :color-emacs "maroon" :color-evil "blue")
  ;; (let ((cur '(box "blue")))
  ;;   (setq evil-normal-state-cursor cur)
  ;;   (setq evil-visual-state-cursor '(hollow "blue"))
  ;;   (setq evil-operator-state-cursor cur))

  (my-rainbow-parens-light-bg2)

  (custom-theme-set-faces
   'github
   `(mode-line ((t (:background "grey75"
                                :foreground "black"
                                :box (:line-width -1 :style released-button)
                                :height 1.0))))

   `(avy-lead-face
     ((t (:foreground "black"
                      :background "yellow"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))

   '(fringe
     ((t (:foreground "#9B9B9B" :background "gray90"))))

   `(ace-jump-face-foreground
     ((t (:foreground "yellow"
                      :background "black"
                      :slant normal
                      :weight bold
                      :inverse-video nil))))

   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   ;; '(js2-external-variable ((t :underline (:color "red" :style wave)
   ;;                             :background "black")))
   ;; '(js2-error ((t :underline (:color "red" :style wave)
   ;;                             :background "dark red")))
   '(js2-function-call ((t :foreground "#990000"))) ;;making same as font-lock-function-name-face
   ;; '(js2-warning ((t :underline (:color "yellow" :style wave)
   ;;                   :background "navy blue")))
   ;;'(js2-private-member ((t :foreground "green")))
   ;;'(js2-function-param ((t :foreground "green")))
   ;;'(js2-instance-member ((t :foreground "green")))
   ;;'(js2-private-function-call ((t :foreground "green")))
   ))

(defun my-color-grandshell ()
  "Grandshell with a few mode-specific additoins."
  (interactive)
  (load-theme 'grandshell t)

  (custom-theme-set-faces
   'grandshell

   `(avy-lead-face ;; the first overlay char
     ((t (:foreground "green"
                      :slant normal
                      :weight bold
                      :inverse-video nil))))

   `(font-lock-comment-face
     ((t (:foreground "gray50" :slant italic))))
   `(font-lock-comment-delimiter-face
     ((t (:foreground "gray50"))))

   ;;This isn't a js2 error face. But externals tend to be an error since js2 doesn't find the them.
   '(js2-external-variable ((t :underline (:color "red" :style wave)
                               :background "black")))
   '(js2-error ((t :underline (:color "red" :style wave)
                   :background "dark red")))
   '(js2-function-call ((t :foreground "violet"))) ;;making same as font-lock-function-name-face
   '(js2-warning ((t :underline (:color "yellow" :style wave)
                     :background "navy blue")))
   ;; colors copied from grandshell-theme.el
   `(mode-line ((t (:foreground "#eee"
                                :background "#331133"
                                :box (:line-width -1 :style released-button)))))
   ;;colors copied from grandshell-theme.el
   `(mode-line-inactive ((t (:foreground "#643"
                                         :background "#110011"
                                         :weight light
                                         :box (:line-width -1 :style released-button)
                                         :inherit (mode-line)))))))

(defun my-color-gruvbox-dark ()
  (interactive)
  (load-theme 'gruvbox-dark t)

  (custom-theme-set-variables
   'gruvbox-dark
   `(fci-rule-color "#3d3d3d")

   `(evil-emacs-state-cursor '(bar "cyan"))
   `(evil-normal-state-cursor '(hollow "spring green"))
   `(evil-insert-state-cursor '(bar "spring green"))
   `(evil-visual-state-cursor '(hollow "yellow"))
   `(evil-operator-state-cursor '(hollow "spring green"))
   `(evil-replace-state-cursor '(hbar "spring green"))
   `(evil-motion-state-cursor '(box "spring green")))

  (custom-theme-set-faces
   'gruvbox-dark
   `(region ((t (:background "#59584E"))))
   '(fringe ((t (:background "black"))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))))

(defun my-color-gruvbox ()
  (interactive)
  (load-theme 'gruvbox t)

  (my-rainbow-parens-dark-bg)

  (custom-theme-set-faces
   'gruvbox
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))
   `(fringe
     ((t (:foreground "burlywood"
                      :background "black"))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))))

(defun my-color-kosmos ()
  (interactive)
  (load-theme 'kosmos t)

  (my-rainbow-parens-dark-bg)

  (let ((kosmos-fg "#bdbdbd")
        (kosmos-bg "#000000")
        (kosmos-bg-active "#102040")
        (kosmos-bg-inactive "#202020")
        (kosmos-keyword "#ffffff")
        (kosmos-str "#77cc77")
        (kosmos-comment "#50abab")
        (kosmos-gray "#777777")
        (kosmos-fg-todo "#bdabab")
        (kosmos-bg-todo "#775555")
        (kosmos-fg-done "#abbdab")
        (kosmos-bg-done "#557755")
        (kosmos-h1 "#d1d2d3"))

    (custom-theme-set-faces
     'kosmos
	 
      ;; default emacs completion.
     `(completions-common-part ((t (:foreground "#656565"))))
     `(completions-first-difference ((t (:foreground "green"))))

     `(show-paren-match ((t (:slant italic
                                    :bold t
                                    :strike-through t
                                    :background nil))))

     `(mode-line ((t (:background ,kosmos-bg-active
                                  :foreground ,kosmos-keyword
                                  :box (:line-width -1
                                                    :color "#ffffff"
                                                    :style released-button)))))
     `(mode-line-inactive ((t (:background ,kosmos-bg-inactive :foreground ,kosmos-gray
                                           :box (:line-width -1
                                                             :color ,kosmos-gray
                                                             :sytle pressed-button)))))
     `(show-paren-match ((t (:slant italic
                                    :bold t
                                    :strike-through t
                                    :background nil)))))))

(defun my-color-leuven ()
  (interactive)
  (load-theme 'leuven t)
  ;; (my-cursor-stuff :color-emacs "maroon" :color-evil "blue")
  ;; (let ((cur '(box "blue")))
  ;;   (setq evil-normal-state-cursor cur)
  ;;   (setq evil-visual-state-cursor '(hollow "blue"))
  ;;   (setq evil-operator-state-cursor cur))

  (custom-theme-set-variables
   'leuven
   `(fci-rule-color "gray97")

   `(evil-emacs-state-cursor    '(bar "maroon"))
   `(evil-normal-state-cursor   '(box "#0FB300"))
   `(evil-insert-state-cursor   '(bar "#0FB300"))
   `(evil-visual-state-cursor   '(hollow "blue"))
   `(evil-operator-state-cursor '(box "blue"))
   `(evil-replace-state-cursor  '(hbar "blue"))
   `(evil-motion-state-cursor   '(box)))


  (custom-theme-set-faces
   'leuven
   ;; `(default ((t (:foreground "black" :background ,mayan-smoke))))
   `(default ((t (:foreground "black"))))
   ;;`(default ((t (:foreground "black" :background ,"white"))))
   `(mode-line ((t (:box (:line-width -1 :color "#1A2F54")
                         :foreground "#85CEEB" :background "#335EA8"
                         :style released-button))))
   `(mode-line-inactive ((t (:box (:line-width -1 :color "#4E4E4C")
                                  :foreground "#F0F0EF" :background "#9B9C97"
                                  :style released-button))))


   '(js2-function-call ((t :foreground "blue"
                           )))
   '(js2-external-variable ((t :underline (:color "black" :style wave)
                               ;; :background "yellow"
                               )))
   `(js2-warning ((t :underline (:color "black" :style wave)
                     :strike-through t
                     :background "lemonchiffon"
                     )))
   `(js2-error ((t :underline (:color "blue" :style wave)
                   :strike-through t
                   :background "light salmon")))


   '(erc-timestamp-face ((t :foreground "purple" :weight bold)))
   '(fringe
     ((t (:foreground "#9B9B9B" :background "alice blue"))))
   '(leerzeichen ((t (:foreground "black" ;"#A8A800"
                                  :background "white" ;"#D4D4C8"
                                  :italic nil
                                  :bold nil
                                  ;;:box t
                                  ))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))

   `(font-lock-function-name-face ((t (:weight bold :foreground "#006699"))))
   `(font-lock-keyword-face ((t (:bold t :foreground "#0000FF")))) ; #3654DC
   `(font-lock-builtin-face ((t (:weight bold :foreground "#006FE0"))))

   `(avy-lead-face ;; if 1 highlight char, or for remaining highlight chars.
     ((t (:foreground "black"
                      :background "yellow"
                      :slant normal
                      :weight normal
                      :strike-through nil
                      :underline nil))))

   `(avy-lead-face-0 ;; the first overlay char if 2+
     ((t (:foreground "black"
                      :background "spring green"
                      :slant normal
                      :weight normal
                      :strike-through nil
                      :underline nil))))
   `(avy-lead-face-1 ;; ??? Maybe the 2cd overlay char if 3+ ???
     ((t (:foreground "black"
                      :background "red"
                      :slant normal
                      :weight normal
                      :strike-through nil
                      :underline nil))))


   '(completions-common-part ((t (:foreground "gray40"))))
   '(completions-first-difference ((t (:foreground "black"
                                                   :background "lemon chiffon"
                                                   :weight bold))))

   ;; ido colors from twilight-bright-theme.el
   ;; `(ido-only-match ((t (:foreground "#5f9411" :background "#eff8e9"))))
   ;; `(ido-subdir ((t (:foreground "#a66bab" :background "#f8f1f8"))))

   `(ido-only-match ((t (:foreground "darkgreen" :background "#eff8e9" :weight bold))))
   `(ido-subdir ((t (:foreground "purple" :background "#f8f1f8"))))
   `(ido-first-match ((t (:foreground "black" :background "lemonchiffon" :weight bold))))
   ;; `(ido-indicator ((t (:foreground "red" :background "black"))))


   '(rainbow-delimiters-depth-1-face ((t (:foreground "black" :background "gray94" :weight bold))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "black" :background "light cyan" :weight normal))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :background "#faEaEa" :weight normal))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple" :background "lavenderblush" :weight normal))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "black" :background "lemon chiffon" :weight normal))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta" :background "#EEEEFF" :weight normal))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray52" :weight normal))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "indianred3" :weight normal))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "orange" :background "#fff7ca" :weight normal))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "yellow" :background "black" :weight normal))))))

(defun my-color-majapahit ()
  (interactive)
  (load-theme 'majapahit-dark t)

  (my-rainbow-parens-dark-bg)

  ;;wrap mods in `custom-theme-set-faces' so they can be rolled back with `disable-theme'
  (custom-theme-set-faces
   'majapahit-dark

   `(mode-line
     ((t (:foreground "#F8F8F2"
                      :background "#3E3D31"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "#75715E"
                      :background "#272822"
                      :box (:line-width -1 :style released-button)))))

   '(isearch ((t :background "black"
                 :foreground "yellow"
                 :bold nil
                 :underline t)))
   '(lazy-highlight ((t
                      :background "black"
                      :foreground "cyan"
                      :bold nil
                      :underline t)))

   ;; override the hi-yellow face for printf escapes.
   '(hi-yellow
     ((t (:foreground "hotpink"
                      :weight bold))))

   '(sr-highlight-path-face
     ((t (:background "black" :foreground "light sky blue"
                      :bold t
                      :height 120))))
   '(sr-active-path-face
     ((t (:background "black" :foreground "light sky blue"
                      :bold t
                      :height 120))))
   '(sr-passive-path-face
     ((t (:background "black" :foreground "gray"
                      :bold t
                      :height 120))))

   ;; swiper/ivy faces
   '(ivy-current-match
     ((t (:background "black" :foreground "light sky blue"))))

   '(aw-leading-char-face               ; ace-window character.
     ((t (:foreground "spring green"
                      :background "black"
                      :height 400       ; big font
                      ))))
   ;; '(cursor ((t (:background "cyan"))))

   `(font-lock-comment-delimiter-face
     ((t (:foreground "darkolivegreen3"))))

   `(ace-jump-face-foreground
     ((t (:foreground "spring green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(avy-lead-face-0 ;; the first overlay char
     ((t (:foreground "spring green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(avy-lead-face-1 ;; for matched chars, but currently not used???? matches disapear at the moment.
     ((t (:foreground "green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(avy-lead-face ;;for the chars after the first?
     ((t (:foreground "spring green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))

   `(fringe
     ((t (:foreground "#eddcca"
                      :background "black"))))

   `(region
     ((t (:background "#69685E"))))     ;"#49483E"

   `(num3-face-even
     ((t (:underline nil
                     ;; :background "#99988E"
                     ;; :foreground "black"
                     :background "black"
                     :foreground "yellow green"
                     :bold nil))))
   '(leerzeichen ((t (:foreground "yellow4" ;"#A8A800"
                                  :background "black" ;"#D4D4C8"
                                  :italic nil
                                  :bold nil))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil)))))

  ;; remove the bold from rainbow-delimiters.
  ;; (dolist (f (face-list))
  ;;   (when (my-str-starts-with-p (symbol-name f)
  ;;                               "rainbow-delimiters-")
  ;;     (set-face-attribute f
  ;;                         nil ;; all frames
  ;;                         :weight 'normal)))
  )

(defun my-color-molokai ()
  (interactive)
  (load-theme 'molokai t)

  (my-rainbow-parens-dark-bg)

  (custom-theme-set-faces
   'molokai

   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))

   ;; swiper/ivy faces TODO: fix more ivy/swiper faces.
   '(ivy-current-match
     ((t (:background "black" :foreground "light sky blue"))))

   ;; '(font-lock-variable-name-face
   ;;   ((t (:foreground "#924040"))))
   ;; '(my-tilde-face
   ;;   ((t (:foreground "#523030"))))

   `(font-lock-comment-face
     ((t (:foreground "RosyBrown4" :slant italic))))
   `(font-lock-comment-delimiter-face
     ((t (:foreground "RosyBrown3"))))

   `(fringe
     ((t (:foreground "burlywood"
                      :background "black"))))

   `(mode-line
     ((t (:foreground "#F8F8F2"
                      :background "#3E3D31"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-inactive
     ((t (:foreground "#75715E"
                      :background "#272822"
                      :box (:line-width -1 :style released-button)))))

   ;; the buffer name
   '(mode-line-buffer-id ((t (:foreground nil :weight semi-bold))))

   `(avy-lead-face-0 ;; the first overlay char
     ((t (:foreground "spring green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(avy-lead-face-1 ;; for matched chars, but currently not used???? matches disapear at the moment.
     ((t (:foreground "green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(avy-lead-face ;;for the chars after the first?
     ((t (:foreground "spring green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))))

(defun my-color-monokai ()
  "Load the monokai theme with several adjustments."
  (interactive)
  (load-theme 'monokai t)

  (my-rainbow-parens-dark-bg)

  (custom-theme-set-faces
   'monokai
   ;;from VIM charcoal: hi Normal guifg=#ADC299 guibg=#35352B "*
   ;; `(default ((t (:background ,my-charcoal))))

   `(region
     ((t (:background "#69685E"))))

   `(erc-notice-face
     ((t (:foreground "dark gray"))))
   `(compilation-info
     ((t (:foreground "DarkOrange2"))))
   ;; `(cursor
   ;;   ((t (:foreground "black"
   ;;                    :background "green"))))
   `(font-lock-comment-face
     ((t (:foreground "#66A555"))))
   `(fringe
     ((t (:foreground "burlywood"
                      :background "black"))))

   `(ace-jump-face-foreground
     ((t (:foreground "spring green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))

   ;; s-mode-line-bg "#3E3D31"
   ;; s-mode-line-fg "#F8F8F2"
   `(mode-line
     ((t (:foreground "#F8F8F2"
                      :background "#3E3D31"
                      :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   ;; s-mode-line-inactive-fg "#75715E"
   ;; s-mode-line-inactive-bg "#272822"
   `(mode-line-inactive
     ((t (:foreground "#75715E"
                      :background "#272822"
                      :box (:line-width -1 :style released-button)))))


   ;; '(js2-error
   ;;   ((t (:foreground "red"
   ;;                    :underline t))))
   ;;highlight so i can see the slime function parameters highlight.
   '(highlight ((t (:foreground "spring green"
                                :background "black"))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))))

(defun my-color-niflheim ()
  "Load the zenburn theme created by Bozhidar Batsov.  Make a few extra mods too."
  (interactive)
  (load-theme 'niflheim t)

  (my-rainbow-parens-dark-bg)

  (let ((class '((class color) (min-colors 89)))
        (background "#303030")
        (dark "#202020")
        (fringe "#353535")
        (highlight "#454545")
        (comment "#929283")
        (light "#f6f3e8")
        (veryligh "fbfaf5")
        (grey "#666666")
        (grey-light "#aaaaaa")
        (grey-darker "#333333")
        (grey-dark "#252525")
        (orange "#ffcd8e")
        (orange-light "#ffedd5")
        (orange-2 "#f7af75")
        (orange-dark "#da8548")
        (orange-darker "#bd6626")
        (mode-line-inactive "#2a2a2a")
        (yellow-dark "#888833")
        (purple "#cbaaf5")
        (purple-light "#ddcaf6")
        (purple-dark "#7846b5")
        (purple-darker "#544568")
        (blue "#7ac1ff")
        (blue-alt "#1268b4")
        (blue-light "#aaccff")
        (blue-dark "#456981")
        (blue-darker "#3e4d58")
        (green "#789771")
        (green-2 "#70a56f")
        (green-3 "#92a65e")
        (green-4 "#83e1b2")
        (green-light "#aaeeab")
        (green-dark "#284437")
        (green-alt "#198754")
        (red "#ff6c6b")
        (red-light "#ff5b66")
        (red-alt "#981b1b")
        (red-dark "#553333")
        (default "#b8c4cf")
        (cursor-background "#b6c4cf"))

    (custom-theme-set-faces
     'niflheim

     `(mode-line ((,class (:background ,purple-darker
                                       :foreground ,light
                                       :box (:line-width -1 :color ,grey-light)))))
     `(mode-line-inactive ((,class (:background ,mode-line-inactive
                                                :foreground ,grey-light
                                                :box (:line-width -1 :color ,grey-light)))))

     `(whitespace-space-before-tab ((t (:background "orange"))))

     `(show-paren-match ((t (:slant italic
                                    :bold t
                                    :strike-through t
                                    :background nil)))))))

(defun my-color-spacemacs-light ()
  (interactive)
  (load-theme 'spacemacs-light t)

  (custom-theme-set-variables
   'spacemacs-light
   `(fci-rule-color "#4d4d4d"))

  (my-rainbow-parens-light-bg)

  (custom-theme-set-faces
   'spacemacs-light

   `(default ((t (:foreground "black" :background "#fffeFa"))))
   ;; `(default ((t (:foreground "black"))))

   '(highlight-indent-guides-odd-face
     ((t (:background "floral white"))))

   '(highlight-indent-guides-even-face
     ((t (:inherit default))))

   `(whitespace-space-before-tab ((t (:background "orange"))))

   `(fringe ((t (:background "gray96"))))

   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))

   `(font-lock-string-face ((t (:foreground "deeppink"))))
   `(font-lock-constant-face ((t (:foreground "darkgreen" :background "lemonchiffon" :slant italic)))) ;; `(font-lock-comment-face ((t (:foreground "orangered" :background nil :slant italic))))
   `(font-lock-comment-face ((t (:foreground "#05b505" :background nil :slant italic))))
   ;; `(font-lock-comment-delimiter-face ((t (:foreground "purple" :background nil :slant italic))))

   `(mode-line ((t (:foreground "#655370" :background "#e7e5eb" :box (:color "#b3b9be"  :line-width -1)))))
   `(mode-line-inactive ((t (:foreground "#655370" :background "#fbf8ef" :box (:color "#b3b9be" :line-width -1)))))
   ;; `(mode-line ((t (:box (:line-width -1 :style released-button)))))
   ;; `(mode-line-inactive ((t (:box (:line-width -1 :sytle pressed-button)))))
   ))

(defun my-color-sunburn ()
  (interactive)
  (load-theme 'sunburn t)
  (my-rainbow-parens-dark-bg)

  (custom-theme-set-variables
   'sunburn

   `(fci-rule-color "#4d4d4d")

   `(evil-emacs-state-cursor    '(bar "cyan"))
   `(evil-normal-state-cursor   '(hollow "spring green"))
   `(evil-insert-state-cursor   '(bar "spring green"))
   `(evil-visual-state-cursor   '(hollow "orange"))
   `(evil-operator-state-cursor '(hollow "spring green"))
   `(evil-replace-state-cursor  '(hbar "spring green"))
   `(evil-motion-state-cursor   '(box "spring green")))

  (custom-theme-set-faces
   'sunburn

   `(region ((t (:background "#69685E"))))

   '(completions-common-part ((t (:foreground "gray60"))))
   '(completions-first-difference ((t (:foreground "yellow"))))))

(defun my-color-tango-dark ()
  (interactive)
  (load-theme 'tango-dark t)

  (my-rainbow-parens-dark-bg)

  ;;wrap mods in `custom-theme-set-faces' so they can be rolled back with `disable-theme'
  (custom-theme-set-faces
   'tango-dark

   `(mode-line ((t (:foreground "#aaaaaa"
                                :background "#331133"
                                :box (:line-width -1 :style released-button)))))
   ;;colors copied from grandshell-theme.el
   `(mode-line-inactive ((t (:foreground "#643"
                                         :background "#110011"
                                         :weight light
                                         :box (:line-width -1 :style released-button)
                                         :inherit (mode-line)))))
   ;; separates windows.
   '(vertical-border ((t (:foreground "gray30"))))

   '(highlight-indent-guides-odd-face
     ((t (:background "#3F3F3C" ;; "#40403A"
                      :weight bold))))

   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))))

(defun my-color-tommyh ()
  (interactive)
  (load-theme 'tommyh t)

  (my-rainbow-parens-light-bg2)

  (custom-theme-set-faces
   'tommyh

   '(erc-timestamp-face
     ((t (:foreground "#b8574e"))))
   '(my-tilde-face
     ((t (:foreground "black"))))
   '(fringe
     ((t (:foreground "black"
                      :background "#74a6bd"))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background "yellow"))))))

(defun my-color-overcast ()
  (interactive)
  (load-theme 'overcast t)
  (custom-theme-set-faces
   'overcast

   (my-rainbow-parens-dark-bg-bold)
   ;; (color-comment "#005353") "#70C3C3"
   `(font-lock-comment-face ((t (:foreground "#50A3A3"))))
   `(font-lock-doc-face ((t (:inherit font-lock-string-face))))))

(defun my-color-zenburn ()
  "Load the zenburn theme created by Bozhidar Batsov.  Make a few extra mods too."
  (interactive)
  (load-theme 'zenburn t)

  ;; set variables (that are not faces) using `custom-theme-set-variables'. It
  ;; will allow them to be rolled back automatically when the theme is later
  ;; disabled.
  (custom-theme-set-variables
   'zenburn
   `(fci-rule-color "#4d4d4d")

   `(evil-emacs-state-cursor    '(bar "cyan"))
   `(evil-normal-state-cursor   '(hollow "spring green"))
   `(evil-insert-state-cursor   '(bar "spring green"))
   `(evil-visual-state-cursor   '(hollow "orange"))
   `(evil-operator-state-cursor '(box "indian red"))
   `(evil-replace-state-cursor  '(hbar "spring green"))
   `(evil-motion-state-cursor   '(box "spring green"))
   ;; TODO: finish this off.
   `(ivy-switch-buffer-faces-alist '((emacs-lisp-mode . '((t (:foreground "yellow"))))
                                     (lisp-interaction-mode . '((t (:foreground "yellow"))))
                                     (dired-mode . ivy-subdir)
                                     (org-mode . org-level-4))))


  ;;wrap mods in `custom-theme-set-faces' so they can be rolled back with `disable-theme'
  (custom-theme-set-faces
   'zenburn
   ;;from VIM charcoal: hi Normal guifg=#ADC299 guibg=#35352B "*
   ;;`(default ((t (:foreground "#CFC5A9" :background "#35352B"))))
   ;;`(default ((t (:foreground "#CCCCBC" :background "#35352B"))))

   ;;'(cursor ((t (:foreground "blue" :background "red"))))
   ;; `(mode-line
   ;;   ((t (:foreground "#8FB28F"
   ;;                    :background "#032203"
   ;;                    ;;:underline "dark yellow"
   ;;                    ;;:overline "green"
   ;;                    :box (:line-width -1 :style released-button)))
   ;;    (t :inverse-video t)))

   ;; '(hl-line
   ;;   ((t (:background "black"))))

   ;; '(minibuffer-prompt
   ;;   ((t (:foreground "spring green"))))

   ;; `(header-line
   ;;   ((t (:foreground "lemon chiffon"
   ;;                    :background "#303030"
   ;;                    :weight bold
   ;;                    :box (:line-width -1 :style released-button)))))


   '(completions-common-part ((t (:foreground "gray60"))))
   '(completions-first-difference ((t (:foreground "spring green"))))

   ;; used in adoc-mode
   '(markup-meta-face ((t (:foreground "yellow green"))))
   '(markup-internal-reference-face ((t (:foreground "yellow green"))))
   '(markup-meta-hide-face ((t (:foreground "powder blue"))))

   '(cider-fringe-good-face ((t (:foreground "lime green"))))
   '(cider-result-overlay-face ((t (:foreground "orange"))))

   '(org-agenda-calendar-event ((t (:background "black"))))

   ;; separates windows.
   '(vertical-border ((t (:foreground "gray50"))))

   '(highlight-indent-guides-odd-face
     ((t (:background "#3F3F3C" ;; "#40403A"
                      :weight bold))))

   '(highlight-indent-guides-even-face
     ((t (:background "#3D3D3D" ;; "#3B3B3B"
                      :weight bold))))

   ;; override the hi-yellow face for printf escapes.
   '(hi-yellow
     ((t (:foreground "hotpink"
                      :weight bold))))

   '(sr-highlight-path-face
     ((t (:background "black" :foreground "light sky blue"
                      :bold t
                      :height 120))))
   '(sr-active-path-face
     ((t (:background "black" :foreground "light sky blue"
                      :bold t
                      :height 120))))
   '(sr-passive-path-face
     ((t (:background "black" :foreground "gray"
                      :bold t
                      :height 120))))

   ;; swiper/ivy faces
   '(ivy-current-match
     ((t (:background "black" :foreground "light sky blue"))))

   '(js2-highlight-vars-face
     ((t (:background "#69685E"))))

   '(slime-repl-inputed-output-face
     ((t (:foreground "light sky blue"))))
   '(nxml-tag-delimiter
     ((t (:foreground "#83D0FF" :weight bold))))
   '(nxml-element-local-name
     ((t (:foreground "#93E0E3" :weight bold))))
   '(org-hide
     ((t (:foreground "#5F5F5F"))))
   '(my-tilde-face
     ((t (:foreground "dark gray"))))
   '(aw-leading-char-face               ; ace-window character.
     ((t (:foreground "spring green"
                      :background "black"
                      :height 400       ; big font
                      ))))
   '(cursor ((t (:background "cyan"))))
   '(hydra-face-red
     ((t (:foreground "green" :bold t))))
   '(hydra-face-blue
     ((t (:foreground "cyan" :bold t))))
   '(hydra-face-amaranth
     ((t (:foreground "green" :bold t))))

   ;; used by lisp doc strings
   ;; `(font-lock-doc-face ((t (:inherit font-lock-keyword-face :weight normal))))
   ;; `(font-lock-doc-face ((t (:foreground "moccasin"))))

   `(font-lock-comment-face
     ((t (:foreground "#8FB28F" :slant italic))))
   `(font-lock-comment-delimiter-face
     ((t (:foreground "darkolivegreen3"))))

   `(ace-jump-face-foreground
     ((t (:foreground "spring green"
                      :slant normal
                      :weight normal
                      :inverse-video nil))))
   `(avy-lead-face-0 ;; the first overlay char
     ((t (:foreground "spring green"
                      :background "black"
                      :slant normal
                      :weight normal
                      :inverse-video nil
                      :underline nil))))
   `(avy-lead-face-1 ;; for matched chars, but currently not used???? matches disapear at the moment.
     ((t (:foreground "green"
                      :background "black"
                      :slant normal
                      :weight normal
                      :inverse-video nil
                      :underline nil))))
   `(avy-lead-face ;;for the chars after the first?
     ((t (:foreground "spring green"
                      :background "black"
                      :slant normal
                      :weight normal
                      :inverse-video nil
                      :underline nil))))
   ;; `(avy-background-face
   ;;   ((t (:foreground "white"
   ;;                    :slant normal
   ;;                    :weight normal
   ;;                    :inverse-video nil))))

   ;;highlight so i can see the slime function parameters highlight.
   '(highlight ((t (:foreground "spring green"
                                :background "black"))))

   `(region
     ((t (:background "#69685E"))))     ;"#49483E"
   ;; '(region ((t :background "black")))
   '(isearch ((t :background "yellow"
                 :foreground "black"
                 :bold t
                 :underline nil)))
   ;; the non-selected matches from isearch
   '(lazy-highlight ((t
                      :background "black"
                      :foreground "cyan"
                      :bold nil
                      :underline t)))
   ;;This isn't a js2 error face. But externals tend to be an error since js2 doesn't find the them.
   '(js2-external-variable ((t :underline (:color "red" :style wave)
                               :background "black")))
   '(js2-error ((t :underline (:color "red" :style wave)
                   :background "dark red")))
   '(js2-function-call ((t :foreground "#93E0E3"))) ;;making same as font-lock-function-name-face
   '(js2-warning ((t :underline (:color "yellow" :style wave)
                     :background "navy blue")))

   ;;'(js2-private-member ((t :foreground "green")))
   ;;'(js2-function-param ((t :foreground "green")))
   ;;'(js2-instance-member ((t :foreground "green")))
   ;;'(js2-private-function-call ((t :foreground "green")))

   ;; '(num3-face-odd ((t)))
   ;; '(num3-face-even ((t (:underline t :background "black"))))
   `(num3-face-even
     ((t (:underline nil
                     ;; :background "#99988E"
                     ;; :foreground "black"
                     :background "black"
                     :foreground "yellow green"
                     :bold nil))))
   '(leerzeichen ((t (:foreground "yellow4"           ;"#A8A800"
                                  :background "black" ;"#D4D4C8"
                                  :italic nil
                                  :bold nil))))
   `(show-paren-match ((t (:slant italic
                                  :bold t
                                  :strike-through t
                                  :background nil))))


   `(ido-subdir ((t (:foreground "light blue" :background "black"))))

   '(rainbow-delimiters-depth-1-face ((t (:foreground "orange red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "plum"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "lawn green"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "white"))))
   ;;'(rainbow-delimiters-depth-8-face ((t (:foreground "seagreen1"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "hot pink" :background "#2F2F2F"
                                                      ))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "burlywood3"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "sienna" :background "black"))))))

(provide 'my-color-theme-mods)