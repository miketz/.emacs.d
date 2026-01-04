;;; doom-stranger-things-theme.el --- A theme inspired by Stranger Things -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Claude Code
;; Created: January 2, 2026
;; URL: https://codeberg.org/juboba/emacs-config/src/branch/main/doom-stranger-things-theme.el
;;
;;; Commentary:
;;
;; A dark, moody theme inspired by the Netflix series Stranger Things.
;; Features the iconic red title aesthetic, dark blues and blacks reminiscent
;; of the Upside Down, and neon 80s colors. Think vintage synth-wave meets
;; supernatural horror.
;;
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-stranger-things-theme nil
  "Options for doom-stranger-things theme."
  :group 'doom-themes)

(defcustom doom-stranger-things-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-stranger-things-theme
  :type 'boolean)

(defcustom doom-stranger-things-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-stranger-things-theme
  :type 'boolean)

(defcustom doom-stranger-things-comment-bg doom-stranger-things-brighter-comments
  "If non-nil, comments will have a subtle, darker background."
  :group 'doom-stranger-things-theme
  :type 'boolean)

(defcustom doom-stranger-things-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line."
  :group 'doom-stranger-things-theme
  :type '(choice integer boolean))

;;
;;; Theme definition

(def-doom-theme doom-stranger-things
  "A dark, atmospheric theme inspired by Stranger Things with neon 80s vibes."

  ;; name        default   256       16
  ((bg         '("#000000" "#000000" nil           ))  ; Black
   (bg-alt     '("#0d0d15" "#0d0d15" nil           ))  ; Slightly lighter dark
   (base0      '("#121218" "#121218" "black"       ))  ; Deepest shadow
   (base1      '("#161622" "#161622" "brightblack" ))  ; Dark blue-black
   (base2      '("#1a1a2e" "#1a1a2e" "brightblack" ))  ; Deep blue shade
   (base3      '("#242438" "#242438" "brightblack" ))  ; Muted blue
   (base4      '("#2e2e45" "#2e2e45" "brightblack" ))  ; Darker muted blue
   (base5      '("#3d3d5c" "#3d3d5c" "brightblack" ))  ; Mid-tone blue
   (base6      '("#52526e" "#52526e" "brightblack" ))  ; Lighter mid-tone
   (base7      '("#6b6b8a" "#6b6b8a" "brightblack" ))  ; Even lighter
   (base8      '("#9999b3" "#9999b3" "white"       ))  ; Light blue-grey
   (fg-alt     '("#b3b3cc" "#b3b3cc" "white"       ))  ; Muted foreground
   (fg         '("#e6e6f2" "#e6e6f2" "brightwhite" ))  ; Bright foreground

   (grey       '("#5a5a75" "#5a5a75" "gray"          ))
   (red        '("#ff0000" "#ff0000" "red"          ))  ; Iconic Stranger Things red
   (orange     '("#ff6b35" "#ff6b35" "brightred"    ))  ; Warm neon orange
   (green      '("#39ff14" "#39ff14" "green"        ))  ; Neon green (alphabet wall lights)
   (teal       '("#00ffcc" "#00ffcc" "brightgreen"  ))  ; Electric teal
   (yellow     '("#ffed4e" "#ffed4e" "yellow"       ))  ; Warning lights yellow
   (blue       '("#00b4d8" "#00b4d8" "brightblue"   ))  ; Electric blue
   (dark-blue  '("#0077b6" "#0077b6" "blue"         ))  ; Deep electric blue
   (magenta    '("#ff006e" "#ff006e" "magenta"      ))  ; Hot magenta (80s neon)
   (violet     '("#bb00ff" "#bb00ff" "brightmagenta"))  ; Purple neon
   (cyan       '("#00ffff" "#00ffff" "brightcyan"   ))  ; Bright cyan
   (dark-cyan  '("#0096c7" "#0096c7" "cyan"         ))  ; Muted cyan

   ;; face categories -- required for all themes
   (highlight      red)                ; That iconic red glow
   (vertical-bar   (doom-darken base2 0.3))
   (selection      base4)
   (builtin        cyan)               ; Electric blue-cyan for built-ins
   (comments       (if doom-stranger-things-brighter-comments dark-cyan grey))
   (doc-comments   teal)
   (constants      violet)             ; Purple for constants
   (functions      blue)               ; Electric blue functions
   (keywords       red)                ; Red keywords for that Stranger Things feel
   (methods        cyan)
   (operators      magenta)            ; Hot magenta operators
   (type           yellow)             ; Yellow types
   (strings        green)              ; Neon green strings
   (variables      fg-alt)             ; Subtle variables
   (numbers        orange)             ; Orange numbers
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-stranger-things-brighter-modeline)
   (-modeline-pad
    (when doom-stranger-things-padded-modeline
      (if (integerp doom-stranger-things-padded-modeline) doom-stranger-things-padded-modeline 4)))

   (modeline-fg     fg)
   (modeline-fg-alt base6)

   (modeline-bg
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg) 0.1) ,@(cdr bg)))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :background (if doom-stranger-things-comment-bg (doom-lighten bg 0.05)))
   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :foreground red :bold t)  ; Red current line
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; company
   (company-tooltip-selection :background base4 :foreground red)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground cyan)
   (css-selector             :foreground blue)

   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-modified :inherit 'bold :foreground red)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-error :background red)
   (doom-modeline-project-dir :bold t :foreground cyan)
   (doom-modeline-project-parent-dir :foreground cyan)

   ;;;; elscreen
   (elscreen-tab-other-screen-face :background bg :foreground base4)

   ;;;; evil
   (evil-ex-lazy-highlight :background base3 :foreground fg :weight 'bold)
   (evil-ex-substitute-matches :background base0 :foreground red :weight 'bold :strike-through t)
   (evil-ex-substitute-replacement :background base0 :foreground green :weight 'bold)
   (evil-search-highlight-persist-highlight-face :inherit 'lazy-highlight :background base3)

   ;;;; helm
   (helm-bookmark-w3m :foreground violet)
   (helm-buffer-not-saved :foreground violet)
   (helm-buffer-process :foreground orange)
   (helm-buffer-saved-out :foreground fg)
   (helm-buffer-size :foreground fg)
   (helm-candidate-number :foreground bg :background fg)
   (helm-ff-directory :foreground cyan :weight 'bold)
   (helm-ff-executable :foreground yellow :inherit 'italic)
   (helm-ff-invalid-symlink :foreground magenta :weight 'bold)
   (helm-ff-prefix :foreground bg :background magenta)
   (helm-ff-symlink :foreground magenta :weight 'bold)
   (helm-grep-finish :foreground base2)
   (helm-grep-running :foreground red)
   (helm-header :foreground base2 :underline nil :box nil)
   (helm-match :foreground red :weight 'bold)
   (helm-moccur-buffer :foreground cyan)
   (helm-selection :background base3 :foreground red :weight 'bold)
   (helm-source-go-package-godoc-description :foreground green)
   (helm-source-header :foreground fg)
   (helm-swoop-target-line-block-face :background base3 :foreground fg)
   (helm-swoop-target-line-face :foreground red :weight 'bold)
   (helm-swoop-target-word-face :background blue :foreground fg)
   (helm-visible-mark :foreground bg :background base3)

   ;;;; highlight-quoted-mode
   (highlight-quoted-symbol :foreground violet)
   (highlight-quoted-quote  :foreground magenta)

   ;;;; js2-mode
   (js2-function-param :foreground fg-alt)
   (js2-jsdoc-tag :foreground cyan)
   (js2-object-property :foreground violet)

   ;;;; lsp-mode
   (lsp-face-highlight-read :background base3 :foreground cyan :weight 'bold)
   (lsp-face-highlight-textual :background base3 :foreground fg :weight 'bold)
   (lsp-face-highlight-write :background base3 :foreground red :weight 'bold)
   (lsp-ui-doc-background :background base0)
   (lsp-ui-sideline-code-action :foreground yellow)

   ;;;; magit
   (magit-blame-heading :foreground orange :background base3)
   (magit-diff-removed :foreground (doom-darken red 0.2) :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight :foreground red :background (doom-blend red bg 0.2) :bold bold)

   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;;;; org-mode
   (org-block :background base0)
   (org-block-begin-line :foreground grey :background base0)
   (org-code :foreground cyan)
   (org-document-info-keyword :foreground grey)
   (org-level-1 :foreground red :bold t :height 1.3)
   (org-level-2 :foreground magenta :bold t :height 1.2)
   (org-level-3 :foreground violet :bold t :height 1.1)
   (org-level-4 :foreground cyan :bold t)
   (org-level-5 :foreground blue)
   (org-level-6 :foreground teal)
   (org-level-7 :foreground green)
   (org-level-8 :foreground yellow)
   (org-link :foreground blue :underline t)
   (org-todo :foreground red :bold t)
   (org-done :foreground green :bold t)

   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground red)
   (rainbow-delimiters-depth-2-face :foreground cyan)
   (rainbow-delimiters-depth-3-face :foreground magenta)
   (rainbow-delimiters-depth-4-face :foreground violet)
   (rainbow-delimiters-depth-5-face :foreground blue)
   (rainbow-delimiters-depth-6-face :foreground teal)
   (rainbow-delimiters-depth-7-face :foreground green)

   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;;;; treemacs
   (treemacs-root-face :foreground red :weight 'bold :height 1.2)
   (treemacs-directory-face :foreground cyan)
   (treemacs-git-modified-face :foreground orange)

   ;;;; web-mode
   (web-mode-current-element-highlight-face :background base4 :foreground fg)
   (web-mode-html-attr-name-face :foreground cyan)
   (web-mode-html-attr-value-face :foreground green)
   (web-mode-html-tag-face :foreground blue)
   (web-mode-json-key-face :foreground cyan)
   (web-mode-json-string-face :foreground green)))

;;; doom-stranger-things-theme.el ends here
