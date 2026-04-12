;;; ember-light-theme.el --- Warm ivory, one coral spark -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: Hossam Saraya
;; URL: https://github.com/ember-theme/emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (doom-themes "2.3.0"))
;; Keywords: faces themes

;; Copyright (c) 2026 Hossam Saraya
;; MIT License (see ember-theme.el for full text)

;;; Commentary:

;; Ember Light — warm ivory variant with darkened accents for contrast.
;; See `ember-theme' for the full description.

;;; Code:

(require 'doom-themes)

(def-doom-theme ember-light
    "Warm ivory with a single coral ember — the light side of graphite."

  ;; name        default                 256           16
  ;; ── Background ramp ── Same shade as d8d0c0 (ivory), lightened for bg
  ;; Parchment paper warmth, not clinical white.
  ((bg         '("#e6daC4" nil          nil          ))  ; H38  S24% L86%  — warm parchment
   (bg-alt     '("#ded2b8" "#eeeeee"   nil          ))  ; H38  S22% L82%  — deeper parchment
   (base0      '("#eee4d0" "#ffffff"   "white"      ))  ; H38  S22% L90%
   (base1      '("#e6dac4" "#ffffff"   "brightwhite" )) ; = bg
   (base2      '("#ded2b8" "#eeeeee"   "brightwhite" )) ; = bg-alt
   (base3      '("#d4c8ae" "#e4e4e4"   "brightwhite" )) ; H38  S22% L78%
   (base4      '("#c4b89e" "#cccccc"   "white"      ))  ; H38  S20% L72%
   (base5      '("#9c9280" "#aaaaaa"   "brightblack" )) ; H38  S16% L58%
   (base6      '("#746a58" "#6b6b6b"   "brightblack" )) ; H38  S16% L42%
   (base7      '("#544c3c" "#555555"   "brightblack" )) ; H38  S18% L29%
   (base8      '("#342c20" "#333333"   "black"      ))  ; H38  S20% L17%
   (fg         '("#282418" "#2d2d2d"   "black"      ))  ; H38  S20% L14%  — warm near-black
   (fg-alt     '("#4c4434" "#555555"   "brightblack" )) ; H38  S18% L26%

   (grey       base5)

   ;; ── Accent colors ──
   ;; Same hues as dark variant, darkened for WCAG AA (4.5:1) on cream.
   ;; Coral is still the hero — everything else stays muted.
   (red        '("#b84c30" "#cc3333"    "red"          )) ; H14  S60% L45%  — CORAL (hero!)
   (orange     '("#946030" "#aa6622"    "brightred"    )) ; H28  S52% L38%
   (yellow     '("#7a6820" "#887722"    "yellow"       )) ; H48  S60% L30%  — dark gold
   (green      '("#4a6830" "#558833"    "green"        )) ; H90  S36% L30%  — dark olive
   (blue       '("#3a6080" "#336688"    "brightblue"   )) ; H205 S36% L36%  — dark steel
   (dark-blue  '("#2a4860" "#224466"    "blue"         )) ; H210 S38% L27%
   (teal       '("#386858" "#337755"    "brightgreen"  )) ; H150 S30% L31%  — dark sage
   (cyan       '("#407060" "#338866"    "brightcyan"   )) ; H150 S28% L34%
   (dark-cyan  '("#285848" "#226644"    "cyan"         )) ; H150 S30% L25%
   (magenta    '("#905050" "#884444"    "brightmagenta")) ; H0   S30% L44%  — dark dusty rose
   (violet     '("#706070" "#665577"    "magenta"      )) ; H280 S10% L40%  — dark mauve

   ;; ── Semantic mappings ──
   (highlight      red)
   (vertical-bar   base4)
   (selection       base3)
   (builtin        red)
   (comments       base5)
   (doc-comments   base6)
   (constants      orange)
   (functions      yellow)
   (keywords       red)
   (methods        cyan)
   (operators      base6)
   (type           yellow)
   (strings        green)
   (variables      fg)
   (numbers        orange)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red)

   ;; ── Modeline ──
   (hidden     `(,(car bg-alt) "white" "white"))
   (-modeline-bright nil)
   (-modeline-pad    4)

   (modeline-fg     fg)
   (modeline-fg-alt base6)
   (modeline-bg     base2)
   (modeline-bg-l   base2)
   (modeline-bg-inactive   bg)
   (modeline-bg-inactive-l bg))

  ;; ── Face overrides ── FULL coverage
  (
   ;; ── Font-lock ──
   ((font-lock-keyword-face &override)       :foreground red :weight 'bold)
   ((font-lock-builtin-face &override)       :foreground red :weight 'bold)
   ((font-lock-function-name-face &override) :foreground yellow :weight 'semibold)
   ((font-lock-function-call-face &override) :foreground yellow)
   ((font-lock-type-face &override)          :foreground yellow :weight 'bold :slant 'italic)
   ((font-lock-constant-face &override)      :foreground orange :weight 'semibold)
   ((font-lock-variable-name-face &override) :foreground fg)
   ((font-lock-variable-use-face &override)  :foreground fg-alt)
   ((font-lock-string-face &override)        :foreground green)
   ((font-lock-number-face &override)        :foreground orange :weight 'semibold)
   ((font-lock-comment-face &override)       :foreground base5 :slant 'italic)
   ((font-lock-doc-face &override)           :foreground base6 :slant 'italic)
   ((font-lock-operator-face &override)      :foreground base6)
   ((font-lock-property-name-face &override) :foreground orange)
   ((font-lock-preprocessor-face &override)  :foreground red :weight 'bold)
   ((font-lock-warning-face &override)       :foreground red :weight 'bold)

   ;; ── UI ──
   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :foreground red :weight 'bold)
   (cursor :background red)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :overline base4
    :box `(:line-width ,-modeline-pad :color ,bg-alt))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :overline base4
    :box `(:line-width ,-modeline-pad :color ,modeline-bg-inactive))
   (mode-line-emphasis :foreground red)
   (doom-modeline-bar :background red)
   (doom-modeline-buffer-file :foreground fg :weight 'bold)

   ;; ── VTerm ANSI ──
   (vterm-color-black   :foreground base4   :background base6)
   (vterm-color-red     :foreground red     :background red)
   (vterm-color-green   :foreground green   :background green)
   (vterm-color-yellow  :foreground yellow  :background yellow)
   (vterm-color-blue    :foreground blue    :background blue)
   (vterm-color-magenta :foreground magenta :background magenta)
   (vterm-color-cyan    :foreground cyan    :background cyan)
   (vterm-color-white   :foreground fg      :background fg)

   ;; ── Completion (Corfu) ──
   (corfu-default          :background base2 :foreground fg)
   (corfu-current          :background base3 :foreground red :weight 'bold :extend t)
   (corfu-bar              :background red)
   (corfu-border           :background base4)
   (corfu-annotations      :foreground base5)
   (corfu-deprecated       :foreground base5 :strike-through t)
   (internal-border        :background base4)
   ;; ── LSP UI ──
   (lsp-ui-doc-background  :background base2)
   (lsp-ui-doc-header      :background base3 :foreground fg :weight 'bold)
   (completions-common-part    :foreground red :weight 'bold)
   (completions-first-difference :foreground orange)
   (orderless-match-face-0 :foreground red :weight 'bold)
   (orderless-match-face-1 :foreground orange :weight 'bold)
   (orderless-match-face-2 :foreground yellow)
   (orderless-match-face-3 :foreground green)

   ;; ── Vertico / Minibuffer ──
   (vertico-current :background base3 :foreground red :weight 'bold)
   ((minibuffer-prompt &override) :foreground red :weight 'bold)

   ;; ── Which-key / Transient ──
   (which-key-key-face :foreground red)
   (which-key-command-description-face :foreground fg)
   (which-key-group-description-face :foreground orange)
   (transient-key :foreground red :weight 'bold)

   ;; ── Treemacs / Dired ──
   (treemacs-root-face :foreground red :weight 'ultra-bold)
   (treemacs-directory-face :foreground fg)
   (treemacs-file-face :foreground fg-alt)
   (treemacs-git-modified-face :foreground yellow)
   (treemacs-git-added-face :foreground green)

   ;; ── Current line ──
   (hl-line :background "#ded2b6")

   ;; ── Goggles (pulse on change) ──
   (goggles-changed :background "#d8cca0")   ; warm yellow tint
   (goggles-added   :background "#c8d4a8")   ; olive tint
   (goggles-removed :background "#dcc0b0")   ; coral tint

   ;; ── Misc UI ──
   (highlight :background base3)
   (lazy-highlight :background base4 :foreground red)
   (isearch :background red :foreground bg :weight 'bold)
   (evil-ex-lazy-highlight :background base4 :foreground red)
   (region :background base3)
   (secondary-selection :background base2)
   (link :foreground red :underline t)
   (link-visited :foreground magenta :underline t)
   (tooltip :background base2 :foreground fg)

   ;; ── Org ──
   (org-block            :background base2 :extend t)
   (org-block-begin-line :foreground base5 :slant 'italic :background bg-alt)
   (org-block-end-line   :foreground base5 :slant 'italic :background bg-alt)
   (org-quote            :background base2 :extend t :slant 'italic)
   (org-verse            :background base2 :extend t)
   (org-ellipsis         :underline nil :foreground base5)
   (org-hide             :foreground bg)

   ;; ── Outline headings ── coral pop on level 1, rest fades to warm neutrals
   ((outline-1 &override) :foreground red :weight 'ultra-bold)
   ((outline-2 &override) :foreground orange :weight 'bold)
   ((outline-3 &override) :foreground yellow)
   ((outline-4 &override) :foreground green)
   ((outline-5 &override) :foreground blue)
   ((outline-6 &override) :foreground base6)

   ;; ── Markdown ──
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-darken bg 0.03))

   ;; ── CSS ──
   (css-proprietary-property :foreground orange)
   (css-property             :foreground fg)
   (css-selector             :foreground red :weight 'bold)

   ;; ── Org Agenda ──
   (org-agenda-structure :foreground red :weight 'bold)
   (org-agenda-date :foreground fg :weight 'bold)
   (org-agenda-date-today :foreground red :weight 'ultra-bold)
   (org-agenda-date-weekend :foreground base5)
   (org-agenda-date-weekend-today :foreground red :weight 'bold)
   (org-agenda-done :foreground base5 :slant 'italic)
   (org-agenda-dimmed-todo-face :foreground base5)
   (org-agenda-current-time :foreground red :weight 'bold)
   (org-agenda-clocking :background base3 :weight 'bold)
   (org-scheduled :foreground fg)
   (org-scheduled-today :foreground fg :weight 'bold)
   (org-scheduled-previously :foreground orange)
   (org-upcoming-deadline :foreground red :weight 'bold)
   (org-upcoming-distant-deadline :foreground base5)
   (org-warning :foreground red :weight 'bold)
   (org-super-agenda-header :foreground red :weight 'bold)

   ;; ── Org TODO / Tags / Properties ──
   (org-todo :foreground red :weight 'bold)
   (org-done :foreground base5 :weight 'bold :strike-through t)
   (org-headline-done :foreground base5 :strike-through t)
   (org-priority :foreground orange :weight 'bold)
   (org-tag :foreground base5 :weight 'normal)
   (org-date :foreground blue :underline t)
   (org-special-keyword :foreground base5)
   (org-checkbox :foreground red :weight 'bold)
   (org-checkbox-statistics-todo :foreground red)
   (org-checkbox-statistics-done :foreground base5)
   (org-code :foreground orange :background base2)
   (org-verbatim :foreground green)
   (org-table :foreground base7)
   (org-formula :foreground orange)
   (org-drawer :foreground base5)
   (org-property-value :foreground base6)
   (org-link :foreground red :underline t)

   ;; ── Mu4e (Email) ──
   (mu4e-header-face :foreground fg-alt)
   (mu4e-header-highlight-face :background base3 :weight 'bold)
   (mu4e-header-marks-face :foreground orange)
   (mu4e-header-title-face :foreground red :weight 'bold)
   (mu4e-header-field-face :foreground base5)
   (mu4e-unread-face :foreground red :weight 'bold)
   (mu4e-flagged-face :foreground red :weight 'bold)
   (mu4e-replied-face :foreground base6 :slant 'italic)
   (mu4e-forwarded-face :foreground blue)
   (mu4e-draft-face :foreground yellow :weight 'bold)
   (mu4e-trashed-face :foreground base5 :strike-through t)
   (mu4e-related-face :foreground base5)
   (mu4e-contact-face :foreground red)
   (mu4e-header-key-face :foreground red :weight 'bold)
   (mu4e-header-value-face :foreground fg)
   (mu4e-special-header-value-face :foreground orange)
   (mu4e-compose-separator-face :foreground base4)
   (mu4e-link-face :foreground red :underline t)
   (mu4e-url-number-face :foreground orange :weight 'bold)
   (mu4e-cited-1-face :foreground base6)
   (mu4e-cited-2-face :foreground base5)
   (mu4e-cited-3-face :foreground base4)
   (mu4e-thread-fold-face :foreground base5 :slant 'italic)
   (mu4e-context-face :foreground red :weight 'bold)
   (mu4e-modeline-face :foreground red)
   (mu4e-highlight-face :foreground red :weight 'bold)
   (mu4e-title-face :foreground red :weight 'bold)
   (mu4e-footer-face :foreground base5)
   (mu4e-ok-face :foreground green :weight 'bold)
   (mu4e-warning-face :foreground orange :weight 'bold)
   (mu4e-system-face :foreground base5 :slant 'italic)
   (mu4e-region-code :background base2)

   ;; ── Solaire ──
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box `(:line-width ,-modeline-pad :color ,modeline-bg-l))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l))))

;;; dlock-ember-light-theme.el ends here

(provide 'ember-light-theme)

;;; ember-light-theme.el ends here
