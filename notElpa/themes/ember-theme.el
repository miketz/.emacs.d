;;; ember-theme.el --- Warm graphite, one coral spark -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: Hossam Saraya
;; URL: https://github.com/ember-theme/emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (doom-themes "2.3.0"))
;; Keywords: faces themes

;; Copyright (c) 2026 Hossam Saraya
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.

;;; Commentary:

;; Ember is a warm, nearly monochrome theme built on graphite tones with a
;; single vivid coral accent.  Eight muted colors balanced to equal perceptual
;; brightness — and one coral that cuts through like a match struck in the dark.
;;
;; Three flavors:
;;   - `ember'       — dark graphite (L10% background)
;;   - `ember-soft'  — lifted graphite (L13% background)
;;   - `ember-light' — warm ivory (L86% background)

;;; Code:

(require 'doom-themes)

(def-doom-theme ember
    "Warm graphite with ivory mist and a single coral ember."

  ;; name        default                 256           16
  ;; ── Background ramp ── H45 warm olive-grey, S4-8% (nearly neutral)
  ;; Identity: #2A2B2A Graphite — extract H45 warm tint, keep almost neutral
  ((bg         '("#1c1b19" nil          nil          ))  ; H45  S6%  L10%
   (bg-alt     '("#222120" nil          nil          ))  ; H30  S4%  L13%
   (base0      '("#151412" "black"      "black"      ))  ; H40  S8%  L7%
   (base1      '("#1c1b19" "#1e1e1e"    "brightblack" )) ; H45  S6%  L10%
   (base2      '("#252422" "#2e2e2e"    "brightblack" )) ; H40  S5%  L14%
   (base3      '("#2e2d2a" "#2F3237"    "brightblack" )) ; H42  S5%  L17%
   (base4      '("#3e3c38" "#4f5b66"    "brightblack" )) ; H42  S5%  L23%
   (base5      '("#585550" "#65737E"    "brightblack" )) ; H40  S5%  L33%
   (base6      '("#706c61" "#6b6b6b"    "brightblack" )) ; H43  S8%  L41%  — Dim Grey from seed!
   (base7      '("#908a7e" "#979797"    "brightblack" )) ; H42  S8%  L53%
   (base8      '("#b8b0a0" "#dfdfdf"    "white"      ))  ; H40  S12% L68%
   (fg         '("#d8d0c0" "#c0c5ce"    "brightwhite" )) ; H42  S16% L82%  — ivory warmth
   (fg-alt     '("#b0a898" "#a0a0a0"    "white"      ))  ; H38  S12% L64%

   (grey       base4)

   ;; ── Accent colors ──
   ;; Strategy: nearly monochrome warm palette with ONE vivid coral pop.
   ;; Most accents are olive/brown/sand at S15-30%, L*~52-58
   ;; Coral is the hero at S50%, stands alone as the only saturated color.
   (red        '("#e08060" "#ff6655"    "red"          )) ; H18  S55% L63%  L*~58  — CORAL (hero!)
   (orange     '("#c09058" "#dd8844"    "brightred"    )) ; H30  S42% L55%  L*~55
   (yellow     '("#c8b468" "#ECBE7B"    "yellow"       )) ; H45  S42% L60%  L*~58  — warm gold
   (green      '("#8a9868" "#99bb77"    "green"        )) ; H78  S22% L50%  L*~54  — olive, muted
   (blue       '("#7890a0" "#5599bb"    "brightblue"   )) ; H205 S18% L55%  L*~53  — steel, muted
   (dark-blue  '("#586878" "#446688"    "blue"         )) ; H210 S16% L41%  L*~42
   (teal       '("#789080" "#44b9b1"    "brightgreen"  )) ; H150 S12% L52%  L*~52  — sage, barely there
   (cyan       '("#80a090" "#00cc99"    "brightcyan"   )) ; H150 S14% L56%  L*~54  — sage
   (dark-cyan  '("#607868" "#5699AF"    "cyan"         )) ; H140 S12% L42%  L*~42
   (magenta    '("#b07878" "#cc6688"    "brightmagenta")) ; H0   S25% L58%  L*~52  — dusty rose
   (violet     '("#988090" "#8877cc"    "magenta"      )) ; H315 S12% L55%  L*~50  — mauve, faint

   ;; ── Semantic mappings ──
   ;; Coral keywords = the ember that catches your eye
   ;; Everything else recedes into warm neutrals
   (highlight      red)
   (vertical-bar   base4)
   (selection       base3)
   (builtin        red)
   (comments       base6)
   (doc-comments   base7)
   (constants      orange)
   (functions      yellow)
   (keywords       red)
   (methods        cyan)
   (operators      base7)
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
   (hidden     `(,(car bg-alt) "black" "black"))
   (-modeline-bright nil)
   (-modeline-pad    4)

   (modeline-fg     fg)
   (modeline-fg-alt base6)
   (modeline-bg     base1)
   (modeline-bg-l   base1)
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
   ((font-lock-comment-face &override)       :foreground base6 :slant 'italic)
   ((font-lock-doc-face &override)           :foreground base7 :slant 'italic)
   ((font-lock-operator-face &override)      :foreground base7)
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
    :box `(:line-width ,-modeline-pad :color ,bg))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :overline base4
    :box `(:line-width ,-modeline-pad :color ,modeline-bg-inactive))
   (mode-line-emphasis :foreground red)
   (doom-modeline-bar :background red)
   (doom-modeline-buffer-file :foreground fg :weight 'bold)

   ;; ── VTerm ANSI ──
   (vterm-color-black   :foreground base2   :background base4)
   (vterm-color-red     :foreground red     :background red)
   (vterm-color-green   :foreground green   :background green)
   (vterm-color-yellow  :foreground yellow  :background yellow)
   (vterm-color-blue    :foreground blue    :background blue)
   (vterm-color-magenta :foreground magenta :background magenta)
   (vterm-color-cyan    :foreground cyan    :background cyan)
   (vterm-color-white   :foreground fg      :background fg)

   ;; ── Completion (Corfu) ──
   (corfu-default          :background base3 :foreground fg)
   (corfu-current          :background base4 :foreground red :weight 'bold :extend t)
   (corfu-bar              :background red)
   (corfu-border           :background base5)
   (corfu-annotations      :foreground base6)
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
   (vertico-current :background base4 :foreground red :weight 'bold)
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
   (hl-line :background "#242320")

   ;; ── Goggles (pulse on change) ──
   (goggles-changed :background "#302c1a")   ; warm yellow tint
   (goggles-added   :background "#1e2c1a")   ; olive tint
   (goggles-removed :background "#302020")   ; coral tint

   ;; ── Misc UI ──
   (highlight :background base4)
   (lazy-highlight :background base5 :foreground red)
   (isearch :background red :foreground bg :weight 'bold)
   (evil-ex-lazy-highlight :background base5 :foreground red)
   (region :background base4)
   (secondary-selection :background base2)
   (link :foreground red :underline t)
   (link-visited :foreground magenta :underline t)
   (tooltip :background base2 :foreground fg)

   ;; ── Org ──
   (org-block            :background base2 :extend t)
   (org-block-begin-line :foreground base5 :slant 'italic :background base1)
   (org-block-end-line   :foreground base5 :slant 'italic :background base1)
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
   ((outline-6 &override) :foreground base7)

   ;; ── Markdown ──
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten bg 0.03))

   ;; ── CSS ──
   (css-proprietary-property :foreground orange)
   (css-property             :foreground fg)
   (css-selector             :foreground red :weight 'bold)

   ;; ── Org Agenda ──
   (org-agenda-structure :foreground red :weight 'bold)
   (org-agenda-date :foreground fg :weight 'bold)
   (org-agenda-date-today :foreground red :weight 'ultra-bold)
   (org-agenda-date-weekend :foreground base6)
   (org-agenda-date-weekend-today :foreground red :weight 'bold)
   (org-agenda-done :foreground base5 :slant 'italic)
   (org-agenda-dimmed-todo-face :foreground base5)
   (org-agenda-current-time :foreground red :weight 'bold)
   (org-agenda-clocking :background base3 :weight 'bold)
   (org-scheduled :foreground fg)
   (org-scheduled-today :foreground fg :weight 'bold)
   (org-scheduled-previously :foreground orange)
   (org-upcoming-deadline :foreground red :weight 'bold)
   (org-upcoming-distant-deadline :foreground base6)
   (org-warning :foreground red :weight 'bold)
   (org-super-agenda-header :foreground red :weight 'bold)

   ;; ── Org TODO / Tags / Properties ──
   (org-todo :foreground red :weight 'bold)
   (org-done :foreground base5 :weight 'bold :strike-through t)
   (org-headline-done :foreground base5 :strike-through t)
   (org-priority :foreground orange :weight 'bold)
   (org-tag :foreground base6 :weight 'normal)
   (org-date :foreground blue :underline t)
   (org-special-keyword :foreground base6)
   (org-checkbox :foreground red :weight 'bold)
   (org-checkbox-statistics-todo :foreground red)
   (org-checkbox-statistics-done :foreground base5)
   (org-code :foreground orange :background base2)
   (org-verbatim :foreground green)
   (org-table :foreground base7)
   (org-formula :foreground orange)
   (org-drawer :foreground base6)
   (org-property-value :foreground base7)
   (org-link :foreground red :underline t)

   ;; ── Mu4e (Email) ──
   (mu4e-header-face :foreground fg-alt)
   (mu4e-header-highlight-face :background base3 :weight 'bold)
   (mu4e-header-marks-face :foreground orange)
   (mu4e-header-title-face :foreground red :weight 'bold)
   (mu4e-header-field-face :foreground base5)
   (mu4e-unread-face :foreground red :weight 'bold)
   (mu4e-flagged-face :foreground red :weight 'bold)
   (mu4e-replied-face :foreground base7 :slant 'italic)
   (mu4e-forwarded-face :foreground blue)
   (mu4e-draft-face :foreground yellow :weight 'bold)
   (mu4e-trashed-face :foreground base5 :strike-through t)
   (mu4e-related-face :foreground base6)
   (mu4e-contact-face :foreground red)
   (mu4e-header-key-face :foreground red :weight 'bold)
   (mu4e-header-value-face :foreground fg)
   (mu4e-special-header-value-face :foreground orange)
   (mu4e-compose-separator-face :foreground base4)
   (mu4e-link-face :foreground red :underline t)
   (mu4e-url-number-face :foreground orange :weight 'bold)
   (mu4e-cited-1-face :foreground base7)
   (mu4e-cited-2-face :foreground base6)
   (mu4e-cited-3-face :foreground base5)
   (mu4e-thread-fold-face :foreground base5 :slant 'italic)
   (mu4e-context-face :foreground red :weight 'bold)
   (mu4e-modeline-face :foreground red)
   (mu4e-highlight-face :foreground red :weight 'bold)
   (mu4e-title-face :foreground red :weight 'bold)
   (mu4e-footer-face :foreground base5)
   (mu4e-ok-face :foreground green :weight 'bold)
   (mu4e-warning-face :foreground orange :weight 'bold)
   (mu4e-system-face :foreground base6 :slant 'italic)
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

(provide 'ember-theme)

;;; ember-theme.el ends here
