;;; temple-os-theme.el --- The Authentic Holy Covenant Theme -*- lexical-binding: t; -*-

;; Author: Senka07
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.0"))
;; Keywords: faces, theme, retro, cga
;; URL: https://github.com/Senka07/temple-os-emacs-theme

;;; Commentary:
;; The authentic TempleOS theme (CGA/EGA).
;; Features a high-contrast white background, black text, and blue borders,
;; strictly adhering to Terry Davis's divine 16-color palette.

;;; Code:

(deftheme temple-os
  "The authentic TempleOS theme (CGA/EGA). White background, black text, and blue borders.")

(let ((class '((class color) (min-colors 16)))
      ;; Pure CGA Palette (High Intensity)
      (temple-black      "#000000")
      (temple-blue       "#0000AA")
      (temple-green      "#00AA00")
      (temple-cyan       "#00AAAA")
      (temple-red        "#AA0000")
      (temple-magenta    "#AA00AA")
      (temple-brown      "#AA5500")
      (temple-light-grey "#AAAAAA")
      (temple-dark-grey  "#555555")
      (temple-br-blue    "#5555FF")
      (temple-br-green   "#55FF55")
      (temple-br-cyan    "#55FFFF")
      (temple-br-red     "#FF5555")
      (temple-br-magenta "#FF55FF")
      (temple-yellow     "#FFFF55")
      (temple-white      "#FFFFFF"))

  (custom-theme-set-faces
   'temple-os

   ;; --- Temple Base (White Background Editor) ---
   ;; Note: :family removed to respect user configuration. Uncomment if strictly necessary.
   `(default ((,class (:foreground ,temple-black :background ,temple-white)))) ;; :family "TempleOS"
   `(cursor ((,class (:background ,temple-br-blue :foreground ,temple-white))))
   `(fringe ((,class (:background ,temple-white :foreground ,temple-black))))
   `(region ((,class (:background ,temple-br-cyan :foreground ,temple-black))))

   ;; --- System Interface (Imitating TempleOS Top Bar) ---
   ;; Status bar is Blue with blinking White text (simulated with bold)
   `(mode-line ((,class (:background ,temple-blue :foreground ,temple-white :box (:line-width 2 :color ,temple-blue) :weight bold))))
   `(mode-line-inactive ((,class (:background ,temple-light-grey :foreground ,temple-black :box (:line-width 2 :color ,temple-light-grey)))))
   `(header-line ((,class (:background ,temple-red :foreground ,temple-yellow :weight bold))))
   `(vertical-border ((,class (:foreground ,temple-blue))))
   `(minibuffer-prompt ((,class (:foreground ,temple-br-blue :weight bold))))

   ;; --- DolDoc & HolyC Syntax ---
   ;; TempleOS uses heavy red for links and DolDoc markup
   `(font-lock-builtin-face ((,class (:foreground ,temple-br-blue :weight bold))))
   `(font-lock-comment-face ((,class (:foreground ,temple-green))))      ; Green comments (common in DOS)
   `(font-lock-constant-face ((,class (:foreground ,temple-br-magenta))))
   `(font-lock-function-name-face ((,class (:foreground ,temple-black :background ,temple-light-grey :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,temple-blue :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,temple-red))))         ; Strings in Red
   `(font-lock-type-face ((,class (:foreground ,temple-brown :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,temple-black))))
   `(font-lock-warning-face ((,class (:foreground ,temple-br-red :background ,temple-yellow :weight bold))))

   ;; --- Links and Buttons (DolDoc Style) ---
   `(link ((,class (:foreground ,temple-red :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,temple-brown :underline t))))
   `(button ((,class (:background ,temple-light-grey :foreground ,temple-black :box (:style released-button)))))

   ;; --- Line Numbering ---
   `(line-number ((,class (:foreground ,temple-blue :background ,temple-white))))
   `(line-number-current-line ((,class (:foreground ,temple-br-blue :background ,temple-white :weight bold))))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'temple-os)

;;; temple-os-theme.el ends here
