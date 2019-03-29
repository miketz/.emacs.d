;;; my-pdump --- pdump helper functions -*- lexical-binding: t -*-

;;; Commentary:
;;; Helper functions to prepare for and make a portable Emacs dump.

;;; Code:


;; Function `my-handle-weird-theme-setups' lives in init.el.
;; That may change soon.
(declare-function my-handle-weird-theme-setups 'suppress)


(defun my-load-everything-for-pdump ()
  "Load libraries so they will be part of the saved image from pdumper.
For faster subsequent start up."
  (interactive)
  ;; measure time spent loading libs.
  (let ((start (float-time)))
    ;; byte compile my libs before loading them. So fast versions will be
    ;; loaded for the dump.
    (byte-recompile-directory
     (expand-file-name "~/.emacs.d/notElpa/mine/")
     0  ;; 0 means compile .el files if .elc is missing.
     t) ;; t means force re-compile even if the .elc is up-to-date. May be
        ;; useful if the Emacs version changed and should have an .elc compiled
        ;; again to be compatible.

    ;; my libs
    (my-handle-weird-theme-setups)
    (require 'mode-on-region)
    (require 'my-hydras)
    (require 'my-rand)
    (require 'my-type-tutor)
    (require 'my-window-search)
    (require 'my-square-one)
    (require 'my-screen-real-estate)
    (require 'my-load-theme)
    (require 'my-grep)
    (require 'my-date-stuff)
    (require 'my-color-theme-mods)
    (require 'my-code-snippet-url)
    (require 'my-font-cycle)
    (require 'my-ruler)
    (require 'my-cycle-line-position)
    (require 'my-font-stuff)
    (require 'my-horizontal-scroll)
    (require 'my-line-nums)
    (require 'my-misc)
    (require 'my-pdump) ; we're already in here, but just to be complete.

    ;; 3rd party libs.
    (require 'company)
    (require 'swiper)
    (require 'ivy)
    (require 'counsel)
    (require 'flx)
    (require 'ido)
    (require 'smex)
    (require 'eshell)
    (require 'expand-region)
    (require 'ibuffer)
    (require 'org)
    (require 'org-agenda)
    (require 'org-element)
    (require 'org-macro)
    (require 'org-footnote)
    (require 'org-pcomplete)
    (require 'org-list)
    (require 'org-faces)
    (require 'org-entities)
    (require 'org-version)
    (require 'org-src)
    (require 'org-compat)
    (require 'org-macs)
    (require 'org-loaddefs)
    (require 'calendar)
    (require 'slime)
    (require 'eww)
    (require 'js)
    (require 'js2-mode)
    (require 'js2-highlight-vars)
    (require 'json-mode)
    (require 'cc-mode)
    (require 'num3-mode)
    (require 'powershell)
    (require 'leerzeichen)
    (require 'fill-column-indicator)
    (require 'hydra)
    (require 'avy)
    (require 'lispy)
    (require 'elisp-slime-nav)
    (require 'electric-spacing)
    (require 'highlight-tail)
    (require 'highlight-indent-guides)
    (require 'smart-tabs-mode)
    (require 'lua-mode)
    (require 'typescript-mode)
    (require 'erc)
    (require 'erc-hl-nicks)
    (require 'flycheck)
    (require 'unkillable-scratch)
    (require 'web-mode)
    (require 'css-mode)
    (require 'htmlize)
    (require 'rainbow-mode)
    (require 'paredit)
    (provide 'sql)
    (require 'sql-indent)
    (require 'yasnippet)
    (require 'ispell)
    (require 'grep)
    (require 'bookmark)
    (require 'darkroom)
    (require 'ediff)
    (require 'vimrc-mode)
    (require 'vc)
    (require 'ace-window)
    (require 'ace-link)
    (require 'dired)
    (require 'nxml-mode)
    ;; (require 'winner) ; exclude winner. It breaks the dump.
    ;; (require 'magit) ; exclude magit until hook performance is resolved.


    ;; print elapsed time
    (message
     (format "Finished loading libs. elapsed seconds: %f"
             (time-to-seconds (time-subtract (float-time)
                                             start))))))

;; after the dump is made start emacs with the command:
;; emacs --dump-file=~/.emacs.d/dump.pdmp
(defun my-make-pdump ()
  "Make a pdump.  You may want to call `my-load-everything-for-pdump' first."
  (interactive)
  (let ((dmp-file "~/.emacs.d/dump.pdmp"))
    (dump-emacs-portable dmp-file)
    (message "Emacs image dumped to %s" dmp-file)))


(provide 'my-pdump)

;;; my-pdump.el ends here
