(progn ;; JUMPrestore
  ;; tricks to improve startup time.
  ;; from https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_st
  ;; eps_to_speed_up_emacs_start/
  ;; on mac-mini-m1-2021 this improves start up from 1.3s to 0.62s

  (defvar gc-cons-threshold-backup gc-cons-threshold)
  (setq gc-cons-threshold (if (version< emacs-version "24.4")
                              200000000
                            2000000000))

  (defvar file-name-handler-alist-backup file-name-handler-alist)
  (setq file-name-handler-alist nil)

  ;; restore original values at end of init.el.
  ;; sort of like my own dynamic binding. I don't want to wrap the entire
  ;; config in a giant let.
  )


;; (when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
;; (when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
;; (when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
;; (when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode 0))
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


;; sacrifice proper display of right-to-left languages for performance.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)


;; remove title bar. to fit more lines of code on the screen
(unless (eq system-type 'windows-nt) ;; locks window on Windows!
  (add-to-list 'default-frame-alist '(undecorated . t)))

;; on mac thius makes the window a fixed size!!! Rectangle plugin can't resize emacs.
;; (progn
;;   (add-to-list 'default-frame-alist '(undecorated . t))
;;   (add-to-list 'default-frame-alist '(width . 1.0))
;;   (add-to-list 'default-frame-alist '(height . 1.0)))


(setq package-enable-at-startup nil)


;; Avoid resizing the GUI frame when font changes.
;; see https://old.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn
;; _fast/
(setq frame-inhibit-implied-resize t)
;; (setq frame-resize-pixelwise t)



;; On windows set init file locations to Local instead of Roaming
;; NOTE: buffer-file-name is null in early-init.el so don't rely on that!
(when (and (eq system-type 'windows-nt)
           (string-match "Roaming" (expand-file-name user-emacs-directory)))

  (let* ((dir-roam user-emacs-directory)
         (dir-appdata (file-name-parent-directory (file-name-parent-directory dir-roam)))
         (dir-local (concat dir-appdata "Local/"))
         (dir-user (file-name-parent-directory dir-appdata)))

    (setq user-init-file (concat dir-local ".emacs.d/init") ; no .el
          user-emacs-directory (concat dir-local ".emacs.d/")
          default-directory dir-user)
    (setenv "HOME" dir-local) ; so ~ expands correctly
    ;; `(:dir-roam ,dir-roam
    ;;             :dir-appdata ,dir-appdata
    ;;             :dir-local ,dir-local
    ;;             :dir-user ,dir-user
    ;;             :user-init-file ,user-init-file
    ;;             :user-emacs-directory ,user-emacs-directory
    ;;             :default-directory ,default-directory)
    ))



(setq custom-theme-directory "~/.emacs.d/notElpa/themes/") ;color themes.
;; some themes require helper files so add themes dir to load-path.
(push custom-theme-directory load-path)
(push "~/.emacs.d/notElpa/themes/replace-colorthemes/" custom-theme-load-path)



;; currently used computer. (manually set)
;; Used to conditionally set computer specific options, and paths.
;; NOTE: When setting up emacs on a new computer create file
;; ~/.emacs.d/my-curr-computer.txt
;; Then type the name of the symbol (see `my-computers') in the text file.
;; The file should contain 1 line and no whitespace. The text will be converted
;; to a symbol.
(defconst my-curr-computer
  (let ((curr-comp-file "~/.emacs.d/my-curr-computer.txt"))
    (ignore-errors
      (when (file-exists-p curr-comp-file)
        (intern (with-temp-buffer
                  (insert-file-contents curr-comp-file)
                  (buffer-string))))))
  "The computer running this Emacs.  Identified by a flag file.
nil if computer is unknown.
Specific configs may be made based on the computer.")


(cond
 ((eq my-curr-computer 'work-laptop-mac)
  (let ((charcoal-color-cnt 16777216)) ; will be 0 in early-init so shadow it
    (load-theme 'charcoal t))
  (push
   '(font . "-*-Menlo-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
   default-frame-alist))

 ((eq my-curr-computer 'mac-mini-m1-2021)
  (let ((charcoal-color-cnt 16777216)) ; will be 0 in early-init so shadow it
    (load-theme 'charcoal t))
  ;; transparent bg on mac, iterm2. see the picture set as bg image in iterm2.
  ;; (when (not my-graphic-p)
  ;;   (my-color-transparent-bg))

  ;; (custom-theme-set-faces
  ;;  'ultimate
  ;;  `(default ((t :background "ivory3"))))
  (push
   ;; '(font . "-*-Menlo-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
   ;; '(font . "-*-JetBrains Mono NL-light-normal-normal-*-15-*-*-*-m-0-iso10646-1")
   ;; '(font . "-*-Ubuntu Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
   ;; '(font . "-*-Ubuntu Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
   ;; '(font . "-*-Ubuntu Mono-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
   '(font . "Ubuntu Mono-19:antialias=false:hinting=false")
   ;; '(font . "Ubuntu Mono-19:antialias=false:hinting=false:weight=bold")
   ;; '(font . "-*-Iosevka-regular-normal-normal-*-17-*-*-*-m-0-iso10646-1")
   ;; '(font . "-*-Iosevka-light-normal-normal-*-16-*-*-*-m-0-iso10646-1")
   ;; '(font . "-*-Unifont-normal-normal-normal-*-19-*-*-*-p-0-iso10646-1")
   ;; '(font . "-*-Unifont-bold-normal-normal-*-19-*-*-*-p-0-iso10646-1")
   default-frame-alist))

 ((eq my-curr-computer 'wild-dog)
  (let ((charcoal-color-cnt 16777216)) ; will be 0 in early-init so shadow it
    (load-theme 'charcoal t))

  ;; faster than `set-frame-font' for setting the font?
  ;; see https://old.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_da
  ;; mn_fast/
  (push
   '(font . "-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-9")
   default-frame-alist))

 ((eq my-curr-computer 'work-laptop-2019)
  (let ((charcoal-color-cnt 16777216)) ; will be 0 in early-init so shadow it
    (load-theme 'charcoal t))
  ;; (load-theme 'ultimate t)
  ;; (set-background-color "#E5E1C3")
  ;; (custom-theme-set-faces
  ;;  'ultimate
  ;;  `(default ((t :background "white"))))
  ;; (my-rainbow-parens-light-bg3)
  (push
   '(font
     .
     ;; "-raster-Fixedsys-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
     ;; "-raster-Terminal-normal-normal-normal-mono-18-*-*-*-c-*-ms-oemlatin"
     ;; "-raster-Terminus-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
     ;; "-raster-Terminus-bold-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
     ;; "-raster-Terminus-bold-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
     "-raster-Terminus-bold-normal-normal-mono-14-*-*-*-c-*-iso8859-1"
     ;; "-outline-Iosevka Medium-medium-normal-normal-mono-14-*-*-*-c-*-iso10646-1"
     ;; "-outline-Iosevka Medium-medium-normal-normal-mono-16-*-*-*-c-*-iso10646-1"
     ;; "-outline-Lucida Console-normal-normal-normal-mono-13-*-*-*-c-*-iso10646-1"
     ;; "-outline-Ubuntu Mono-bold-normal-normal-mono-16-*-*-*-c-*-iso10646-1"
     ;; "-outline-Ubuntu Mono-bold-normal-normal-mono-15-*-*-*-c-*-iso10646-1"
     ;; "-outline-Ubuntu Mono-normal-normal-normal-mono-14-*-*-*-c-*-iso10646-1"
     ;; "-outline-JetBrains Mono NL ExtraBold-extrabold-normal-normal-mono-13-*-*-*-c-*-iso10646-1"
     ;; "-outline-JetBrains Mono NL-normal-normal-normal-mono-13-*-*-*-c-*-iso10646-1"
     )
   default-frame-alist)
  ;; (set-frame-font
  ;;  "-raster-Dina-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1")
  )

 ((eq my-curr-computer 'leyna-laptop)
  (let ((charcoal-color-cnt 16777216)) ; will be 0 in early-init so shadow it
    (load-theme 'charcoal t))
  (set-frame-font
   "-raster-Terminal-normal-normal-normal-mono-18-*-*-*-c-*-ms-oemlatin"))

 ((eq my-curr-computer 'a-laptop-old)
  (let ((charcoal-color-cnt 16777216)) ; will be 0 in early-init so shadow it
    (load-theme 'charcoal t))
  (set-frame-font
   "-raster-Fixedsys-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"))

 ((eq my-curr-computer 'hp-tower-2009)
  (let ((charcoal-color-cnt 16777216)) ; will be 0 in early-init so shadow it
    (load-theme 'charcoal t)))

 ((eq my-curr-computer 'a-laptop-faster)
  (let ((charcoal-color-cnt 16777216)) ; will be 0 in early-init so shadow it
    (load-theme 'charcoal t))
  (set-frame-font
   "-unknown-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))

 ;; unknown windows computer.
 ((and (null my-curr-computer)
       (eq system-type 'windows-nt))
  (let ((charcoal-color-cnt 16777216)) ; will be 0 in early-init so shadow it
    (load-theme 'charcoal t))
  (set-frame-font
   "-raster-Fixedsys-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1")))
