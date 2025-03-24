;; -*- lexical-binding: t -*-

(require 'my-font-stuff)
(require 'expand-region)

;; (defhydra hydra-zoom (global-map "<f2>")
;;   "zoom"
;;   ;; The property name ":color" is misleading.
;;   ;; :color blue makes hydra-mode exit after execution, like evil-leader.
;;   ;; :color red stays in mode.
;;   ("i" text-scale-increase "in" :color red)
;;   ("o" text-scale-decrease "out" :color blue))

;; (defhydra hydra-leader (evil-normal-state-map "\\")
;;   "cmd"
;;   ("h" backward-char)
;;   ("j" next-line)
;;   ("k" previous-line)
;;   ("l" forward-char)
;;   ("x" eval-defun "evalD")
;;   ("e" eval-last-sexp "eval"))

;; (global-set-key
;;  (kbd "C-M-o")
;;  (defhydra hydra-window ;;()
;;    (
;;     ;; :pre
;;     ;; (set-cursor-color "purple")
;;     ;; :post
;;     ;; (set-cursor-color "green")
;;     :color amaranth ;keep the hydra active when a unbound key is accidentally pressed.
;;            )
;;    "window"
;;    ("h" windmove-left)
;;    ("j" windmove-down)
;;    ("k" windmove-up)
;;    ("l" windmove-right)
;;    ("H" (lambda ()
;;           (interactive)
;;           (enlarge-window-horizontally 15)))
;;    ("J" (lambda ()
;;           (interactive)
;;           (shrink-window 10)))
;;    ("K" (lambda ()
;;           (interactive)
;;           (enlarge-window 10)))
;;    ("L" (lambda ()
;;           (interactive)
;;           (shrink-window-horizontally 15)))
;;    ("e" evil-window-split) ;keeps sizes balanced as it splits like in Vim.
;;    ("E" evil-window-vsplit);keeps sizes balanced as it splits like in Vim.
;;    ("S" (lambda ()
;;           (interactive)
;;           (split-window-right)
;;           (windmove-right))
;;     "vert")
;;    ("s" (lambda ()
;;           (interactive)
;;           (split-window-below)
;;           (windmove-down))
;;     "horz")
;;    ("d" (lambda ()
;;           (interactive)
;;           (delete-window))
;;     "del")
;;    ("o" other-window)
;;    ("n" next-buffer)
;;    ("p" previous-buffer)
;;    ("b" balance-windows)
;;    ;;("K" kill-this-buffer)
;;    ("x" maximize-window "max")
;;    ("," delete-other-windows "one")
;;    ("q" nil "cancel") ;nil for function is an automatic blue head.
;;    ))

;; horizontal scroll test 3333-=--------------------------------------------------------------------------------------------------aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBCCCCCCCCCcccccccccccccccccccccccccccccccccccccDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1111111111111111111111111111111111111111111112222222222222222222222222222222222222222233333333333333333333333333333333444444444444444444444444444444444455555555555555555555
;;easy 1 key scrolling
(defhydra hydra-easyscroll (:color amaranth)
  "scroll"
  ("v" scroll-up-command)
  ("M-v" scroll-down-command)
  ;; ("f" scroll-left)
  ;; ("b" scroll-right)
  ("," beginning-of-buffer)
  ("." end-of-buffer)

  ("j" evil-scroll-line-down)
  ("k" evil-scroll-line-up)
  ;; ("h" (lambda ()
  ;;        (interactive)
  ;;        (evil-scroll-column-left 20)))
  ;; ("l" (lambda ()
  ;;        (interactive)
  ;;        (evil-scroll-column-right 20)))
  ("h" my-scroll-right)
  ("l" my-scroll-left)

  ("e" move-end-of-line)
  ("C-e" move-end-of-line)
  ("a" move-beginning-of-line)
  ("C-a" move-beginning-of-line)

  ("C-g" nil nil)
  ("q" nil))

(defhydra my-hydra-smerge (:color amaranth)
  "Hydra for smerge-mode."
  ("n" smerge-next)
  ("p" smerge-prev)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("b" smerge-keep-base)
  ("a" smerge-keep-all)

  ("," delete-other-windows) ; make 1 window so i can see the conflicts.

  ("C-g" nil nil)
  ("q" nil))


(defhydra my-hydra-font (:color amaranth)
  "Hydra for changing font size/style."
  ("j" (lambda ()
         (interactive)
         (my-change-font-size t)))
  ("k" (lambda ()
         (interactive)
         (my-change-font-size nil)))
  ("b" (lambda ()
         (interactive)
         (custom-set-faces
          '(default ((t (:weight bold)))))
         ;; refresh screen.
         (when (fboundp 'my-w32-run) ; TODO: make it work on non-Windows machines.
           (my-w32-run 'restore-curr-frame)
           (my-w32-run 'max))))
  ("n" (lambda ()
         (interactive)
         (custom-set-faces
          '(default ((t (:weight normal)))))
         ;; refresh screen.
         (when (fboundp 'my-w32-run) ; TODO: make it work on non-Windows machines.
           (my-w32-run 'restore-curr-frame)
           (my-w32-run 'max))))
  ("i" (lambda ()
         (interactive)
         (custom-set-faces
          '(default ((t (:slant italic)))))))
  ("u" (lambda ()
         (interactive)
         (custom-set-faces
          '(default ((t (:slant normal)))))))

  ("C-g" nil nil)
  ("q" nil))

;; (defhydra my-hydra-hs (:color amaranth)
;;   "Hydra for hs-minor-mode."
;;   ("f" hs-hide-block)
;;   ("j" hs-show-block)

;;   ("d" hs-hide-level)
;;   ("k" hs-show-block)

;;   ("s" hs-hide-all)
;;   ("l" hs-show-all)

;;   ("C-g" nil nil)
;;   ("q" nil))

;; avoid moving hand to arrow keys for barf/slurp
(defhydra hydra-paredit ()
  "paredit"
  ("h" paredit-forward-barf-sexp)
  ("l" paredit-forward-slurp-sexp)
  ("H" paredit-backward-slurp-sexp)
  ("L" paredit-backward-barf-sexp)
  ;; ("f" paredit-forward)
  ;; ("b" paredit-backward)
  ("\\" nil)
  ("q" nil))
;;(key-chord-define evil-normal-state-map "c," #'hydra-paredit/body)


;; ;; avoid moving hand to arrow keys for barf/slurp
;; (defhydra hydra-smartparens ()
;;   "smartparens"
;;   ("<" sp-forward-barf-sexp)
;;   (">" sp-forward-slurp-sexp)
;;   ("," sp-backward-slurp-sexp)
;;   ("." sp-backward-barf-sexp)
;;   ("q" nil))


(defhydra hydra-window ;;()
  (;; :pre ;;executes before each head.
   ;; (progn (message "executed pre")
   ;;        (my-cycle-light-bg))
   ;; :post ;;executes on exit from body, not exit from a head.
   ;; (progn (message "executed post")
   ;;        (my-cycle-light-bg))
   :color amaranth ;keep the hydra active when a unbound key is accidentally pressed.
          )
  "window"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("H" evil-window-move-far-left)
  ("J" evil-window-move-very-bottom)
  ("K" evil-window-move-very-top)
  ("L" evil-window-move-far-right)
  (">" (lambda ()
         (interactive)
         (enlarge-window-horizontally 1)))
  ("<" (lambda ()
         (interactive)
         (shrink-window-horizontally 1)))
  ("," (lambda ()
         (interactive)
         (shrink-window 1)))
  ("." (lambda ()
         (interactive)
         (enlarge-window 1)))
  ("e" evil-window-split) ;keeps sizes balanced as it splits like in Vim.
  ("E" evil-window-vsplit);keeps sizes balanced as it splits like in Vim.
  ("v" evil-window-new)
  ("V" evil-window-vnew)
  ("r" evil-window-rotate-upwards)
  ("R" evil-window-rotate-downwards)
  ("S" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right))
   "vert")
  ("s" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down))
   "horz")
  ("d" (lambda ()
         (interactive)
         (delete-window))
   "del")
  ("o" other-window)
  ("n" next-buffer)
  ("p" previous-buffer)
  ("b" balance-windows)
  ;;("K" kill-this-buffer)
  ("x" maximize-window "max")
  ("X" delete-other-windows "only")
  ("C-g" nil nil)
  ("\\" nil)
  ("q" nil "quit") ;nil for function is an automatic blue head.
  )
;; (define-key evil-normal-state-map (kbd "\\") 'hydra-window/body)
;; (define-key evil-motion-state-map (kbd "\\") 'hydra-window/body)
;; (eval-after-load "magit"
;;   '(progn
;;      (define-key magit-mode-map (kbd "\\") 'hydra-window/body)))
;;(evil-define-key 'emacs magit-mode-map (kbd "\\") 'hydra-window/body)

;; (defhydra helm-like-unite ()
;;   "vim movement"
;;   ("?" helm-help "help")
;;   ("<escape>" keyboard-escape-quit "exit")
;;   ("<SPC>" helm-toggle-visible-mark "mark")
;;   ("a" helm-toggle-all-marks "(un)mark all")
;;   ;; not sure if there's a better way to do this
;;   ("/" (lambda ()
;;          (interactive)
;;          (execute-kbd-macro [?\C-s]))
;;    "search")
;;   ("v" helm-execute-persistent-action)
;;   ("g" helm-beginning-of-buffer "top")
;;   ("G" helm-end-of-buffer "bottom")
;;   ("j" helm-next-line "down")
;;   ("k" helm-previous-line "up")
;;   ("q" helm-keyboard-quit) ;exit helm in 1 step
;;   ("i" nil "cancel"))
;; (define-key helm-map (kbd "<escape>") 'helm-like-unite/body)


(let ((initial-expand 2))

 (defhydra hydra-expand-region
   (:body-pre
    (er/expand-region initial-expand) ; expand mulitple times for a head start.
    ;; (call-interactively #'er/expand-region)
    ;; TODO: wire up "fj" keybind to quit with `key-chord'
    )
   ("k" er/expand-region)
   ("j" er/contract-region)
   ("i" (lambda ()
          (interactive)
          (er/expand-region 2)))
   ("u" (lambda ()
          (interactive)
          (er/contract-region 2)))

   ;; NOTE: don't bind C-g in the hydra as it messes up expand-region's ability
   ;; to jump back to the original starting point with C-g.
   ;; ("C-g" nil nil)

   ("q" nil))

 (defun my-hydra-set-initial-expand ()
   (interactive)
   (setq initial-expand
         (read-number "initial-expand: "
                      initial-expand)))) ; default to current val.


(defhydra my-fugitive-hydra (:color blue :hint nil) ;;(:color blue)
  "
_f_: shell-command (most used)
_l_: log graph
_;_: log between revs
_'_: fetch, then log delta
_a_: quick-commit
_b_: blame
_d_: diff between revs
_e_: find local-only branches you may want to delete
_s_: show commit at point
_p_: jump to parent commit in log buffer
_c_: delete fugitive buffers
_L_: enter log Hydra
_q_, _C-g_: quit"
  ("f" fugitive-shell-command)
  ("l" fugitive-log-graph)
  (";" fugitive-log-between)
  ("'" fugitive-fetch-n-log)
  ("a" fugitive-quick-commit)
  ("b" fugitive-blame)
  ("d" fugitive-diff-between)
  ("e" fugitive-find-local-only-branches-ediff)
  ("s" fugitive-show)
  ("p" fugitive-parent-commits-jump-to)
  ("c" fugitive-delete-buffers-except-current)
  ("L" my-fugitive-log-hydra/body)
  ;; don't use the hint text as it makes (:hint nil) not work?
  ("C-g" nil nil)
  ("q" nil))

(defhydra my-fugitive-log-hydra (:color blue :hint nil) ;;(:color blue)
  "
_f_: single file
_p_: first parent commits only
_c_: graph compact
_m_: graph medium
_l_: graph long
_s_: fast
_a_: faster, no colors
_;_: log between revs
_q_, _C-g_: quit"
  ("f" fugitive-log-file)
  ("p" fugitive-log-first-parent)
  ("c" fugitive-log-graph-compact)
  ("m" fugitive-log-graph-medium)
  ("l" fugitive-log-graph-long)
  ("s" fugitive-log-fast)
  ("a" fugitive-log-faster)
  (";" fugitive-log-between)
  ;; don't use the hint text as it makes (:hint nil) not work?
  ("C-g" nil nil)
  ("q" nil))


(load "my-test-mode") ;; TODO: move this into a proper mode file
(defhydra my-sql-comp-hydra (:color blue :hint nil) ;;(:color blue)
  "
_i_: init schema data
_s_: schema
_t_: table
_v_: view
_c_: col
_q_, _C-g_: quit"
  ("i" my-sql-fill-completion-data)
  ("s" my-sql-complete-schema)
  ("t" my-sql-complete-table)
  ("v" my-sql-complete-view)
  ("c" my-sql-complete-col)
  ;; don't use the hint text as it makes (:hint nil) not work?
  ("C-g" nil nil)
  ("q" nil))



;; spawn hydras from a single binding. A hydra of hydras.
(let ((my-hydras (mapcar #'symbol-name
                         (list #'hydra-easyscroll/body
                               #'hydra-window/body
                               #'my-hydra-font/body
                               #'hydra-expand-region/body
                               #'hydra-smerge/body
                               ;;trying paredit #'hydra-paredit/body
                               ))))
  (defun my-choose-hydra ()
    (interactive)
    (funcall (intern (completing-read "pick one: " my-hydras)))))


(provide 'my-hydras)