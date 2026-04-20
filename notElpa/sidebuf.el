;;; sidebuf.el --- Buffer list sidebar panel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 rain-64

;; Author: rain-64 <https://github.com/rain-64>
;; URL: https://github.com/rain-64/sidebuf
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience buffers

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Sidebuf displays a persistent, sorted buffer list in a side
;; window.  It provides a simple, zero-dependency alternative to
;; heavier buffer management packages.
;;
;; Features:
;;  - Flat, sorted buffer list pinned to the left or right side
;;  - Alphabetical or most-recent sort order
;;  - Pin buffers to the top of the list
;;  - Toggle visibility of *special* and hidden buffers
;;  - Active-buffer tracking with fringe indicator
;;  - Smart window reuse when selecting buffers
;;  - Modified-buffer indicators
;;  - Kill buffers without leaving the sidebar
;;
;; Usage:
;;   M-x sidebuf-toggle    Open or close the panel
;;   M-x sidebuf-open      Open the panel
;;   M-x sidebuf-close     Close the panel
;;
;; Press ? inside the panel to see all keybindings.

;;; Code:

(require 'cl-lib)
(require 'face-remap)
(require 'hl-line)

;;;; Customization group

(defgroup sidebuf nil
  "Buffer list sidebar panel."
  :group 'convenience
  :prefix "sidebuf-")

;;;; User options

(defcustom sidebuf-side 'left
  "Which side of the frame the panel appears on."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'sidebuf)

(defcustom sidebuf-width 24
  "Width of the side window in characters."
  :type 'natnum
  :group 'sidebuf)

(defcustom sidebuf-show-special t
  "Whether to show *special* buffers (names starting with *)."
  :type 'boolean
  :group 'sidebuf)

(defcustom sidebuf-show-hidden nil
  "Whether to show hidden buffers (names starting with a space)."
  :type 'boolean
  :group 'sidebuf)

(defcustom sidebuf-sort-order 'alphabetical
  "Sort order for the buffer list.
`alphabetical' groups buffers by type (regular, special, hidden)
and sorts alphabetically within each group.
`recent' sorts by most recently visited."
  :type '(choice (const :tag "Alphabetical (grouped)" alphabetical)
                 (const :tag "Most recent first" recent))
  :group 'sidebuf)

(defcustom sidebuf-pin-new-position 'bottom
  "Where newly pinned buffers appear in the pinned group."
  :type '(choice (const :tag "Top of pinned list" top)
                 (const :tag "Bottom of pinned list" bottom))
  :group 'sidebuf)

(defcustom sidebuf-show-indicators t
  "Whether to show modified/read-only indicators next to names."
  :type 'boolean
  :group 'sidebuf)

;;;; Faces

(defface sidebuf-hl-line
  '((((background dark))
     :background "white" :foreground "black" :extend t)
    (t
     :background "black" :foreground "white" :extend t))
  "Face for `hl-line' in the Sidebuf panel when it has focus."
  :group 'sidebuf)

(defface sidebuf-hl-line-inactive
  '((((background dark))
     :box (:line-width -1 :color "white") :extend nil)
    (t
     :box (:line-width -1 :color "black") :extend nil))
  "Face for `hl-line' in the Sidebuf panel when it lacks focus.
Highlights which buffer is currently active in the other window
while making it obvious that the panel itself is not focused.
The box color is pinned explicitly so the outline does not
inherit the buffer name's foreground (which would otherwise
tint the box around faced buffer names like *special* ones).
`:extend nil' is set explicitly to override the base `hl-line'
face: with `:extend t', Emacs does not draw the right edge of
the box across the virtual end-of-line extension."
  :group 'sidebuf)

(defface sidebuf-fringe-indicator
  `((t :foreground ,(face-background 'cursor nil t)))
  "Face for the fringe indicator."
  :group 'sidebuf)

(defface sidebuf-pinned
  '((t :inherit warning))
  "Face for pinned buffer names."
  :group 'sidebuf)

(defface sidebuf-special
  '((t :inherit font-lock-comment-face))
  "Face for *special* buffer names."
  :group 'sidebuf)

(defface sidebuf-hidden
  '((t :inherit shadow))
  "Face for hidden buffer names (space-prefixed)."
  :group 'sidebuf)

(defface sidebuf-modified
  '((t :inherit font-lock-constant-face))
  "Face for modified buffer names."
  :group 'sidebuf)

;;;; Internal variables

(defconst sidebuf--buffer-name "*Sidebuf*"
  "Name of the Sidebuf panel buffer.")

(defvar sidebuf--refreshing nil
  "Guard against recursive buffer list refreshes.")

(defvar sidebuf--last-modified-state nil
  "Alist of (buffer-name . modified-p) from last render.
Used to detect modified-state changes without refreshing on
every command.")

(defvar sidebuf--pinned '()
  "List of pinned buffer names, in pin order.")

(defvar-local sidebuf--fringe-ov nil
  "Overlay for the fringe indicator in the panel.")

(defvar-local sidebuf--hl-line-cookie nil
  "Face-remap cookie for `hl-line' in the panel.")

(defvar-local sidebuf--match-p nil
  "Non-nil if the last render placed point on a visible line.
When nil while the panel is unfocused, `sidebuf-refresh' hides
the `hl-line' overlay so no stale line is highlighted.")

;;;; Fringe bitmap

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'sidebuf-indicator
    (make-vector 200 #b11111111)))

(defconst sidebuf--fringe-string
  (propertize " " 'display
              '(left-fringe sidebuf-indicator
                            sidebuf-fringe-indicator))
  "Before-string for the fringe indicator overlay.")

(defun sidebuf--update-fringe-color (&rest _)
  "Update fringe indicator color to match current cursor face."
  (set-face-foreground 'sidebuf-fringe-indicator
                       (face-background 'cursor nil t)))

;;;; Buffer filtering

(defun sidebuf--visible-p (buf)
  "Return non-nil if BUF should appear in the panel."
  (let ((name (buffer-name buf)))
    (and (not (string= name sidebuf--buffer-name))
         (or sidebuf-show-hidden
             (not (string-prefix-p " " name)))
         (or sidebuf-show-special
             (not (string-prefix-p "*" name))))))

;;;; Sorting

(defun sidebuf--group (name)
  "Return sort group for buffer NAME.
0 = regular, 1 = special (*), 2 = hidden (space)."
  (cond
   ((string-prefix-p " " name) 2)
   ((string-prefix-p "*" name) 1)
   (t 0)))

(defun sidebuf--pinned-p (buf)
  "Return non-nil if BUF is pinned."
  (member (buffer-name buf) sidebuf--pinned))

(defun sidebuf--sort-pinned (bufs)
  "Sort pinned BUFS according to their order in `sidebuf--pinned'."
  (sort bufs (lambda (a b)
               (< (or (cl-position (buffer-name a) sidebuf--pinned
                                   :test #'string=)
                       0)
                  (or (cl-position (buffer-name b) sidebuf--pinned
                                   :test #'string=)
                       0)))))

(defun sidebuf--sort-alphabetical (bufs)
  "Sort BUFS: regular first, then special, then hidden.
Alphabetical (case-insensitive) within each group."
  (sort bufs
        (lambda (a b)
          (let ((ag (sidebuf--group (buffer-name a)))
                (bg (sidebuf--group (buffer-name b))))
            (if (= ag bg)
                (string< (downcase (buffer-name a))
                          (downcase (buffer-name b)))
              (< ag bg))))))

(defun sidebuf--sort (bufs)
  "Sort BUFS according to `sidebuf-sort-order'.
Pinned buffers always come first, in pin order."
  (let ((pinned (cl-remove-if-not #'sidebuf--pinned-p bufs))
        (unpinned (cl-remove-if #'sidebuf--pinned-p bufs)))
    (append
     (sidebuf--sort-pinned pinned)
     (pcase sidebuf-sort-order
       ('alphabetical (sidebuf--sort-alphabetical unpinned))
       ('recent unpinned)))))

;;;; Rendering

(defun sidebuf--render ()
  "Redraw the Sidebuf panel contents."
  (let* ((panel (get-buffer sidebuf--buffer-name))
         (panel-win (and panel (get-buffer-window panel)))
         (panel-focused (eq (frame-selected-window) panel-win))
         (point-buf-name
          (when (and panel-focused panel)
            (with-current-buffer panel
              (let ((b (get-text-property
                        (line-beginning-position) 'sidebuf-buffer)))
                (and b (buffer-name b))))))
         (sw (or (minibuffer-selected-window) (selected-window)))
         (active (window-buffer
                  (if (and panel
                           (eq (window-buffer sw) panel))
                      (get-mru-window nil nil t)
                    sw)))
         (bufs (sidebuf--sort
                (cl-remove-if-not #'sidebuf--visible-p
                                  (buffer-list))))
         (text-width (if panel-win
                         (window-body-width panel-win)
                       sidebuf-width))
         target-pos)
    ;; Prune stale pins
    (setq sidebuf--pinned
          (cl-remove-if-not (lambda (n) (get-buffer n))
                            sidebuf--pinned))
    (when panel
      (with-current-buffer panel
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (buf bufs)
            (let* ((bol (point))
                   (name (buffer-name buf))
                   (pinned (member name sidebuf--pinned))
                   (modified (and sidebuf-show-indicators
                                  (buffer-file-name buf)
                                  (buffer-modified-p buf)))
                   (readonly (and sidebuf-show-indicators
                                  (buffer-file-name buf)
                                  (with-current-buffer buf
                                    buffer-read-only)))
                   (col1 (if pinned
                             (propertize "^" 'face 'sidebuf-pinned)
                           " "))
                   (col2 (cond
                          (modified
                           (propertize "*" 'face 'sidebuf-modified))
                          (readonly "%")
                          (t " ")))
                   (prefix (concat col1 col2 " "))
                   (face (cond
                          (modified 'sidebuf-modified)
                          ((string-prefix-p " " name)
                           'sidebuf-hidden)
                          ((string-prefix-p "*" name)
                           'sidebuf-special)
                          (t nil)))
                   (text (concat prefix name))
                   (pad (max 0 (- text-width
                                  (string-width text)))))
              (insert (propertize
                       (concat text (make-string pad ?\s))
                       'sidebuf-buffer buf
                       'face face)
                      "\n")
              (when (if panel-focused
                        (and point-buf-name
                             (string= name point-buf-name))
                      (eq buf active))
                (setq target-pos bol))))
          (setq sidebuf--match-p (and target-pos t))
          (when (and target-pos panel-win)
            (set-window-point panel-win target-pos)))))))

;;;; Fringe indicator

(defun sidebuf--hl-line-range ()
  "Return the `hl-line' range for the panel.
Excludes the trailing newline so the `:box' outline from
`sidebuf-hl-line-inactive' closes on the right edge.  The
display engine will not render a box vertical at a newline
position, so hl-line's default range (which includes the
newline) leaves the right side of the box open."
  (cons (line-beginning-position) (line-end-position)))

(defun sidebuf--clamp-point ()
  "Keep point on a valid buffer line."
  (let ((last-valid (save-excursion
                      (goto-char (point-max))
                      (forward-line -1)
                      (line-beginning-position))))
    (when (> (line-beginning-position) last-valid)
      (goto-char last-valid))))

(defun sidebuf--move-fringe ()
  "Clamp point and move the fringe indicator."
  (sidebuf--clamp-point)
  (when sidebuf--fringe-ov
    (move-overlay sidebuf--fringe-ov
                  (line-beginning-position)
                  (1+ (line-beginning-position)))))

(defun sidebuf--apply-hl-line-face (&rest _)
  "Swap the `hl-line' face remap in the panel based on focus.
Uses `sidebuf-hl-line' when the panel has focus and
`sidebuf-hl-line-inactive' otherwise."
  (let ((buf (get-buffer sidebuf--buffer-name)))
    (when buf
      (with-current-buffer buf
        (when sidebuf--hl-line-cookie
          (face-remap-remove-relative sidebuf--hl-line-cookie)
          (setq sidebuf--hl-line-cookie nil))
        (let* ((win (get-buffer-window buf))
               (focused (and win (eq (frame-selected-window) win))))
          (setq sidebuf--hl-line-cookie
                (face-remap-add-relative
                 'hl-line
                 (if focused
                     'sidebuf-hl-line
                   'sidebuf-hl-line-inactive))))))))

(defun sidebuf--show-fringe (&rest _)
  "Show or hide fringe indicator based on panel focus."
  (let* ((buf (get-buffer sidebuf--buffer-name))
         (win (and buf (get-buffer-window buf))))
    (when (and win buf)
      (with-current-buffer buf
        (when sidebuf--fringe-ov
          (if (eq (frame-selected-window) win)
              (let ((pt (window-point win)))
                (move-overlay sidebuf--fringe-ov pt (1+ pt))
                (overlay-put sidebuf--fringe-ov
                             'before-string
                             sidebuf--fringe-string))
            (overlay-put sidebuf--fringe-ov
                         'before-string nil)))))))

(defun sidebuf--maybe-refresh-modified ()
  "Refresh panel if any buffer's modified state has changed."
  (when (and sidebuf-show-indicators
             (get-buffer-window sidebuf--buffer-name))
    (let ((current (mapcar (lambda (b)
                             (cons (buffer-name b)
                                   (and (buffer-file-name b)
                                        (buffer-modified-p b))))
                           (buffer-list))))
      (unless (equal current sidebuf--last-modified-state)
        (setq sidebuf--last-modified-state current)
        (sidebuf-refresh)))))

;;;; Refresh

(defun sidebuf-refresh (&rest _)
  "Refresh the Sidebuf panel when visible."
  (interactive)
  (unless sidebuf--refreshing
    (let ((sidebuf--refreshing t)
          (win (get-buffer-window sidebuf--buffer-name)))
      (when win
        (sidebuf--render)
        (let ((focused (eq (frame-selected-window) win)))
          (with-selected-window win
            (if (or focused sidebuf--match-p)
                (hl-line-highlight)
              (hl-line-unhighlight))))
        (sidebuf--show-fringe)))))

;;;; Interactive commands

(defun sidebuf-select ()
  "Open the buffer at point in another window.
If the buffer is already visible, switch to its window."
  (interactive)
  (let ((buf (get-text-property (line-beginning-position)
                                'sidebuf-buffer)))
    (when (and buf (buffer-live-p buf))
      (let ((win (get-buffer-window buf)))
        (if win
            (select-window win)
          (pop-to-buffer buf
                         '((display-buffer-use-some-window)
                           (inhibit-same-window . t))))))))

(defun sidebuf-display ()
  "Display the buffer at point without moving focus."
  (interactive)
  (let ((buf (get-text-property (line-beginning-position)
                                'sidebuf-buffer)))
    (when (and buf (buffer-live-p buf))
      (let ((win (get-buffer-window buf)))
        (unless win
          (display-buffer buf
                          '((display-buffer-use-some-window)
                            (inhibit-same-window . t))))))))

(defun sidebuf-kill ()
  "Kill the buffer at point."
  (interactive)
  (let ((buf (get-text-property (line-beginning-position)
                                'sidebuf-buffer))
        (line (line-number-at-pos)))
    (when (and buf (buffer-live-p buf))
      (kill-buffer buf)
      (sidebuf-refresh)
      (goto-char (point-min))
      (forward-line (1- line))
      (sidebuf--clamp-point))))

(defun sidebuf-toggle-sort ()
  "Toggle sort order between alphabetical and recent."
  (interactive)
  (setq sidebuf-sort-order
        (if (eq sidebuf-sort-order 'alphabetical)
            'recent
          'alphabetical))
  (sidebuf-refresh)
  (message "Sort: %s" sidebuf-sort-order))

(defun sidebuf-toggle-special ()
  "Toggle visibility of *special* buffers."
  (interactive)
  (setq sidebuf-show-special (not sidebuf-show-special))
  (sidebuf-refresh)
  (message "Special buffers: %s"
           (if sidebuf-show-special "shown" "hidden")))

(defun sidebuf-toggle-hidden ()
  "Toggle visibility of hidden buffers."
  (interactive)
  (setq sidebuf-show-hidden (not sidebuf-show-hidden))
  (sidebuf-refresh)
  (message "Hidden buffers: %s"
           (if sidebuf-show-hidden "shown" "hidden")))

(defun sidebuf-pin ()
  "Pin or unpin the buffer at point."
  (interactive)
  (let* ((buf (get-text-property (line-beginning-position)
                                 'sidebuf-buffer))
         (name (and buf (buffer-name buf))))
    (when name
      (if (member name sidebuf--pinned)
          (progn
            (setq sidebuf--pinned (delete name sidebuf--pinned))
            (message "Unpinned: %s" name))
        (setq sidebuf--pinned
              (pcase sidebuf-pin-new-position
                ('top (cons name sidebuf--pinned))
                ('bottom (append sidebuf--pinned (list name)))))
        (message "Pinned: %s" name))
      (sidebuf-refresh))))

(defun sidebuf-help ()
  "Show a help window with all Sidebuf keybindings."
  (interactive)
  (let ((help-buf (get-buffer-create "*Sidebuf Help*")))
    (with-current-buffer help-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         "Sidebuf -- Keybindings\n"
         (make-string 30 ?=) "\n\n"
         "Navigation:\n"
         "  p, C-p, up, left     Previous buffer\n"
         "  n, C-n, down, right  Next buffer\n\n"
         "Actions:\n"
         "  RET                  Select buffer (smart window reuse)\n"
         "  o                    Display buffer (keep focus here)\n"
         "  k                    Kill buffer at point\n"
         "  i                    Pin/unpin buffer at point\n"
         "  g                    Refresh buffer list\n\n"
         "Toggles:\n"
         "  s                    Toggle sort (alphabetical / recent)\n"
         "  *                    Toggle *special* buffer visibility\n"
         "  .                    Toggle hidden buffer visibility\n\n"
         "Other:\n"
         "  ?                    Show this help\n"
         "  q                    Close sidebuf panel\n")
        (goto-char (point-min))
        (special-mode)))
    (display-buffer help-buf
                    '((display-buffer-below-selected)
                      (window-height . fit-window-to-buffer)))))

;;;; Keymap

(defvar sidebuf-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation
    (define-key map (kbd "n")     #'next-line)
    (define-key map (kbd "C-n")   #'next-line)
    (define-key map [down]        #'next-line)
    (define-key map [right]       #'next-line)
    (define-key map (kbd "p")     #'previous-line)
    (define-key map (kbd "C-p")   #'previous-line)
    (define-key map [up]          #'previous-line)
    (define-key map [left]        #'previous-line)
    ;; Actions
    (define-key map (kbd "RET")   #'sidebuf-select)
    (define-key map (kbd "o")     #'sidebuf-display)
    (define-key map (kbd "k")     #'sidebuf-kill)
    (define-key map (kbd "i")     #'sidebuf-pin)
    (define-key map (kbd "g")     #'sidebuf-refresh)
    ;; Toggles
    (define-key map (kbd "s")     #'sidebuf-toggle-sort)
    (define-key map (kbd "*")     #'sidebuf-toggle-special)
    (define-key map (kbd ".")     #'sidebuf-toggle-hidden)
    ;; Help / quit
    (define-key map (kbd "?")     #'sidebuf-help)
    (define-key map (kbd "q")     #'sidebuf-close)
    map)
  "Keymap for `sidebuf-mode'.")

;;;; Major mode

(define-derived-mode sidebuf-mode special-mode "Sidebuf"
  "Major mode for the Sidebuf buffer list panel."
  :group 'sidebuf
  (setq-local cursor-type nil
              truncate-lines t
              hl-line-sticky-flag t
              hl-line-range-function #'sidebuf--hl-line-range)
  (hl-line-mode 1)
  (sidebuf--apply-hl-line-face)
  ;; Fringe indicator overlay
  (setq sidebuf--fringe-ov
        (make-overlay 1 1 (current-buffer)))
  (add-hook 'post-command-hook
            #'sidebuf--move-fringe nil t))

;;;; Open / close / toggle

;;;###autoload
(defun sidebuf-open ()
  "Open the Sidebuf buffer list panel."
  (interactive)
  (let* ((buf (get-buffer-create sidebuf--buffer-name))
         (win (display-buffer
               buf
               `(display-buffer-in-side-window
                 (side . ,sidebuf-side)
                 (slot . 0)
                 (window-width . ,sidebuf-width)
                 (window-parameters
                  . ((no-delete-other-windows . t)))))))
    (with-current-buffer buf
      (unless (derived-mode-p 'sidebuf-mode)
        (sidebuf-mode)))
    (sidebuf--render)
    (set-window-dedicated-p win t)
    (add-hook 'window-selection-change-functions #'sidebuf-refresh)
    (add-hook 'window-selection-change-functions #'sidebuf--show-fringe)
    (add-hook 'window-selection-change-functions
              #'sidebuf--apply-hl-line-face)
    (add-hook 'buffer-list-update-hook #'sidebuf-refresh)
    (add-hook 'after-save-hook #'sidebuf-refresh)
    (add-hook 'post-command-hook #'sidebuf--maybe-refresh-modified)
    (add-hook 'enable-theme-functions #'sidebuf--update-fringe-color)))

;;;###autoload
(defun sidebuf-close ()
  "Close the Sidebuf panel."
  (interactive)
  (remove-hook 'window-selection-change-functions #'sidebuf-refresh)
  (remove-hook 'window-selection-change-functions #'sidebuf--show-fringe)
  (remove-hook 'window-selection-change-functions
               #'sidebuf--apply-hl-line-face)
  (remove-hook 'buffer-list-update-hook #'sidebuf-refresh)
  (remove-hook 'after-save-hook #'sidebuf-refresh)
  (remove-hook 'post-command-hook #'sidebuf--maybe-refresh-modified)
  (remove-hook 'enable-theme-functions #'sidebuf--update-fringe-color)
  (let ((buf (get-buffer sidebuf--buffer-name)))
    (when buf
      (let ((win (get-buffer-window buf)))
        (when win (delete-window win)))
      (kill-buffer buf))))

;;;###autoload
(defun sidebuf-toggle ()
  "Toggle the Sidebuf panel open or closed."
  (interactive)
  (if (get-buffer-window sidebuf--buffer-name)
      (sidebuf-close)
    (sidebuf-open)))

(provide 'sidebuf)
;;; sidebuf.el ends here