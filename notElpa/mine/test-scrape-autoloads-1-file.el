;;; Another expiriment at avoiding a call to package-initialize.
;;; Not currently used.
;;; Attemps to scrape all the autoloads out of package folders and store them
;;; in 1 file.


;; alterantive to package-initialize. But it's slower!! So don't
;; use it. Keeping it here just for future reference.

(require 'cl-lib)

(defconst my-autoloads-dump-file "~/.emacs.d/all-packages-autoloads.el"
  "A file to accumulate all the autoloads of packages.
To avoid searching for autoload files (which is slow).")

(defconst my-autoloads-dump-file-byte-comp (concat my-autoloads-dump-file
                                                   "c"))

(defun fake-add-to-list (&rest args))

(defun my-scrape-out-package-autoloads ()
  "Looks at every folder under ~/.emacs.d/elpa, scrapes out the autoload code
from the *-autoloads.el files and copies it into 1 giant dump file. This dump
file is loaded during emacs start up as an alterative to calling
`package-initialize'.

The motivation behind this fn is to avoid a very slow call to
`package-initialize' during every emacs startup. It should be faster to
load and run 1 known autoload file than search for many unknown autoload files,
load, and then run them."
  (interactive)
  ;; create empty file if it does not exist.
  (unless (file-exists-p my-autoloads-dump-file)
    (with-temp-buffer (write-file my-autoloads-dump-file)))

  (with-current-buffer (find-file-noselect my-autoloads-dump-file)
    ;; clear text in `my-autoloads-dump-file'
    (goto-char (point-min))
    (delete-char (1- (point-max)))

    ;; append autoload code of each package into the dump file
    (cl-loop
     for dir in (directory-files "~/.emacs.d/elpa" t "^[^.]" t)
     do
     (cl-loop
      for a in (directory-files dir t "-autoloads.el$" t)
      do
      ;; append text to `my-autoloads-dump-file'
      (insert "\n")
      (insert (format "\n(add-to-list 'load-path \"%s\")\n" dir))

      ;; TODO: copy/paste the s-replace function(s) into
      ;; init.el becuase this code executes before all the
      ;; package stuff is set up.

      ;; comment out the adds to load-path from the autoload
      ;; file as it detects the current folder ~/.emacs.d/
      ;; which is not the actual package folder.
      (let ((autoload-file-str (my-get-string-from-file a)))
        (setq autoload-file-str
              (s-replace "(add-to-list 'load-path (directory-file-name"
                         "(fake-add-to-list 'load-path '(directory-file-name"
                         autoload-file-str))
        ;; allow byte-compiling
        (setq autoload-file-str
              (s-replace "no-byte-compile: t"
                         "no-byte-compile: nil"
                         autoload-file-str))
        (insert autoload-file-str))))
    (save-buffer)
    (byte-compile-file my-autoloads-dump-file)))



;; Copy this section to init.el. Then comment out package-initialize.
(when nil
  (require 'test-scrape-autoloads-1-file)
  ;; bootstrap the giant autoloads file thing. Normally fn
  ;; `my-scrape-out-package-autoloads' will be called manually, not at startup.
  ;; But on a new emacs installation, call it on startup.
  (unless (file-exists-p my-autoloads-dump-file)
    (my-scrape-out-package-autoloads))

  (load my-autoloads-dump-file-byte-comp))


(provide 'test-scrape-autoloads-1-file)