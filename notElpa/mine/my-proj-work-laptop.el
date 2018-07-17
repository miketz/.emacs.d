;;; my-proj-work-laptop.el --- Proj functions -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'bookmark)
;; (require 'helm-cmd-t)

;; (defun proj-ecp ()
;;   (interactive)
;;   (let* ((root "C:\\Users\\mtz\\proj\\TFS\\SafetyWebsite\\Main\\Source\\")
;;          (sln (concat root "Safety.sln"))
;;          (defaultFile (concat root "Safety.WebUI\\Areas\\ECP\\Controllers\\ProcedureController.cs")))
;;     ;; helm-cmd-t stuff
;;     (add-to-list 'helm-cmd-t-find-prunes "obj")
;;     (add-to-list 'helm-cmd-t-find-prunes "bin")
;;     (add-to-list 'helm-cmd-t-find-prunes ".svn")
;;     (add-to-list 'helm-cmd-t-find-prunes "packages")
;;     (add-to-list 'helm-cmd-t-find-prunes "Safety.WebUI.Tests")
;;     (add-to-list 'helm-cmd-t-find-prunes "TestResults")
;;     (setq dir_ecp (helm-cmd-t-get-create-source-dir root))
;;     (evil-leader/set-key "h" (lambda ()
;;                                (interactive)
;;                                (helm :sources '(helm-source-buffers-list
;;                                                 dir_ecp)
;;                                      :buffer "*ECP Project*")))
;;     ;;(dired root)
;;     (find-file-existing defaultFile)
;;     ;;custom start of omnisharp. The commnad line string made by (omnisharp-start-omnisharp-server sln) doesn't work on my box.
;;     (my-start-omnisharp-server sln)

;;     ;;TODO: build ctags or etags.
;;     ;;(start-process-shell-command "makingCtags" nil "ctags -R -e *.cs")
;;     ))


(defun my-proj-safetyweb ()
  "Open the ECP project."
  (interactive)
  (let* ((root "C:\\Users\\mtz\\proj\\TFS\\SafetyWebsite\\OSHE\\Main\\Source\\")
         ;; (sln (concat root "Safety.sln"))
         ;; (defaultFile (concat root "Safety.WebUI\\Areas\\ECP\\Controllers\\ProcedureController.cs"))
         )
    ;; ;; helm-cmd-t stuff
    ;; (add-to-list 'helm-cmd-t-find-prunes "obj")
    ;; (add-to-list 'helm-cmd-t-find-prunes "bin")
    ;; (add-to-list 'helm-cmd-t-find-prunes ".svn")
    ;; (add-to-list 'helm-cmd-t-find-prunes "packages")
    ;; (add-to-list 'helm-cmd-t-find-prunes "Safety.WebUI.Tests")
    ;; (add-to-list 'helm-cmd-t-find-prunes "TestResults")
    ;; (setq dir_ecp (helm-cmd-t-get-create-source-dir root))
    ;; (evil-leader/set-key "h" (lambda ()
    ;;                            (interactive)
    ;;                            (helm :sources '(helm-source-buffers-list
    ;;                                             dir_ecp)
    ;;                                  :buffer "*Saftey Web Project*")))
    (dired root)
    ;;custom start of omnisharp. The commnad line string made by (omnisharp-start-omnisharp-server sln) doesn't work on my box.
    ;;(my-start-omnisharp-server sln)
    ))

(defun my-proj-safetyweb-ects ()
  "Open the ECTS project."
  (interactive)
  (let* ((root "C:/Users/mtz/proj/TFS/SafetyWebsite/OSHE/Development/ECTS/Source/")
         ;; (sln (concat root "Safety.sln"))
         ;; (defaultFile (concat root "Safety.WebUI\\Areas\\ECP\\Controllers\\ProcedureController.cs"))
         )
    ;; ;; helm-cmd-t stuff
    ;; (add-to-list 'helm-cmd-t-find-prunes "obj")
    ;; (add-to-list 'helm-cmd-t-find-prunes "bin")
    ;; (add-to-list 'helm-cmd-t-find-prunes ".svn")
    ;; (add-to-list 'helm-cmd-t-find-prunes "packages")
    ;; (add-to-list 'helm-cmd-t-find-prunes "Safety.WebUI.Tests")
    ;; (add-to-list 'helm-cmd-t-find-prunes "TestResults")
    ;; (setq dir_ects (helm-cmd-t-get-create-source-dir root))
    ;; (evil-leader/set-key "h" (lambda ()
    ;;                            (interactive)
    ;;                            (helm :sources '(helm-source-buffers-list
    ;;                                             dir_ects)
    ;;                                  :buffer "*Saftey Web Project*")))
    (dired root)
    ;;custom start of omnisharp. The commnad line string made by (omnisharp-start-omnisharp-server sln) doesn't work on my box.
    ;;(my-start-omnisharp-server sln)
    ))

(defun my-proj-rsims ()
  "Enable project features for the RSIMS website."
  (interactive)
  (let* ((root "C:\\Users\\mtz\\proj\\TFS\\SafetyWebsite\\RSIMS\\Main\\Source\\")
         ;; (sln (concat root "Rsims.sln"))
         )
    ;; ;; helm-cmd-t stuff
    ;; (add-to-list 'helm-cmd-t-find-prunes "obj")
    ;; (add-to-list 'helm-cmd-t-find-prunes "bin")
    ;; (add-to-list 'helm-cmd-t-find-prunes ".svn")
    ;; (add-to-list 'helm-cmd-t-find-prunes "packages")
    ;; (setq my-dir-rsims (helm-cmd-t-get-create-source-dir root))
    ;; (evil-leader/set-key "h" (lambda ()
    ;;                            (interactive)
    ;;                            (helm :sources '(helm-source-buffers-list
    ;;                                             my-dir-rsims)
    ;;                                  :buffer "*RSIMS Web Project*")))
    (dired root)))

(defun my-proj-daily-diff ()
  "Enable project features for the RSIMS website."
  (interactive)
  (let* ((root "C:\\Users\\mtz\\proj\\DD_DailyDiff\\")
         ;; (sln (concat root "DD_DailyDiff.sln"))
         )
    ;; ;; helm-cmd-t stuff
    ;; (add-to-list 'helm-cmd-t-find-prunes "obj")
    ;; (add-to-list 'helm-cmd-t-find-prunes "bin")
    ;; (add-to-list 'helm-cmd-t-find-prunes ".svn")
    ;; (add-to-list 'helm-cmd-t-find-prunes "packages")
    ;; (setq my-dir-dd (helm-cmd-t-get-create-source-dir root))
    ;; (evil-leader/set-key "h" (lambda ()
    ;;                            (interactive)
    ;;                            (helm :sources '(helm-source-buffers-list
    ;;                                             my-dir-dd)
    ;;                                  :buffer "*Daily Diff*")))
    (dired root)))

(defun my-proj-db-safety ()
  "Open the /Main/DbScripts folder."
  (interactive)
  (let ((root "C:\\Users\\mtz\\proj\\TFS\\SafetyWebsite\\OSHE\\Main\\DbScripts"))
    ;; (setq root-cmd-t (helm-cmd-t-get-create-source-dir root))
    ;; (evil-leader/set-key "h" (lambda ()
    ;;                            (interactive)
    ;;                            (helm :sources '(root-cmd-t)
    ;;                                  :buffer "*OSHE DB Scripts*")))
    (dired root)))

(defun my-proj-trighist ()
  "Open the history trigger generator project."
  (interactive)
  (let* ((root "C:\\Users\\mtz\\proj\\HistoryImp\\dev\\code\\v3_GeneralHistory\\HistoryTriggerGen\\")
         ;; (sln (concat root "HistoryTriggerGen.sln"))
         )
    ;; ;; helm-cmd-t stuff
    ;; (add-to-list 'helm-cmd-t-find-prunes "obj")
    ;; (add-to-list 'helm-cmd-t-find-prunes "bin")
    ;; (add-to-list 'helm-cmd-t-find-prunes ".svn")
    ;; (add-to-list 'helm-cmd-t-find-prunes ".git")
    ;; (add-to-list 'helm-cmd-t-find-prunes "packages")
    ;; (setq dir_triggerhist (helm-cmd-t-get-create-source-dir root))
    ;; (evil-leader/set-key "h" (lambda ()
    ;;                            (interactive)
    ;;                            (helm :sources '(helm-source-buffers-list
    ;;                                             dir_triggerhist)
    ;;                                  :buffer "*ECP Project*")))
    (dired root)
    ;;custom start of omnisharp. The commnad line string made by (omnisharp-start-omnisharp-server sln) doesn't work on my box.
    ;;(my-start-omnisharp-server sln)
    ))

(defun my-proj-emacs ()
  "Open the Emacs source code folder."
  (interactive)
  (let* ((emacs-root "C:\\Users\\mtz\\scratch\\emacs\\"))
    ;; ;; helm-cmd-t stuff
    ;; (add-to-list 'helm-cmd-t-find-prunes ".git")
    ;; (setq dir_emacs (helm-cmd-t-get-create-source-dir emacs-root))
    ;; (evil-leader/set-key "h" (lambda ()
    ;;                            (interactive)
    ;;                            (helm :sources '(helm-source-buffers-list
    ;;                                             dir_emacs)
    ;;                                  :buffer "*Emacs Project*")))
    (dired emacs-root)))

(defun my-proj-cl ()
  "Open a scratch Lisp file."
  (interactive)
  ;; ;; helm-cmd-t stuff
  ;; (setq root_dir_cl (helm-cmd-t-get-create-source-dir "C:\\Users\\mtz\\scratch\\lisp"))
  ;; (evil-leader/set-key "h" (lambda ()
  ;;                            (interactive)
  ;;                            (helm :sources '(helm-source-buffers-list
  ;;                                             root_dir_cl)
  ;;                                  :buffer "*Lisp Project*")))
  ;;load project
  (find-file-existing "C:\\Users\\mtz\\scratch\\lisp\\test.lisp")
  ;;(dired "C:\\Users\\mtz\\scratch\\lisp")
  ;;(slime)
  )

;; suppress flycheck warning.
(declare-function evil-window-move-far-left 'evil-commands)

(defun my-proj-imgtag ()
  "Open a scratch project for testing web-based image drag-n-drop."
  (interactive)
  (delete-other-windows)
  (let* ((root "C:\\Users\\mtz\\scratch\\ImgDragAndDrop\\")
         (html (concat root "test.html"))
         (css (concat root "test.css"))
         (js (concat root "test.js")))
    (find-file-existing css) (split-window)
    (find-file-existing js) (split-window)
    (find-file-existing html)
    (evil-window-move-far-left)
    (shrink-window-horizontally 35)))

(defun my-proj-pcl ()
  "Open the Practical Common Lisp ebook."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "pcl-test")
  (common-lisp-mode)
  (split-window-horizontally)
  (shrink-window-horizontally 24)
  ;; NOTE: cloned from https://github.com/akosma/PracticalCommonLisp_ePub
  (eww-open-file "c:/Users/mtz/scratch/PracticalCommonLisp_ePub/html/index.html"))

(defun my-proj-paip ()
  "Open the PAIP book."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "paip-test")
  (common-lisp-mode)
  (split-window-horizontally)
  (shrink-window-horizontally 13)
  (let ((paip (car (member "paip" (bookmark-all-names)))))
    (if paip
        (bookmark-jump paip)
      ;; else go to the folder
      (dired "c:/Users/mtz/Downloads/tutorials/paip-lisp/docs"))))

(defun my-proj-emacs-manual ()
  "Open the emacs manual."
  (interactive)
  (let ((bm (car (member "emacs-man" (bookmark-all-names)))))
    (if bm
        (bookmark-jump bm)
      ;; else go to top emacs info node.
      (info "emacs"))))

(defun my-proj-progit2 ()
  "Open the progit2 ebook."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (shrink-window-horizontally 24)
  (let ((progit2 (car (member "progit2" (bookmark-all-names)))))
    (if progit2
        (progn
          (bookmark-jump progit2)
          ;; go to the table of contents so you can see the order of chapters.
          (let ((folder (file-name-directory buffer-file-name)))
            (other-window 1)
            (dired folder)
            (dired-up-directory)
            (dired-previous-line 1)
            (dired-find-file)
            ;; jump back to the text
            (other-window 1)
            ))
      ;; else no bookmark. Go to the top folder
      (dired "c:/users/mtz/scratch/progit2/book"))))

(defun my-proj-dive-python ()
  "Open the dive into python ebook."
  (interactive)
  (find-file-existing
   "C:/Users/mtz/Downloads/tutorials/diveintopython-text-5.4/diveintopython-5.4/diveintopython.txt")
  (markdown-mode))

(defun my-proj-tcpl ()
  "Open a scratch C file."
  (interactive)
  (delete-other-windows)
  (find-file-existing "c:/users/mtz/scratch/test5/test.c")
  (split-window-horizontally)
  (shrink-window-horizontally 24)
  (other-window 1)
  (eshell))

(defun my-proj-cpp ()
  "Open a scratch c++ folder."
  (interactive)
  ;; (setq root_dir_cpp (helm-cmd-t-get-create-source-dir "C:\\Users\\mtz\\scratch\\cpp"))
  ;; (evil-leader/set-key "h" (lambda ()
  ;;                            (interactive)
  ;;                            (helm :sources '(helm-source-buffers-list
  ;;                                             root_dir_cpp)
  ;;                                  :buffer "*Cpp Project*")))
  (dired "C:\\Users\\mtz\\scratch\\cpp")
  ;;(start-process-shell-command "makingCtags" nil "ctags -R -e *.cpp")
  )

(provide 'my-proj-work-laptop)

;;; my-proj-work-laptop.el ends here
