(require 'helm-cmd-t)

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


(defun proj-safetyweb ()
  (interactive)
  (let* ((root "C:\\Users\\mtz\\proj\\TFS\\SafetyWebsite\\OSHE\\Main\\Source\\")
         (sln (concat root "Safety.sln"))
         (defaultFile (concat root "Safety.WebUI\\Areas\\ECP\\Controllers\\ProcedureController.cs")))
    ;; helm-cmd-t stuff
    (add-to-list 'helm-cmd-t-find-prunes "obj")
    (add-to-list 'helm-cmd-t-find-prunes "bin")
    (add-to-list 'helm-cmd-t-find-prunes ".svn")
    (add-to-list 'helm-cmd-t-find-prunes "packages")
    (add-to-list 'helm-cmd-t-find-prunes "Safety.WebUI.Tests")
    (add-to-list 'helm-cmd-t-find-prunes "TestResults")
    (setq dir_ecp (helm-cmd-t-get-create-source-dir root))
    (evil-leader/set-key "h" (lambda ()
                               (interactive)
                               (helm :sources '(helm-source-buffers-list
                                                dir_ecp)
                                     :buffer "*Saftey Web Project*")))
    (dired root)
    ;;custom start of omnisharp. The commnad line string made by (omnisharp-start-omnisharp-server sln) doesn't work on my box.
    ;;(my-start-omnisharp-server sln)
    ))

(defun proj-rsims ()
  "Enable project features for the RSIMS website."
  (interactive)
  (let* ((root "C:\\Users\\mtz\\proj\\TFS\\SafetyWebsite\\RSIMS\\Main\\Source\\")
         (sln (concat root "Rsims.sln")))
    ;; helm-cmd-t stuff
    (add-to-list 'helm-cmd-t-find-prunes "obj")
    (add-to-list 'helm-cmd-t-find-prunes "bin")
    (add-to-list 'helm-cmd-t-find-prunes ".svn")
    (add-to-list 'helm-cmd-t-find-prunes "packages")
    (add-to-list 'helm-cmd-t-find-prunes "Safety.WebUI.Tests")
    (add-to-list 'helm-cmd-t-find-prunes "TestResults")
    (setq my-dir-rsims (helm-cmd-t-get-create-source-dir root))
    (evil-leader/set-key "h" (lambda ()
                               (interactive)
                               (helm :sources '(helm-source-buffers-list
                                                my-dir-rsims)
                                     :buffer "*RSIMS Web Project*")))
    (dired root)))

(defun proj-db-safety ()
  (interactive)
  (let ((root "C:\\Users\\mtz\\proj\\TFS\\SafetyWebsite\\OSHE\\Main\\DbScripts"))
    (setq root-cmd-t (helm-cmd-t-get-create-source-dir root))
    (evil-leader/set-key "h" (lambda ()
                               (interactive)
                               (helm :sources '(root-cmd-t)
                                     :buffer "*OSHE DB Scripts*")))
    (dired root)))

(defun proj-trighist ()
  (interactive)
  (let* ((root "C:\\Users\\mtz\\proj\\HistoryImp\\dev\\code\\v3_GeneralHistory\\HistoryTriggerGen\\")
         (sln (concat root "HistoryTriggerGen.sln")))
    ;; helm-cmd-t stuff
    (add-to-list 'helm-cmd-t-find-prunes "obj")
    (add-to-list 'helm-cmd-t-find-prunes "bin")
    (add-to-list 'helm-cmd-t-find-prunes ".svn")
    (add-to-list 'helm-cmd-t-find-prunes ".git")
    (add-to-list 'helm-cmd-t-find-prunes "packages")
    (setq dir_triggerhist (helm-cmd-t-get-create-source-dir root))
    (evil-leader/set-key "h" (lambda ()
                               (interactive)
                               (helm :sources '(helm-source-buffers-list
                                                dir_triggerhist)
                                     :buffer "*ECP Project*")))
    (dired root)
    ;;custom start of omnisharp. The commnad line string made by (omnisharp-start-omnisharp-server sln) doesn't work on my box.
    ;;(my-start-omnisharp-server sln)
    ))

(defun proj-emacs ()
  (interactive)
  (let* ((emacs-root "C:\\Users\\mtz\\scratch\\emacs\\"))
    ;; helm-cmd-t stuff
    (add-to-list 'helm-cmd-t-find-prunes ".git")
    (setq dir_emacs (helm-cmd-t-get-create-source-dir emacs-root))
    (evil-leader/set-key "h" (lambda ()
                               (interactive)
                               (helm :sources '(helm-source-buffers-list
                                                dir_emacs)
                                     :buffer "*Emacs Project*")))
    (dired emacs-root)))

(defun proj-cl ()
  (interactive)
  ;; helm-cmd-t stuff
  (setq root_dir_cl (helm-cmd-t-get-create-source-dir "C:\\Users\\mtz\\scratch\\lisp"))
  (evil-leader/set-key "h" (lambda ()
                             (interactive)
                             (helm :sources '(helm-source-buffers-list
                                              root_dir_cl)
                                   :buffer "*Lisp Project*")))
  ;;load project
  (find-file-existing "C:\\Users\\mtz\\scratch\\lisp\\test.lisp")
  ;;(dired "C:\\Users\\mtz\\scratch\\lisp")
  (slime))

(defun proj-imgtag ()
  (interactive)
  (let* ((root "C:\\Users\\mtz\\scratch\\ImgDragAndDrop\\")
         (html (concat root "test.html"))
         (css (concat root "test.css"))
         (js (concat root "test.js")))
    (find-file-existing css) (split-window)
    (find-file-existing js) (split-window)
    (find-file-existing html)
    (evil-window-move-far-left)
    (shrink-window-horizontally 35)))

(defun proj-cpp ()
  (interactive)
  (setq root_dir_cpp (helm-cmd-t-get-create-source-dir "C:\\Users\\mtz\\scratch\\cpp"))
  (evil-leader/set-key "h" (lambda ()
                             (interactive)
                             (helm :sources '(helm-source-buffers-list
                                              root_dir_cpp)
                                   :buffer "*Cpp Project*")))
  (dired "C:\\Users\\mtz\\scratch\\cpp")
  ;;(start-process-shell-command "makingCtags" nil "ctags -R -e *.cpp")
  )