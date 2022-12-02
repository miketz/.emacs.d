;;; my-modules-tests.el --- Tests -*- lexical-binding: t -*-

;;; Commentary:
;;; Tests for package `my-modules'.
;;;
;;; NOTE: These are not "good" unit tests.  They assume specific git repo
;;; states that only exist on my machine.  They are for my personal testing
;;; only.
;;;
;;; Naming convention:
;;;  - prefix all test names with "my-modules-".
;;;  - include the word "test" in the name.
;;;
;;; Run tests:
;;;  - M-x ert RET "^my-modules-" RET

;;; Code:
(require 'my-modules)
(require 'ert)

(ert-deftest my-modules-git-remote-create-test ()
  ;; shadow `my-modules' to have a dummy helm module for the tests
  (let ((my-modules `(,(make-module
                        :name 'helm
                        :comment nil
                        :folder (concat my-module-folder "helm")
                        :remotes '((:sym mine :url "https://github.com/miketz/helm"
                                         :alias "origin")
                                   (:sym upstream :url "https://github.com/emacs-helm/helm"
                                         :alias "upstream"))
                        :remote-default 'mine
                        :source-control 'git
                        :submodule-p t
                        :use-branch "master"
                        :depend-hard '((emacs "25.1")
                                       (async "1.9.4")
                                       (popup "0.5.3"))
                        :depend-soft '()
                        :depend-bundled '((helm-core))))))
    ;; tests
    (should (eq 'already-created
                (my-git-remote-create (my-get-module-by-symbol 'helm) 'mine)))
    (should (eq 'remote-not-configured-in-my-modules
                (my-git-remote-create (my-get-module-by-symbol 'helm) 'non-existing)))
    ;; don't test this as we dont' want to actually add a remote. Also the upstream might exsit making the
    ;; assumed pre-condition for git-state wrong.
    ;; (should (eq 'ok-now-creating-git-remote
    ;;             (my-git-remote-create (my-get-module-by-symbol 'helm) 'upstream)))
    ))

(provide 'my-modules-tests)

;;; my-modules-tests.el ends here