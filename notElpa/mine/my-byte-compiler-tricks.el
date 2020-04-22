;;; This file is not meant to be loaded/requried. It's a scratch pad for
;;; ideas to make code execute at byte-compile time.
;;; Tricks to speed up init time.
;;; Or tricks to control when code is evaluated for other purposes.


;; from Misha_B on #emacs
;; A macro to wrap things to force them to be evaluted during macro-expansion
;; time (ie byte-compile time).
;; Might only work for things that are pure data Values.
;; Would not work for side-effects like setting the color theme as that must
;; occur at runtime in the future, not at byte-compile time.
(defmacro constexpr (b)
  (eval b))


;; in byte-comiled codes, it only runs during byte-compile stage.
;; any side-effect code in section not run at all when loading init.elc ???
(eval-when-compile
  ;; stuff
  )
;; But it *will* run if loading a non-compiled .el file???


;; similar to eval-when-compile, but will won't run in non-byte-compiled code?
;; Can be used to (require 'some-package) to silence byte-compiler warnings,
;; about free vars during compile time, but not actually require the package
;; during init time.
(cl-eval-when 'compile
  (require 'autorevert))
;; this techinique shoudl be better than the (defvar foo) solution used
;; to silence free var warnigns. I can mistype name or package can remove
;; definition and compilation checker would be silent in such cases
;; also see:
;; https://emacs.stackexchange.com/questions/17347/why-does-eval-when-compile-run-at-file-load-and-byte-compiled-to-elc
;; ---
;; But it has drawbacks too.
;; Drawback 1: t slows down byte compliation (especially on MS Windows) due to
;; literally running require during compliation.
;; Drawback 2: it does not prevent dynamic vars from being incorreclty
;; lexically bound in a let statment becuase the defvar form never occurs, as
;; the (require 'lib) only runs during byte compilation time. The (defvar foo)
;; trick protects against this incorrect lexical binding.



