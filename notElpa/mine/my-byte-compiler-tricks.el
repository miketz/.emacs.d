;;; This file is not meant to be loaded/requried. It's a scratch pad for
;;; ideas to make code execute at byte-compile time.
;;; Tricks to speed up init time.
;;; Or tricks to control when code is evaluated for other purposes.


;; from Misha_B on #emacs
;; A macro to wrap things to force them to be evaluted during macro-expansion
;; time (ie byte-compile time).
;; Might only work for things that are actually Values. Would not work for
;; side-effects like setting the color theme.
(defmacro constexpr (b)
  (eval b))


;; in byte-comiled codes, it only runs during byte-compile stage.
;; so can be used to (require 'some-package) to silence byte-compiler warnigns,
;; but not actually required the package during init time.
(eval-when-compile)