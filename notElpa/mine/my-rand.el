;;; -*- lexical-binding: t -*-
;;; my-rand.el --- Helper functions for random values.

(random t) ;seed random with time

;;;###autoload
(defun rand (min max) ;TODO: use a custom implementation for speed.
  (+ min
     (random (+ 1
                (- max min)))))

;;;###autoload
(defun roll ()
  (rand 1 1000))

;;;###autoload
(defun rollCheck (val)
  (>= val (roll)))

;; testing distribution of `rand'
;; (let* ((min 1)
;;        (max 100)
;;        (rng (1+ (- max min)))
;;        (times 1000000)
;;        (target-occurs (/ times (* 1.0 rng)))
;;        (rVals (loop repeat times
;;                     collect (rand min max)))
;;        (sum (apply #'+ rVals))
;;        (len times) ; (len (length rVals))
;;        (avg (/ sum (* 1.0 len)))
;;        (counts (loop for n from min to max
;;                      collect 0)))
;;   ;; count the distribution
;;   (dolist (r rVals)
;;     (let ((index (- r 1)))
;;       (setf (elt counts index)
;;             (+ 1 (elt counts index)))))
;;   ;; print
;;   (insert (concat "avg: "
;;                   (number-to-string avg)
;;                   "\n"))
;;   (insert "target: " (number-to-string target-occurs))
;;   (insert "\n")
;;   (cl-loop for x from min to max
;;            do
;;            (insert (number-to-string x))
;;            (insert ": ")
;;            (insert (number-to-string (elt counts (1- x))))
;;            (insert "\n\n")))

(ert-deftest rand-range-overflow-test ()
  "Make sure `rand' generates values within range."
  (random t) ; seed
  (let* ((min 3) ; NOTE: small range [3-7]
         (max 7)
         ;; generate a ton of sample data over a small range.
         ;; to increase likelyhood an out of range bug will occur.
         (data (cl-loop repeat 1000 collect (rand min max))))
    (cl-flet ((out-of-range (r) (or (< r min) (> r max))))
      (should-not
       (cl-member-if #'out-of-range data)))))

(ert-deftest rand-range-underflow-test ()
  "Make sure `rand' does not have an 'underflow' range bug."
  (random t) ; seed
  (let* ((min 3) ; NOTE: small range [3-7]
         (max 7)
         (data (cl-loop repeat 1000 collect (rand min max))))
    (cl-loop for i from min to max
             do
             (should (member i data)))))

(provide 'my-rand)

;;; my-rand.el ends here
