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
;; (let* ((times 1000000)
;;        (rVals (loop for i from 1 to times
;;                     collect (rand 1 100)))
;;        (sum (apply #'+ rVals))
;;        (len (length rVals))
;;        (avg (/ sum len))
;;        (counts (loop for n from 0 to 99
;;                      collect 0)))
;;   ;; count the distribution
;;   (dolist (r rVals)
;;     (setf (elt counts (- r 1))
;;           (+ 1 (elt counts (- r 1)))))
;;   ;; print
;;   (insert (concat "avg: "
;;                   (int-to-string avg)
;;                   "\n"))
;;   (insert "target: " (int-to-string (/ times 100)))
;;   (insert "\n")
;;   (dotimes (x 100)
;;     (insert (int-to-string (+ x 1)))
;;     (insert ": ")
;;     (insert (int-to-string (elt counts x)))
;;     (insert "\n\n")))


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
