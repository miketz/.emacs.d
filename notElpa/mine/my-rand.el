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
