;;; portfolio.el --- Helper funcs for portfolio allocations. -*- lexical-binding: t -*-

;;; Code:
(require 'cl-lib)

(defun leafp (x)
  (null (cddr x)))

(defun tail (x)
  (cddr x))

(cl-defun build-lst (alloc total original-total)
  "Convert a portfolio of percentages to hard allocation numbers.
Also show percent against the original-total."
  (when (null alloc) ; base case
    (cl-return-from build-lst '()))

  (let* ((x (car alloc))
         (x-name (cl-first x))
         (x-percent (* 0.01 (cl-second x))))

    (cond ((leafp x) ; it's a thing to buy
           (let* ((hard-amt (* total x-percent))
                  (hard-per (* (/ hard-amt original-total) 100)))
             (append `((,x-name
                        ,hard-amt
                        ,hard-per))
                     (build-lst (cl-rest alloc) total original-total))))
          (t ; else it's a group category
           (append (build-lst (tail x) (* total x-percent) original-total)
                   (build-lst (cl-rest alloc) total original-total))))))

(defun verify-allocs (allocs total)
  "Check if portfolio allocs are wrong.
Maybe due to the portfolio input allocs being wrong.
Or floating point arithmetic error.
In the case of flaoting point error sum should be very close to total.
sum-percents should be 100."
  (let* ((sum (cl-loop for x in allocs
                       sum (cl-second x)))
         (diff (- total sum))
         (sum-percents (cl-loop for x in allocs
                                sum (cl-third x))))
    `(:correct? ,(cond ((and (= total sum)
                             (= sum-percents 100))
                        "yes")
                       ((and (< (abs diff) 0.7)
                             (< (abs (- 100 sum-percents)) 0.01))
                        "Off a slight amt due to flaoting point arithmetic.")
                       (t "NO!"))
      :total ,total
             :sum ,sum
             :diff ,diff
             :sum-percents ,sum-percents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sample portfolio
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar portfolio
  '((bonds 10
           (vbil 50)
           (schq 50))
    (stocks 90
            (usa 85
                 (schk 95)
                 (avuv 5))
            (intl 15
                  (schf 75)
                  (vexc 25)))))

(let* ((total 100000.0)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check))





(provide 'portfolio)

;;; portfolio.el ends here
