;;; portfolio.el --- Helper funcs for portfolio allocations. -*- lexical-binding: t -*-

;;; Code:
(require 'cl-lib)

(defun leafp (x)
  (let ((er (cl-third x)))
    (or (numberp er)
        (null er))))

(defun tail (x)
  (cddr x))

(cl-defun build-lst (alloc total original-total)
  "Convert a portfolio of percentages to hard allocation numbers.
Also show percent against the original-total."
  (when (null alloc) ; base case
    (cl-return-from build-lst '()))

  (let* ((x (car alloc))
         (x-name (cl-first x))
         (x-percent (* 0.01 (cl-second x)))
         ;; if er is null, default to 0
         (x-er (or (cl-third x) 0)))

    (cond ((leafp x) ; it's a thing to buy
           (let* ((hard-amt (* total x-percent))
                  (hard-per (* (/ hard-amt original-total) 100)))
             (append `((,x-name
                        ,hard-amt
                        ,hard-per
                        ,x-er))
                     (build-lst (cl-rest alloc) total original-total))))
          (t ; else it's a group category
           (append (build-lst (tail x) (* total x-percent) original-total)
                   (build-lst (cl-rest alloc) total original-total))))))



(defvar indent "    ")

(cl-defun build-report-relative (alloc total original-total tabs)
  "Print the relative allocs in a buffer. Tab indented."
  (when (null alloc) ; base case
    (cl-return-from build-report-relative '()))

  (let* ((x (car alloc))
         (x-name (cl-first x))
         (percent-printable (cl-second x))
         (x-percent (* 0.01 (cl-second x)))
         ;; if er is null, default to 0
         (x-er (or (cl-third x) 0)))

    (cond ((leafp x) ; it's a thing to buy
           (let* ((hard-amt (* total x-percent))
                  (hard-per (* (/ hard-amt original-total) 100)))
             (insert (format "%s%s %.5g\n" tabs x-name percent-printable))
             (build-report-relative (cl-rest alloc) total original-total tabs)))
          (t ; else it's a group category
           (insert (format "%s%s %.5g\n" tabs x-name percent-printable))
           (build-report-relative (tail x) (* total x-percent) original-total (concat tabs indent))
           (build-report-relative (cl-rest alloc) total original-total tabs)))))

(cl-defun build-report (alloc total)
  (interactive)
  (let* ((buff (get-buffer-create "*alloc-report*"))
         (absolute-allocs (build-lst alloc total total))
         (er-weighted (weighted-er absolute-allocs))
         (er-fee (* total (* er-weighted 0.01))))

    (with-current-buffer buff
      (num3-mode)
      (erase-buffer)
      (insert "~~~~ Categorized allocations ~~~~\n")
      ;; print the relative allocs. tab indented
      (build-report-relative alloc total total "")

      (insert "\n\n~~~~ Absolute allocations ~~~~\n")
      ;; print the absolute-allocs
      (insert "Sym  %\t\tER\tAmount\n") ; col header
      (insert "------------------------------\n")
      (cl-loop for x in absolute-allocs
               do
               (let ((ticker (symbol-name (cl-first x)))
                     (amt (cl-second x))
                     (percent (cl-third x))
                     ;; if er is null, default to 0
                     (er (or (cl-fourth x) 0)))
                 (insert (format "%s %.4g\t%.4g\t$%.13g\n" ticker percent er amt))))

      ;; print total portfolio weighted ER and fee
      (insert "\n\n~~~~ Total portfolio weighted ER ~~~~\n")
      (insert (format "ER: %.4g\n" er-weighted))
      (insert (format "Fee/yr on $%.13g: $%.4g\n" total er-fee)))
    ;; show report buffer
    (switch-to-buffer-other-window buff)))

;; test report
(when nil
  (let ((total 1000))
    (build-report '((bond 10 (vbil 100 0.07))
                    (stock 90
                           (usa 80
                                (schk 90 0.03)
                                (avuv 10 0.25))
                           (intl 20
                                 (devel 95
                                        (schf 80 0.03)
                                        (avdv 20 0.33))
                                 (emerging 5
                                           (vexc 100 0.07)))))
                  total)))


(defun verify-allocs (allocs total)
  "Check if portfolio allocs are wrong.
Maybe due to the portfolio input allocs being wrong.
Or floating point arithmetic error.
In the case of floating point error sum should be very close to total.
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
                        "Off a slight amt due to floating point arithmetic.")
                       (t "NO!"))
      :total ,total
             :sum ,sum
             :diff ,diff
             :sum-percents ,sum-percents)))


(defun balance (des-alloc cur-alloc)
  "DES-ALLOC is can be the output of `build-lst'
CUR-ALLOC is list of (sym amt) pairs."
  (let ((to-buy '()))
    (cl-loop for x in des-alloc do
             (let* ((sym (car x))
                    (amt-tar (cl-second x))
                    (curr-obj (assoc sym cur-alloc))
                    (amt-curr (cl-second curr-obj)))
               (push `(,sym ,(- amt-tar amt-curr)) to-buy)))
    (reverse to-buy)))

(defun weighted-er (port)
  "Calculate the weighted expense ratio of the entire portfolio."
  (apply #'+
         (mapcar (lambda (x)
                   (let ((tick (nth 0 x))
                         ;; convert whole number percent to decimal percent. ie 1% -> 0.01
                         (per (/ (nth 2 x) 100))
                         (er (nth 3 x)))
                     (* per er)))
                 port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sample portfolio
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((bond 10  0.03)
                    (stock 90
                           (usa 80
                                ;; 95/5 schk/avuv roughly cap weight
                                (schk 90  0.03)
                                (avuv 10  0.25))
                           (intl 20
                                 ;; 75/25 reweight to 81/19 due to ex-C
                                 (devel 95
                                        ;; 90/10 roughly cap weight
                                        (schf 80  0.03)
                                        (avdv 20  0.33))
                                 (emerging 5
                                           (vexc 100  0.07))))))
       (total 1000.0)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total))
       (er (weighted-er allocs)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check
            :weighted-er ,er
            :er-fee ,(* total (* er 0.01))))


;;; sample balance
(balance '((baz 10000.0 10.0) ;target
           (foo 70000 70.0)
           (bar 20000 20.0))
         '((baz 2.80) ; current
           (foo 50000)
           (bar 100000)))

;;; sample weighted ER
(weighted-er '((foo 100 50.0 0)
               (bar 100 50.0 0.6)))


(provide 'portfolio)

;;; portfolio.el ends here
