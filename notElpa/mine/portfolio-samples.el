(require 'portfolio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; standard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((bond 10 (vbil 100  0.07))
                    (stock 90
                           (usa 70
                                ;; 95/5 schk/avuv roughly cap weight. Or prodcues a simliar 9 square style box.
                                (schk 85  0.03)
                                (avuv 15  0.25)) ;3x SCV style box
                           (intl 30
                                 ;; 75/25 reweight to 81/19 due to ex-C. further underweight emerging
                                 (devel 90
                                        ;; 90/10 roughly cap weight. Or prodcues a simliar 9 square style box.
                                        (schf 70  0.03)
                                        (avdv 30  0.36)) ;3x SCV style box
                                 (emerging 10
                                           (vexc 100  0.07))))))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total))
       (er (weighted-er allocs)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check
            :weighted-er ,er
            :er-fee ,(* total (* er 0.01)))
  ;;(build-report portfolio total)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TSP. L target date funds do 65/35 us/intl, 80/20 cFund/sFund split in the stock sleeve.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((bond 0
                          (gFund 50 0.037) ;treasuries
                          (fFund 50 0.037)) ;aggregate bond
                    (stock 100
                           (usa 60
                                (cFund 90 0.036) ;S&P 500
                                (sFund 10 0.051));mid/small caps. non S&P 500
                           (intl 40
                                 (iFund 100 0.038))))) ;international
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total))
       (er (weighted-er allocs)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check
            :weighted-er ,er
            :er-fee ,(* total (* er 0.01)))
  ;; (build-report portfolio total)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; f
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((bond 0 (schr 100  0.03))
                    (stock 100
                           (usa 60
                                ;; 95/5 schk/avuv roughly cap weight. Or prodcues a simliar 9 square style box.
                                (schk 85  0.03)
                                (avuv 15  0.25)) ;3x SCV style box
                           (intl 40
                                 ;; 75/25 reweight to 81/19 due to ex-C. further underweight emerging
                                 (devel 90
                                        ;; 90/10 roughly cap weight
                                        (schf 70  0.03)
                                        (avdv 30  0.36)) ;3x SCV style box
                                 (emerging 10
                                           (vexc 100  0.07))))))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total))
       (er (weighted-er allocs)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check
            :weighted-er ,er
            :er-fee ,(* total (* er 0.01)))
  ;; (build-report portfolio total)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; default. roughly cap weighted, except for US bias.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((bond 10
                          ;(vbil 10) ; very short. 0-3 month tbills
                          ;(scho 20) ; short. 1-3 years
                          ;(schr 20) ; intermediate. 3-10 years
                          ;(schq 25) ; long 10+ years
                          ;(schz 25) ; aggregate US bond market. has corporate bonds.
                          )
                    (stock 90
                           (usa 85
                                ;; 95/5 schk/avuv roughly cap weight
                                (schk 95)
                                (avuv 5))
                           (intl 15
                                 ;; 75/25 reweight to 81/19 due to ex-C
                                 (devel 81
                                        ;; 90/10 roughly cap weight
                                        (schf 90)
                                        (avdv 10))
                                 (emerging 19
                                           (vexc 100))))))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((scho 10)
                    (voo 90)))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check)
  ;; (build-report portfolio total)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ben felix small cap overweight.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((bond 10)
                    (stocks 90
                            (schk 42)
                            (schf 24)
                            (vexc 12)
                            (avuv 14)
                            (avdv 8))))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check)
  ;; (build-report portfolio total)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ginger ale. https://www.optimizedportfolio.com/ginger-ale-portfolio/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((VOO 25) ;us large cap
                    (AVUV 25) ;us smallcap val
                    (VEA 10) ;dev
                    (AVDV 10) ;dev smallcap val
                    (VWO 10) ;eme
                    (DGS 10) ;eme smallcap val
                    (EDV 10) ;extended duration treasury
                    ))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dragonfly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((schk 55)
                    (avuv 15)
                    (vxus 15)
                    (avdv 15)))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dragonfly. translated to grouping, swap some.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((usa 70
                         (schk 78.559 0.03)
                         (avuv 21.441 0.25))
                    (intl 30
                          (devel 90
                                 (schf 44.4449 0.03)
                                 (avdv 55.5556 0.36))
                          (emerging 10
                                    (vexc 100 0.07)))))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total))
       (er (weighted-er allocs)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check
            :weighted-er ,er
            :er-fee ,(* total (* er 0.01))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; influenced by dragonfly. reduce SCV tilts and 60/40 us/intl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((usa 60
                         (schk 85 0.03)
                         (avuv 15 0.25)) ;3x scv style box
                    (intl 40
                          (devel 90
                                 (schf 60 0.03)
                                 (avdv 40 0.36)) ;4x scv style box
                          (emerging 10
                                    (avxc 100 0.33)))))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total))
       (er (weighted-er allocs)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check
            :weighted-er ,er
            :er-fee ,(* total (* er 0.01)))
  ;; (build-report portfolio total)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paul Merriman, ultimate buy and hold. 50/50 us/intl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((avus 10 0.15) ;us lcb
                    (avlv 10 0.15) ;us lcv
                    (avsc 10 0.25) ;us scb
                    (avuv 10 0.25) ;us scv
                    (vnq 10 0.13) ;reit
                    (avde 10 0.23) ;int lcb
                    (dfiv 10 0.27) ;int lcv. or AVIV 0.25er
                    (avds 10 0.30) ;int scb
                    (avdv 10 0.36) ;int scv
                    (avem 10 0.33) ;em lcb
                    ))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total))
       (er (weighted-er allocs)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check
            :weighted-er ,er
            :er-fee ,(* total (* er 0.01)))
  ;; (build-report portfolio total)
  )
;; :weighted-er 0.242

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ultimate buy and hold. modified
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((usa 50
                         (head 80
                               (schk 70 0.03) ;us lcb
                               (avlv 30 0.15)) ;us lcv
                         (avuv 20 0.25)) ;us scv
                    (intl 50
                          (head 62
                                (schf 70 0.03) ;int lcb
                                (aviv 30 0.25)) ;int lcv
                          (avdv 24 0.36) ;int scv
                          (vexc 14 0.07)) ;em lcb
                    ))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total))
       (er (weighted-er allocs)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check
            :weighted-er ,er
            :er-fee ,(* total (* er 0.01)))
  ;; (build-report portfolio total)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paul Merriman, WW 4-fund. 50/50 us/intl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((avus 25 0.15) ;us lcb
                    (avuv 25 0.25) ;us scv
                    (dfiv 25 0.27) ;int lcv. or AVIV 0.25er
                    (avds 25 0.30) ;int scb
                    ))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total))
       (er (weighted-er allocs)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check
            :weighted-er ,er
            :er-fee ,(* total (* er 0.01)))
  ;; (build-report portfolio total)
  )
;; :weighted-er 0.2425


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paul Merriman, WW 4-fund. but with substitutions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((schk 25 0.03) ;us lcb
                    (avuv 25 0.25) ;us scv
                    (schf 25 0.03) ;replacing value with blend
                    (avds 25 0.30) ;int scb
                    ))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total))
       (er (weighted-er allocs)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check
            :weighted-er ,er
            :er-fee ,(* total (* er 0.01)))
  ;; (build-report portfolio total)
  )
;; :weighted-er 0.1525


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 60/40 us/intl. Value tilts on "head" to balance cap weighted indexes.
;; Small cap tails cut off, replaced with avuv/avdv. moreso a tail replacement than a tilt
;; lot's of ETFs but allows finer grained control and maintains ex-C style avoiding AVGV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((usa 50
                         (head 85
                               (tilt 20
                                    (AVLV 84 0.15)
                                    (AVMV 16 0.2))
                               (schk 80 0.03))
                         (avuv 15 0.25)) ;3x SCV style box
                    (intl 50
                          (devel 90
                                 (head 70
                                       (tilt 20
                                             (AVIV 100 0.25))
                                       (schf 80 0.03))
                                 (avdv 30 0.36)) ;3x SCV style box
                          (emerging 10
                                    (vexc 100 0.07)))))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total))
       (er (weighted-er allocs)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check
            :weighted-er ,er
            :er-fee ,(* total (* er 0.01)))
  ;; (build-report portfolio total)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 60/40 us/intl. value tilt on entire core portfolio with AVGV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((val-tilt 10 (AVGV 100 0.26))
                    (core 90
                          (usa 60
                               (schk 85 0.03)
                               (avuv 15 0.25)) ;3x SCV style box
                          (intl 40
                                (devel 90
                                       (schf 70 0.03)
                                       (avdv 30 0.36)) ;3x SCV style box
                                (emerging 10
                                          (vexc 100 0.07))))))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total))
       (er (weighted-er allocs)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check
            :weighted-er ,er
            :er-fee ,(* total (* er 0.01)))
  ;; (build-report portfolio total)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 60/40 us/intl. ZERO funds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((portfolio '((AVGV 20 0.26)
                    (core 80
                          (fzrox 60 0)
                          (fzilx 40 0))))
       (total 1000)
       (allocs (build-lst portfolio total total))
       (sanity-check (verify-allocs allocs total))
       (er (weighted-er allocs)))
  `(:allocs ,allocs
            :sanity-check ,sanity-check
            :weighted-er ,er
            :er-fee ,(* total (* er 0.01))))

