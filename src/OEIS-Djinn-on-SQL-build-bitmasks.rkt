#lang racket

(require db)
(require racket/format)
(require srfi/1) ;; For fold

;;
;; OEIS-Djinn-on-SQL-build-bitmasks.rkt - For the OEIS Djinn project, Antti Karttunen started writing this module 2021-11-24
;; (Might contain a function or two common with OEIS-Djinn-on-SQL-find-links.rkt until code base is reorganized).
;;


(define djinn-c (sqlite3-connect #:database "./Adjinn-A101296.db"))
;; (define djinn-c (sqlite3-connect #:database "/home/antti/A/OEIS-Djinn-work/Adjinn-A032742.db"))


(define (create-tables c)
   (begin
;; These have been already created at the import time by the import-module:
;;   (query-exec c "CREATE TABLE Aseqs(Anum BIGINT NOT NULL, Nval BIGINT NOT NULL, Value BIGINT NOT NULL, PRIMARY KEY (Anum,Nval))")
;;   (query-exec c "CREATE UNIQUE INDEX AseqsAVN ON Aseqs(Anum,Value,Nval)")
;;   (query-exec c "CREATE TABLE Auploadstatus(Anum BIGINT NOT NULL, Uploaddate VARCHAR, Status VARCHAR)")

     (query-exec c "DROP TABLE Bitmasks128")
     (query-exec c "CREATE TABLE Bitmasks128(Anum BIGINT NOT NULL, Mask0 BIGINT NOT NULL, Mask1 BIGINT NOT NULL, PRIMARY KEY(Anum))")
    
   )
)

;; (create-tables djinn-c)


(define (Anum->str num) (~a num #:min-width 6 #:align 'right #:pad-string "0"))

(define (num->hex32digs-str num) (~r num #:base 16 #:min-width 32 #:pad-string "0"))

(define (form-bitmask-from-nums exps) (fold + 0 (map (lambda (e) (expt 2 e)) exps)))


;; Example:
;; (query_eq_classes-in-range-and-form-their-128bit-masks djinn-c 5)
;;  '(170152392186162032610075049835446806700
;;    2658455991569831745808177070547665424
;;    59527168536387273956858827597886309696
;;    21516879137458149662116497378761773056
;;    2417851639229258349477888
;;    22133179789773381095055667429900288
;;    1267650600228229401565422682112
;;    5192296859743753448426600480636928
;;    85070916329273724382753391323453063168)
;;
;;
;; (map num->hex32digs-str (query_eq_classes-in-range-and-form-their-128bit-masks djinn-c 10051))
;;   '("7ffdd75dfdf77d77d7df75df5f75d752" "800228a20208828828208a20a08a28ac")
;; (map num->hex32digs-str (query_eq_classes-in-range-and-form-their-128bit-masks djinn-c 10052))
;;   '("02000010000200010002001002010213" "fdffffeffffdfffefffdffeffdfefdec") ;; Note that A010052 has starting offset 0, that's why the other mask is odd.
;; (map num->hex32digs-str (query_eq_classes-in-range-and-form-their-128bit-masks djinn-c 10055))
;;   '("a20228a2020a828928228a21aa8b2bbe" "5dfdd75dfdf57d76d7dd75de5574d440")
;;
;;
;;


(define (query_eq_classes-in-range-and-form-their-128bit-masks c Anum)
   (let loop ((eclass-lists (query-eq_classes-in-range c Anum 0 127))
              (masks (list))
	     )
       (cond ((null? eclass-lists) (reverse masks))
             ((null? (cdar eclass-lists)) (loop (cdr eclass-lists) masks)) ;; Skip singleton classes.
             (else (loop (cdr eclass-lists) (cons (form-bitmask-from-nums (car eclass-lists)) masks)))
       )
   )
)



;;
;; Example:
;;
;; (query-eq_classes-in-range djinn-c 5 1 127)
;; -->
;; '((1)
;;   (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127)
;;   (4 9 25 49 121)
;;   (6 8 10 14 15 21 22 26 27 33 34 35 38 39 46 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95 106 111 115 118 119 122 123 125)
;;   (12 18 20 28 32 44 45 50 52 63 68 75 76 92 98 99 116 117 124)
;;   (16 81)
;;   (24 30 40 42 54 56 66 70 78 88 102 104 105 110 114)
;;   (36 100)
;;   (48 80 112)
;;   (60 72 84 90 96 108 126)
;;   (64)
;;   (120))
;;

(define (query-eq_classes-in-range c Anum range_begin range_end)
  (let ((sel1 (prepare c "select distinct(Value) from Aseqs where Anum = ? and Nval >= ? and Nval <= ?"))
        (sel2 (prepare c "select Nval from Aseqs where Anum = ? and Value = ? and Nval >= ? and Nval <= ?"))
       )
    (call-with-transaction c
       (lambda ()
          (let ((distvals (query-list c sel1 Anum range_begin range_end)))
             (map (lambda (v) (query-list c sel2 Anum v range_begin range_end)) distvals)
          )
       )
    )
  )
)


(define (query-form-csv-file-of-128bit-masks djinn-c outfilename)
  (call-with-output-file outfilename
     (lambda (out)
        (display
           (format (string-append
                         "# This is the 128-bit equivalence class masks file built in the OEIS Djinn project for a subset of OEIS data.~n"
                         "# Data is in comma-separated format, with each hex-number a 128-bit mask given in the little-endian format,~n"
                         "# in other words, the least significant bits and bytes appear on the right hand end of the hex-code.~n"
                         "# In each mask, positions k of 1-bits (in range 0..127) tell that for those k, the sequence obtains the same particular value.~n"
                         "# The masks that correspond to singleton equivalence classes, i.e., the powers of 2, have been left out.~n"
                         "# Use of this content is governed by the OEIS End-User License: http://oeis.org/LICENSE~n"
                         "# See also https://github.com/karttu/OEIS-Djinn~n"
                         "#"))
                   out
        )
        (let outloop ((anums (query-rows djinn-c "select distinct Anum from Aseqs"))
                      (c 0)
                      (max-num-of-masks 0)
                      (anum-with-max-num-of-masks 0)
                     )
              (cond ((null? anums)
                        (begin
                          (display (format "~n# Total of ~a sequences is included in this file. Sequence A~a has most masks, ~a in total.~n"
                                                 c (Anum->str anum-with-max-num-of-masks) max-num-of-masks) out)
                        )
                    )
                    (else
                      (let ((anum (vector-ref (car anums) 0)))
                        (display (format "~nA~a ," (Anum->str anum)) out)
                        (let inloop ((masks (query_eq_classes-in-range-and-form-their-128bit-masks djinn-c anum))
                                     (k 0)
                                    )
			   (cond ((null? masks)
                                     (outloop (cdr anums) (+ 1 c) (max max-num-of-masks k) (if (> k max-num-of-masks) anum anum-with-max-num-of-masks))
                                 )
                                 (else
                                    (begin
                                      (display (format "~a," (num->hex32digs-str (car masks))) out)
                                      (inloop (cdr masks) (+ 1 k))
                                    )
                                 )
                           )

                        )

                      )
                    )
              )
        )
     )
  )
)


(query-form-csv-file-of-128bit-masks djinn-c "./eqclass-128bit-masks-file-first-test-for-old-A101296-data.txt")




