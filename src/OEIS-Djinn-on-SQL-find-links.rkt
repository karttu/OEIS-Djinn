#lang racket

(require db)
(require racket/format)

;;
;; OEIS-Djinn-on-SQL-find-links.rkt - For the OEIS Djinn project, Antti Karttunen started writing this 2017-12-02 (December 02 2017).
;;
;; Last edited 2017-12-22 by AK.
;;
;;
;; The module that builds the "family tree" of sequences found from Aseqs-table.
;; Here the family tree is a modified DAG (Directed Acyclic Graph), where a set of equivalent-class wise equivalent
;; sequences may be all "parents" or "daughters" of each other, with <=> relation.
;;
;; See also https://en.wikipedia.org/wiki/Equivalence_relation#Comparing_equivalence_relations
;;


;; (define djinn-c (sqlite3-connect #:database "c:/users/karttu/A/matikka/OEIS-Djinn/Adjinn-A101296.db"))
(define djinn-c (sqlite3-connect #:database "c:/users/karttu/A/matikka/OEIS-Djinn/Adjinn-A032742.db"))


(define (create-tables c)
   (begin
;; These have been already created at the import time by the import-module:
;;   (query-exec c "CREATE TABLE Aseqs(Anum BIGINT NOT NULL, Nval BIGINT NOT NULL, Value BIGINT NOT NULL, PRIMARY KEY (Anum,Nval))")
;;   (query-exec c "CREATE UNIQUE INDEX AseqsAVN ON Aseqs(Anum,Value,Nval)")
;;   (query-exec c "CREATE TABLE Auploadstatus(Anum BIGINT NOT NULL, Uploaddate VARCHAR, Status VARCHAR)")

     (query-exec c "DROP TABLE AimpliesB")
     (query-exec c "CREATE TABLE AimpliesB(Anum BIGINT NOT NULL,Bnum BIGINT NOT NULL,ShiftedBy BIGINT NOT NULL,BeginN BIGINT NOT NULL,EndN BIGINT NOT NULL,SepPoint BIGINT NOT NULL,WhenDate VARCHAR, PRIMARY KEY(Anum,Bnum,ShiftedBy))")

    
   )
)

(create-tables djinn-c)

(define (Anum->str num) (~a num #:min-width 6 #:align 'right #:pad-string "0"))


;; Returns triplet (Anum, Value, First-N-Where-Value-occurs).
;; Note that if for all i,j: A(i) = A(j) => B(i) = B(j), then the starting points
;; of each equivalence class in B (the third elements of the triplet) must be a subset
;; of the starting points for sequence A.

(define (query-anum-eqclass-values-and-starting-points c Anum)
  (let ((sel (prepare c "select v1.Anum, v1.Value, v1.Nval from Aseqs v1 where v1.Anum = ? and v1.Nval = (select v2.Nval from Aseqs v2 where v2.Anum = v1.Anum and v2.Value = v1.Value ORDER BY v2.Anum, v2.Value, v2.Nval LIMIT 1) ORDER BY v1.Nval")))
    (call-with-transaction c
       (lambda () (query-rows c sel Anum))
    )
  )
)


(define (query-eqclass-starting-points c Anum)
  (let ((sel (prepare c "select v1.Nval from Aseqs v1 where v1.Anum = ? and v1.Nval = (select v2.Nval from Aseqs v2 where v2.Anum = v1.Anum and v2.Value = v1.Value ORDER BY v2.Anum, v2.Value, v2.Nval LIMIT 1) ORDER BY v1.Nval")))
    (call-with-transaction c
       (lambda () (query-list c sel Anum))
    )
  )
)

(define (query-eqclass-starting-points-for-all-Anums c)
  (let ((sel (prepare c "select v1.Anum, v1.Nval from Aseqs v1 where v1.Nval = (select v2.Nval from Aseqs v2 where v2.Anum = v1.Anum and v2.Value = v1.Value ORDER BY v2.Anum, v2.Value, v2.Nval LIMIT 1) ORDER BY v1.Anum, V1.Nval")))
    (call-with-transaction c
       (lambda () (query-rows c sel))
    )
  )
)




(define (query-Anum-values-up-to-n c Anum uplim)
  (let ((sel (prepare c "select Value from Aseqs where Anum = ? AND Nval <= ? ORDER BY Nval")))
    (call-with-transaction c
       (lambda () (query-list c sel Anum uplim))
    )
  )
)


(define (query-domain-of c Anum)
  (let ((sel (prepare c "select min(Nval), max(Nval) from Aseqs where Anum = ?")))
    (call-with-transaction c
       (lambda () (vector->list (first (query-rows c sel Anum))))
    )
  )
)

(define (query-distinct-values c Anum)
  (let ((sel (prepare c "select distinct(Value) from Aseqs where Anum = ?")))
    (call-with-transaction c
       (lambda () (query-list c sel Anum))
    )
  )
)

(define (query-for-all-Anums-domain-and-number-of-eclasses c)
  (let ((sel (prepare c "select Anum, min(Nval), max(Nval), count(distinct Value) from Aseqs GROUP BY Anum")))
    (call-with-transaction c
       (lambda () (query-rows c sel))
    )
  )
)

(define (query-for-all-Anums-domain-and-number-of-eclasses-from-n-onward c lowlim)
  (let ((sel (prepare c "select Anum, min(Nval), max(Nval), count(distinct Value) from Aseqs where Nval >= ? GROUP BY Anum")))
    (call-with-transaction c
       (lambda () (query-rows c sel lowlim))
    )
  )
)


(define (query-number-of-distinct-values-up-to-n c uplim)
  (let ((sel (prepare c "select Anum, count(distinct Value) from Aseqs where Nval <= ? GROUP BY Anum")))
    (call-with-transaction c
       (lambda () (query-rows c sel uplim))
    )
  )
)

;; This doesn't work because too little memory:
;; "select v1.Anum from Aseqs v1 where ? >= (select count(distinct v2.Nval) from Aseqs v2 where v2.Anum = v1.Anum)"
;; Have to do some processing on the Scheme-side of things:

;; Call e.g. as (query-all-Anums-with-distinct-values-at-max-n-up-to-n djinn-c 1000 20)

(define (query-all-Anums-with-distinct-values-at-max-n-up-to-n c maxNval maxDistVals)
  (filter (lambda (v) (<= (vector-ref v 1) maxDistVals))
          (query-number-of-distinct-values-up-to-n c maxNval)
  )
)


;; Beware applying to big sets:
(define (query-distinct-nonunique-values c Anum)
  (let ((sel (prepare c "select distinct(v1.Value) from Aseqs v1 where v1.Anum = ? and 1 < (select count(distinct v2.Nval) from Aseqs v2 where v2.Anum = v1.Anum and v2.Value = v1.Value LIMIT 2)")))
    (call-with-transaction c
       (lambda () (query-list c sel Anum))
    )
  )
)




;; If I just understood the examples at https://docs.racket-lang.org/db/using-db.html#%28part._dbperf-n%2B1%29

(define (query-eq_classes c Anum)
  (let ((sel1 (prepare c "select distinct(Value) from Aseqs where Anum = ?"))
        (sel2 (prepare c "select Nval from Aseqs where Anum = ? and Value = ?"))
       )
    (call-with-transaction c
       (lambda ()
          (let ((distvals (query-list c sel1 Anum)))
             (map (lambda (v) (query-list c sel2 Anum v)) distvals)
          )
       )
    )
  )
)


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

(define (query-eq_classes-only-upto-n c Anum range_end)
  (let ((sel1 (prepare c "select distinct(Value) from Aseqs where Anum = ? and Nval <= ?"))
        (sel2 (prepare c "select Nval from Aseqs where Anum = ? and Value = ? and Nval <= ?"))
       )
    (call-with-transaction c
       (lambda ()
          (let ((distvals (query-list c sel1 Anum range_end)))
             (map (lambda (v) (query-list c sel2 Anum v range_end)) distvals)
          )
       )
    )
  )
)


(define (query-insert-mark-that-A-implies-B-in-given-range c Anum Bnum shifted_by range_begin range_end whendate)
  (let ((stmt (prepare c "INSERT INTO AimpliesB(Anum,Bnum,ShiftedBy,BeginN,EndN,SepPoint,WhenDate) VALUES(?,?,?,?,?,?,?)")))
     (query-exec c stmt Anum Bnum shifted_by range_begin range_end 0 whendate)
  )
)


(define (query-whether-already-marked-that-A-implies-B-in-given-range? c A_Anum B_Anum range_begin range_end)
  (let ((sel1 (prepare c "select BeginN, EndN from AimpliesB where Anum = ? and Bnum = ? and SepPoint = 0")))
    (call-with-transaction c
       (lambda ()
;; First we fetch a list of distinct values that occur in the sequence B, in the given range:
          (let ((domain-tested (query-rows c sel1 A_Anum B_Anum)))
            domain-tested
          )
       )
    )
  )
)

(define (query-and-mark-whether-A-implies-or-not-B-in-given-range? c Anum A_eclasses Bnum shifted_by range_begin range_end whendate)
  (let ((stmt (prepare c "INSERT INTO AimpliesB(Anum,Bnum,ShiftedBy,BeginN,EndN,SepPoint,WhenDate) VALUES(?,?,?,?,?,?,?)")))
    (cond ((query-whether-A-does-not-imply-B-in-given-range? c Anum A_eclasses Bnum range_begin range_end)
             =>
             (lambda (eclass)
               (let ((sp (query-exact-point-where-A-no-more-implies-B? c Anum A_eclasses Bnum range_begin range_end)))
                  (query-exec c stmt Anum Bnum shifted_by range_begin range_end sp whendate)
               )
             )
          )
          (else ;; it seems that A implies B (in the given range). SepPoint field is set to zero to mark that:
            (query-exec c stmt Anum Bnum shifted_by range_begin range_end 0 whendate)
          )
    )
  )
)


;; Do a binary search over [range_begin, range_end], searching the smallest n where it is clear
;; that A(i) = A(j) => B(i) = B(j) does not hold for range_begin <= i,j <= n.

(define (query-exact-point-where-A-no-more-implies-B? c Anum A_eclasses Bnum range_begin range_end)
  (let loop ((imin range_begin) (imax range_end))
     (cond ((< imax imin)
              (error (format
                  "~nINTERNAL ERROR! query-exact-point-where-A-no-more-implies-B?: A~a, A~a imax < imin (~a < ~a)~n"
                        (Anum->str Anum) (Anum->str Bnum) imax imin
                )
              )
           )
     )
     (let* ((imid (+ imin (/ (- imax imin (modulo (- imax imin) 2)) 2))) ;; imid = avg(imin,imax)
            (s0 (query-whether-A-does-not-imply-B-in-given-range? c Anum A_eclasses Bnum range_begin imid))
           )
       (cond ((not s0) ;; Still A => B in domain [range_begin, imid] ?
                 (if (query-whether-A-does-not-imply-B-in-given-range? c Anum A_eclasses Bnum range_begin (+ 1 imid))
                                  ;; But not  in domain [range_begin, imid+1] ?
                     (+ 1 imid) ;; The we have found the exact separation point.
                     (loop (+ imid 1) imax) ;; Otherwise search from the range [imid+1,imax]
                 )
             )
             (else (loop imin (- imid 1))) ;; Does not imply in [range_begin, imid], so search from [imin, imid-1].
       )
     )
  )
)

;; We should fail if for two distinct n, n_1 and n_2 in a single equivalence class of A,
;; the sequence B has different values.
;; Note that B still might have same values for two distinct n's in two different e-classes of A.
;; (B might be coarser than A).

;; Thus, for each whole eclass of B, if we remove the corresponding Nval's from eclasses of A's,
;; each such affected e-class should be removed totally (leaving just a ()), otherwise A is not
;; a refinement partition of B.


(define (query-whether-A-does-not-imply-B-in-given-range? c Anum A_eclasses Bnum range_begin range_end)
  (let ((sel1 (prepare c "select distinct(Value) from Aseqs where Anum = ? and Nval >= ? and Nval <= ?"))
        (sel2 (prepare c "select Nval from Aseqs where Anum = ? and Nval >= ? and Nval <= ? and Value = ?"))
       )
    (call-with-transaction c
       (lambda ()
;; First we fetch a list of distinct values that occur in the sequence B, in the given range:
          (let loop ((distvals (query-list c sel1 Bnum range_begin range_end)))
             (if (null? distvals) ;; If list of distinct values is finished...
                 #f               ;;  ... then we have found that A _seems_ to imply B in given range, so return #f
                 (let ((B-eclass (query-list c sel2 Bnum range_begin range_end (first distvals))))
                    (if (union-of-one-or-more-eclasses-of-up-to-limit? B-eclass A_eclasses range_end)
                        (loop (cdr distvals)) ;; Was OK, go to fetch the next eclass of B.
;; otherwise, that B-eclass wasn't dispersed nicely among eclasses of A, return the first offending eq.class of B:
                        B-eclass
                    )
                 )
             )
          )
       )
    )
  )
)


;; This is like union-of-one-or-more-eclasses-of? but we ignore all terms in eclasses that are > uplimit
(define (union-of-one-or-more-eclasses-of-up-to-limit? eclass eclasses uplimit)
   (every (lambda (ec)
            (let ((first-is? (not (not (member (first ec) eclass))))) ;; is first of ec in eclass ?
               (let loop ((ecl (rest ec))) ;; then check the other elements of ec, whether they agree...
                  (cond ((null? ecl) #t)
                        ((> (first ecl) uplimit) (loop (rest ecl))) ;; Skip the terms over the uplimit.
                        ((not (eq? (not (not (member (first ecl) eclass))) first-is?)) #f) ;; Failed
                        (else (loop (rest ecl)))
                  )
               )
            )
          )
          eclasses
   )
)

;; If we remove all elements of eclass from those lists in eclasses where those elements reside,
;; are such eclasses eliminated completely? If so, return #t, otherwise false if they leave "residues".
;; This is equivalent to asking a question whether the sum of sizes of eclasses where elements of eclass
;; reside is equal to the size of eclass. Or equally: whether for each eclass ec in eclasses either all or
;; none of its elements are in eclass.

;; (union-of-one-or-more-eclasses-of? '(2 5 7) '((1 3) (2) (4) (5 7) (6 8 9))) --> #t
;; 
;; (union-of-one-or-more-eclasses-of? '(2 5 7) '((1 3) (2) (4) (5 7 11) (6 8 9))) --> #f
;; 
;; (union-of-one-or-more-eclasses-of? '(2 5 7 11 4) '((1 3) (2) (4) (5 7 11) (6 8 9))) --> #t
;; 
;; (union-of-one-or-more-eclasses-of? '(4) '((1 3) (2) (4) (5 7 11) (6 8 9))) --> #t
;; 
;; (union-of-one-or-more-eclasses-of? '(5 4 7 3 11 1) '((1 3) (2) (4) (5 7 11) (6 8 9))) --> #t
;; 
;; (union-of-one-or-more-eclasses-of? '(5 4 3 11 1) '((1 3) (2) (4) (5 7 11) (6 8 9))) --> #f
;;


(define (union-of-one-or-more-eclasses-of? eclass eclasses)
   (every (lambda (ec)
            (let ((first-is? (not (not (member (first ec) eclass))))) ;; is first of ec in eclass ?
               (let loop ((ecl (rest ec))) ;; then check the other elements of ec, whether they agree...
                  (cond ((null? ecl) #t)
                        ((not (eq? (not (not (member (first ecl) eclass))) first-is?)) #f) ;; Failed
                        (else (loop (rest ecl)))
                  )
               )
            )
          )
          eclasses
   )
)


(define (every pred? lista)
   (cond ((null? lista) #t)
         ((not (pred? (first lista))) #f)
         (else (every pred? (rest lista)))
   )
)


;; Beware applying this to big sets like A101296:
(define (query-anum-eqclass-limits-and-sizes c Anum)
  (let ((sel (prepare c "select v1.Anum, v1.Value, min(v1.Nval), max(v1.Nval), count(distinct v2.Nval) from Aseqs v1 INNER JOIN Aseqs v2 where v1.Anum = ? and v2.Anum = v1.Anum and v1.Value = v2.Value GROUP BY v1.Value")))
    (call-with-transaction c
       (lambda () (query-rows c sel Anum))
    )
  )
)


(define n-rows (query-list djinn-c "select count(*) from Aseqs"))

(define Anums-num-of-eclasses (query-rows djinn-c "select Anum, count(distinct Value) from Aseqs GROUP BY Anum"))

(define Anums-num-of-eclasses-upto1000 (query-number-of-distinct-values-up-to-n djinn-c 1000))




(define Anumbers (query-rows djinn-c "select distinct Anum from Aseqs"))

(define How-many-Anumbers (query-rows djinn-c "select count(distinct Anum) from Aseqs"))

(define Anumber-and-max-n (query-rows djinn-c "select Anum, max(Nval) from Aseqs GROUP BY Anum"))

;; Different: (define Anumber-and-count-n (query-rows djinn-c "select Anum, count(Nval) from Aseqs GROUP BY Anum"))


;; (define A107067startset (query-anum-eqclass-values-and-starting-points djinn-c 107067))

;; (define A107067limset (query-anum-eqclass-limits-and-sizes djinn-c 107067))

;; (define A107067distvals (query-distinct-values djinn-c 107067))

;; (define A107067dist_nonuniqs (query-distinct-nonunique-values djinn-c 107067))

;; (define A101296startset (query-anum-eqclass-values-and-starting-points djinn-c 101296))

;; (define A1070675eclasses (query-eq_classes djinn-c 107067))


;; (define A101296limset (query-anum-eqclass-limits-and-sizes djinn-c 101296)) ;; Too much for DrRacket + sqlite!

;; (define A294875startset1 (query-anum-eqclass-values-and-starting-points djinn-c 294875))

;; (define A294875startset (query-eqclass-starting-points djinn-c 294875))

;; (define A294875distvals (query-distinct-values djinn-c 294875))


;; (define A294875dist_nonuniqs (query-distinct-nonunique-values djinn-c 294875))


;; (define A101296eclasses (query-eq_classes djinn-c 101296))

;; (define Auploads (query-rows djinn-c "select * from Auploadstatus"))


(define (seqinfo-num-of-eclasses si) (vector-ref si 3))
(define (seqinfo-domain si) (list (vector-ref si 1) (vector-ref si 2)))
(define (seqinfo-domain-start si) (vector-ref si 1))
(define (seqinfo-domain-end si) (vector-ref si 2))
(define (seqinfo-anumber si) (vector-ref si 0))

(define (seqinfos-domain-of-A-contains-domain-of-B? siA siB)
   (and (>= (seqinfo-domain-start siB) (seqinfo-domain-start siA))
        (<= (seqinfo-domain-end siB) (seqinfo-domain-end siA))
   )
)


(define (seqinfos-common-domain-of-AC-subset-of-AB-and-BC? siA siB siC)
  (let ((AC-start (max (seqinfo-domain-start siA) (seqinfo-domain-start siC)))
        (AC-end (min (seqinfo-domain-end siA) (seqinfo-domain-end siC)))
       )
     (and (>= AC-start (max (seqinfo-domain-start siA) (seqinfo-domain-start siB)))
          (>= AC-start (max (seqinfo-domain-start siB) (seqinfo-domain-start siC)))
          (<= AC-end (min (seqinfo-domain-end siA) (seqinfo-domain-end siB)))
          (<= AC-end (min (seqinfo-domain-end siB) (seqinfo-domain-end siC)))
     )
  )
)


(define MASTERLIST (sort (query-for-all-Anums-domain-and-number-of-eclasses-from-n-onward djinn-c 2)
                         (lambda (si1 si2)
                            (cond ((< (seqinfo-num-of-eclasses si1) (seqinfo-num-of-eclasses si2)) #t) ;; Sequences with least eq.classes come to the front
                                  ((> (seqinfo-num-of-eclasses si1) (seqinfo-num-of-eclasses si2)) #f)
                                  ((> (seqinfo-domain-end si1) (seqinfo-domain-end si2)) #t) ;; If tie by eq.classes, then the longest ones come to the front
                                  ((< (seqinfo-domain-end si1) (seqinfo-domain-end si2)) #f)
                                  (else (< (seqinfo-anumber si1) (seqinfo-anumber si2))) ;; If also tie by the length, then the lesser A-number comes to the front
                            )
                         )
                   )
)

;;
;;
;; (define A295885eclasses (query-eq_classes-in-range djinn-c 295885 2 65536))
;;
;; (query-and-mark-whether-A-implies-or-not-B-in-given-range? c Anum A_eclasses Bnum shifted_by  (seqinfo-domain-start siA)  (seqinfo-domain-end siA) whendate)
;;
;;  (seqinfos-common-domain-of-AC-subset-of-AB-and-BC? siA siB siC)
;;
;;

(define A032742startset (query-eqclass-starting-points djinn-c 032742))

(define A032742eclasses (query-eq_classes djinn-c 032742))

(define A032742_implies_itself? (query-whether-A-does-not-imply-B-in-given-range? djinn-c 03274 A032742eclasses 032724 1 10000))

(define A014673eclasses (query-eq_classes djinn-c 014673))

(define A032742_implies_A014673? (query-whether-A-does-not-imply-B-in-given-range? djinn-c 03274 A032742eclasses 014673 1 10000))

(define A014673_implies_A032742? (query-whether-A-does-not-imply-B-in-given-range? djinn-c 14673 A014673eclasses 032742 1 10000))

(define A032742-domain-range (query-domain-of djinn-c 32742))

(query-and-mark-whether-A-implies-or-not-B-in-given-range? djinn-c 032742 A032742eclasses 014673 0 1 10000 "2017-12-22")

(query-and-mark-whether-A-implies-or-not-B-in-given-range? djinn-c 014673 A014673eclasses 032742 0 1 10000 "2017-12-22")

;; (define A295885eclasses (query-eq_classes-in-range djinn-c 295885 2 65536))

;; (define A101296eclasses (query-eq_classes-in-range djinn-c 101296 2 65536))

;; (query-and-mark-whether-A-implies-or-not-B-in-given-range? djinn-c 295885 A295885eclasses 101296 0 2 10000 "2017-12-22")

;; (query-and-mark-whether-A-implies-or-not-B-in-given-range? djinn-c 101296 A101296eclasses 295885 0 2 10000 "2017-12-22")

;; (query-rows djinn-c "select * from AimpliesB")
;; -->  '(#(295885 101296 0 2 10000 3283 "2017-12-22") #(101296 295885 0 2 10000 9 "2017-12-22"))

(define implies-results (query-rows djinn-c "select * from AimpliesB")) ;;



;;
;; We should construct into table AimpliesB for all 2*C(n,2) = n*(n-1) pairs of n sequences A & B
;; information whether certainly it is not true that A => B (set SepPoint to a larger than zero value)
;; of whether it seems that A => B in the given common range of their domains BeginN .. EndN. (set SepPoint to 0)
;;
;;
;; The only case where we can optimize this construction is when we already know that B => C (in their common domain)
;; then if we detect that A => B (in their common domain), and see that
;; comdom(A,C) is a subset of both comdom(A,B) and comdom(B,C), that is, comdom(A,C) is an
;; intersection of comdom(A,B) and comdom(B,C), only then we can be sure that also A => C (in their common domain).
;;
;;

;;
;; The construction proceeds by first constructing MASTERLIST (see above)

;; When browsing MASTERLIST, we should check for each A, with query-whether-A-does-not-imply-B-in-given-range?
;; in case (1) if domain(B) is a subset of domain(A) (that is minN(B) >= minN(a) and maxN(B) <= maxN(A))
;; only such B's that num_eclasses(B) <= num_eclasses(A).
;; Otherwise, if domain(B) is not completely inside domain(A), then check it in any case.
;;
;;
;;

;;
;; First we want a list of all Anumbers in database, sorted by
;;
;;  (a) the number of equivalence classes (number of distinct values),
;;      seqs with least e-classes first (we start from bottom)
;;      and in case of ties,
;;  (b) the seqs with more terms in database before the shorter ones.
;;
;; This is because, certainly it cannot hold that
;;   A(i) = A(j) => B(i) = B(j) for all i, j
;; if B has more equivalence classes in their common range than A has.
;;

;; So to find potential children of A, we need just to look for
;; sequences with at most the same number of equivalence classes
;; in their common range.


;;
;; At top level loop, each sequence A is checked if it matches to any sequence B
;; (in the sense that for all i,j: A(i) = A(j) => B(i) = B(j)).
;; in the set S that consists of:
;;
;;  (1) All sequences that have lte amount of equivalence classes in their common range
;;
;;  (2) We know that A => B => C (i.e. A => B and B => C) is either wholly true or wholly false
;;      whenever their ranges are "monotonic" in a sense that common_range(A,C) is
;;      a subset (an intersection?) of both common_range(A,B) and common_range(B,C).
;;      Because of this, we start searching for potential matches B from the "high end" of set S
;;      (those sequences with most equivalence classes in their common range that are still <=
;;       # of eq.classes for A), and for all already found children C of B, all such
;;      children for which the monotonicity condition holds, are immediately marked
;;      also as children of A.
;;
;;      We continue this process for all coarser/equally coarse sequences than A that have not
;;      yet been marked as children of A (including such children C of B for which the monotonic range
;;      condition does not hold), and either mark them as children of A,
;;      or mark the least n (failed_as_child_at) for which they B fail to be children of A. (<-- How to do that ?)
;;      Naturally, failed_as_child_at must be <= upper limit of common_range(A,B).
;;      To find failed_as_child_at, start removing one-by-one numbers from the upper end of that common_range(A,B)
;;      for which A(i) = A(j) => B(i) = B(j) is checked up to. failed_as_child_at is one more than the
;;      least upper limit where A(i) = A(j) => B(i) = B(j) seems to hold.
;;


;; 
;; # Call:
;; #
;; # has_coarser_eq_classes_with_threshold(seq1,off,get_nonsingleton_inverses(seq2,off),at_least_n_distinct_classes)
;; # returns True iff, based on available data, it holds that:
;; #
;; #   for all applicable i, j: seq2(i) = seq2(j) => seq1(i) = seq1(j),
;; #
;; # i.e., in terms of https://en.wikipedia.org/wiki/Equivalence_relation#Comparing_equivalence_relations
;; # the equivalence classes of seq1 are coarser (or equally coarse) than the equivalence classes of seq2,
;; # and furthermore:
;; #
;; #   'seq1' is divided to 'at_least_n_distinct_classes' different equivalence classes.
;; #
;; # This last argument (if optionally set to value > 1) provides a way of filtering off
;; # all constant sequences (A000004, A000012) or near-constant like A063524 or A057427,
;; # as, although they in the most cases would be real matches (not false positives in technical sense),
;; # they are still of very little interest for the user.
;; #
;; # Neither we want spurious false-positive matches with sequences like these two:
;; # A103847 McCarthy's 91 Function: a(n) = n-10 if n>100, otherwise a(n) = a(a(n+11)).
;; # A107844 Highest value obtained in the recursion of McCarthy'a 91 function until it terminates.
;; # where the first increase from a repeating term does not happen until after n=100.
;; 
;; 
;; def has_coarser_eq_classes_with_threshold(seq1,off,inverses_of_seq2,at_least_n_distinct_classes):
;;   '''Returns non-false if seq1 obtains an identical value for all members of any particular eq.class of seq2, for all of its equivalence classes, given as a dictionary as the third argument. Also seq1 must have at least at_least_n_distinct_classes.'''
;;   seq1len = len(seq1)
;; 
;;   first_inverses_in_seq1 = {}
;;   distinct_classes = 0
;; 
;; 
;;   for val in inverses_of_seq2:
;;     e = None
;;     for k in inverses_of_seq2[val]:
;;       if k-off >= seq1len:
;;         continue
;;       elif (None == e): # e not initialized yet.
;; # so e is set to the value that seq1 should obtain for ALL k in this particular equivalence class of seq2:
;;         e = seq1[k-off]
;;         if not(e in first_inverses_in_seq1): # This e not encountered before?
;;           first_inverses_in_seq1[e] = 1
;;           distinct_classes = distinct_classes + 1
;; 
;;       elif seq1[k-off] <> e: # found that seq1 obtains a different value at some point in the same e.c. of seq2,
;;         return(False)        # thus their equivalence classes do not match, and return false immediately.
;; 
;; 
;;   return(distinct_classes)
;; 
;; 
;; ;;
;; 
;; ;;
;; 
;; ;;
;; ;;
;; ;;
;; 
;; 
;; ;;
;; ;;
;; ;;
;; 
;; ;; For 
;; ;;   A(i) = A(j) => B(i) = B(j) to hold for all i, j
;; ;; then (at least):
;; ;;
;; ;; (1) B should have no more equivalence classes in their common range than A has,
;; ;; (2) {query-eqclass-starting-points for B} \ {starting points of B outside their common range (the range of A)}
;; ;;     should be a subset of {query-eqclass-starting-points for A}.
;; ;; And the final condition:
;; ;; (3) The set partition of integers by (query-eq_classes A) (in their common range) should be
;; ;;     a refinement of the set partition of integers by (query-eq_classes B) (in their common range).
;; ;;     That is, for any A(i) = A(j) with i, j in their common range, we should have B(i) = B(j).
;; ;;
;; ;;
;; 
;; ;; #
;; ;; # For example:
;; ;; # get_inverses(A046523,1)
;; ;; # {32: [32], 1: [1], 2: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89], 4: [4, 9, 25, 49], 6: [6, 10, 14, 15, 21, 22, 26, 33, 34, 35, 38, 39, 46, 51, 55, 57, 58, 62, 65, 69, 74, 77, 82, 85, 86, 87], 48: [48, 80], 8: [8, 27], 64: [64], 12: [12, 18, 20, 28, 44, 45, 50, 52, 63, 68, 75, 76], 16: [16, 81], 72: [72], 24: [24, 40, 54, 56, 88], 36: [36], 60: [60, 84], 30: [30, 42, 66, 70, 78]}
;; ;; #
;; ;; #
;; ;; 
;; ;; def get_inverses(seq,off):
;; ;;   '''Returns a hash table (dictionary) of all (v, list of positions i where seq(i) = v) for the seq.'''
;; ;;   seqlen = len(seq)
;; ;; 
;; ;;   inverses = {}
;; ;; 
;; ;;   for i in range(seqlen):
;; ;;     if seq[i] in inverses: inverses[seq[i]].append(i+off)
;; ;;     else: inverses[seq[i]] = [i+off]
;; ;; 
;; ;;   return(inverses)
;; ;; 
;; ;;
;; 