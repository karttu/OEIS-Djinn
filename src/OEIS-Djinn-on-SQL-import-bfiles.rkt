#lang racket

(require db)
(require racket/date)
(require racket/format)



;;
;; OEIS-Djinn-on-SQL-import-bfiles.rkt - For the OEIS Djinn project, Antti Karttunen started writing this 2017-12-02 (December 02 2017).
;;
;; Copyright (C) 2017-2025 by Antti Karttunen. Contact his-firstname.his-surname@gmail.com for questions!
;;
;; This module is for inserting data from bfiles to the SQL-table Aseqs.
;;
;;   2017-12-03: Implemented RGS-transformed input that can be used with b-files that have huge terms.
;;
;;   2025-01-30: Renamed Auploadstatus to Afileinfo, and added many columns there.
;;               Implemented a-file referrals in scan-b-file
;;


(define djinn-dbname "Adjinn-A379000.db")
(define djinn-c (sqlite3-connect #:database djinn-dbname #:mode 'create))


;; If abs(a(n)) in a b-file is >= *MAX-ABSVAL-FOR-BIGINTS* for any n, then
;; the RGS-transform (Restricted Growth Sequence) is done to the data before it is uploaded
;; to the database. So, if you set this value to zero then all data is RGS-transformed prior
;; to import.

(define *MAX-ABSVAL-FOR-BIGINTS* (- (expt 2 63) 1))

;; See http://jakegoulding.com/blog/2011/02/06/sqlite-64-bit-integers/
;; See http://www.sqlite.org/datatype3.html

(define (create-tables c)
   (begin
     (query-exec c "CREATE TABLE Aseqs(Anum BIGINT NOT NULL, Nval BIGINT NOT NULL, Value BIGINT NOT NULL, PRIMARY KEY (Anum,Nval))")

     (query-exec c "CREATE UNIQUE INDEX AseqsAVN ON Aseqs(Anum,Value,Nval)")

     (query-exec c "CREATE TABLE Afileinfo(Anum BIGINT NOT NULL, Status VARCHAR, BfileSynthesized TINYINT, Uploadtime BIGINT NOT NULL, Bfilename VARCHAR, Bfilemodtime BIGINT NOT NULL, BfileBegin INT, BfileEnd INT)") ;; Was Auploadstatus. Many new columns added. If you say that now the data is not in the normal form, then please note that Aseqs table might additionally populated with, say, on-the-fly generated data (in future, that is), so its min and max Nval's might not agree with BfileBegin and BfileEnd values in this table, that semantically tell about the properties of b-file.
   )
)


(create-tables djinn-c)


(define (Anum->str num) (~a num #:min-width 6 #:align 'right #:pad-string "0"))


;; The import is done somewhat naively. First we scan the b-file, for checking the correctness
;; of its data, and also for finding the maximum value of abs(n), which can then be used
;; for deciding whether the data needs a RGS-transform.
;; Only after that is done the actual upload, with the b-file opened again for reading,
;; in the same somewhat clumsy way.

;; Returns either a list of three integers, (start end max_absval) where the third element is the maximum
;; absolute value encountered in the b-file (as a term),
;; or returns a string (some variant of "CORRUPT") if it was detected that the file is corrupt in some way.

;; Returns a list of 7 elements, the first element is a status string, either "OK" of some variant of "CORRUPT"
;; the other elements are filename (that was eventually used), its modification time, the least index used in
;; the file, the last index used in the file, maximum absolute value encountered in the file.
;;

(define (scan-b-file Anumber infile allow-afile-refs?)
 (let ((filemodtime (file-or-directory-modify-seconds infile)))
 (display (format "~nA~a --> Scanning b-file ~a~n"(Anum->str Anumber) infile))
  (call-with-input-file infile
    (lambda (inport)
        (let loop ((first_line_read? #f) (synthesized? #f) (prev_n #f) (first_n #f) (max_absval #f))
          (let ((line (read-line inport)))
             (cond ((eof-object? line)
                      (if (not first_n)
                        (begin
                           (display
                               (format "!!!ERROR (A~a): b-file ~a is empty! (no terms!).~n"
                                                (Anum->str Anumber) infile
                               )
                           )
                           (list (string-append "CORRUPT (empty file?): " (format "~a" infile))
                                 infile filemodtime synthesized? first_n prev_n max_absval
                           )
                        )
                        (begin ;; Else, it's OK.
                           (display
                               (format "A~a b-file ~a OK, n=~a..~a, max abs value = ~a.~n"
                                      (Anum->str Anumber) infile first_n prev_n max_absval
                               )
                           )
                           (list "OK" infile filemodtime synthesized? first_n prev_n max_absval)
                        )
                      )
                   )
;; Check for things like these on the very first line:
;; # Terms 1 - 100000 in https://oeis.org/A300385/a300385.txt
;; # Terms 1 - 100000 in https://oeis.org/A309891/a309891_1.txt
;; # A006021 (b-file synthesized from sequence entry)

                   ((and (not first_line_read?) (equal? (string-ref line 0) #\#))  ;; Comment-line in the very beginning of the file?
                      (let ((len (string-length line)))
                        (cond ((and allow-afile-refs? (> len 50) (equal? (substring line 1 8) " Terms "))
                                 (let* ((afilename (last (string-split line "/")))
                                        (afilenamelen (string-length afilename)))
                                    (cond ((and (> afilenamelen 4)
                                                (string-suffix? afilename ".txt")
                                                (not (equal? afilename infile))
                                                (file-exists? afilename)
                                           ) (scan-b-file Anumber (string->path afilename) #f) ;; Recurse on a new filename, do not allow additional (nested) afile-refs
                                          )
                                          (else
					    (list (string-append "CORRUPT a-file REFERENCE FROM: " (format "~a TO: ~a" infile afilename))
                                                  infile filemodtime synthesized? first_n prev_n max_absval
                                            )
                                          )
                                    )
                                 )
                              )
                              ((string-contains? line " synthesized ")
                                  (loop #t #t first_n prev_n max_absval)
                              )
                              (else (loop #t synthesized? first_n prev_n max_absval)) ;; Skip any other initial comment line.
                        )
                      )
                   )

                   ((or (zero? (string-length line))      ;; Skip empty lines.
                        (equal? (string-ref line 0) #\#)  ;; And also the comment lines.
                    )
                      (loop #t synthesized? first_n prev_n max_absval)
                   )
                   (else
                      (let* ((sis (open-input-string line))
                             (n_in_file (read sis))
                             (an_in_file (read sis))
                            )
                        (begin  ;; Skip lines with just white space (it is not an error):
                           (cond ((eof-object? n_in_file) (loop #t synthesized? first_n prev_n max_absval))
                                 ((or (not (integer? n_in_file)) (not (integer? an_in_file)))
                                     (display
                                       (format
          "!!!ERROR (A~a): When scanning b-file ~a, detected an index or value that is not an integer: a(~a) = ~a ? Line=~s, prev_n = ~a.~n"
                                             (Anum->str Anumber) infile n_in_file an_in_file line prev_n
                                       )
                                     )
                                     (list
                                        (string-append "CORRUPT (non-integer index or value) in: " (format "~a after line ~a" infile prev_n))
                                        infile filemodtime synthesized? first_n prev_n max_absval
                                     )
                                 )

                                 ((and prev_n (not (= n_in_file (+ 1 prev_n))))
                                     (display
                                       (format
          "!!!ERROR (A~a): When scanning b-file ~a, detected noncosecutive index ~a != ~a+1.~n"
                                             (Anum->str Anumber) infile n_in_file prev_n
                                       )
                                     )
                                     (list
                                        (string-append "CORRUPT (index mismatch) in: " (format "~a after line ~a" infile prev_n))
                                        infile filemodtime synthesized? first_n prev_n max_absval
                                     )
                                 )
                                 (else
                                     (loop #t synthesized? n_in_file
                                           (or first_n n_in_file)
                                           (if (or (not max_absval) (> (abs an_in_file) max_absval)) an_in_file max_absval)
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
 )
)


(define (ensure-path-is-string path-or-string) (if (path? path-or-string) (some-system-path->string path-or-string) path-or-string))

;; 

;; Give maxval-for-bigints as 0 if you want to do RGS-transform for _every_ sequence.

(define (store-b-file-to-database c Anumber infile uploadtime maxval-for-bigints)
  (let* ((sr (scan-b-file Anumber infile #t)) ;; sr (scan-result) is like: (list "OK" infile filemodtime synthesized? first_n prev_n max_absval)
         (status (list-ref sr 0))
         (infilename (list-ref sr 1)) ;; Might be different from the original infile, if there was an a-number reference.
         (filemodtime (list-ref sr 2)) ;; file modification time of the infile, or of afile if referred to.
         (synthesized-flag (if (list-ref sr 3) 1 0))
	 (first_n (list-ref sr 4))
	 (last_n (list-ref sr 5))
	 (max-absval (list-ref sr 6))
         (insert-fileload-status (prepare c "INSERT INTO Afileinfo(Anum,Status,BfileSynthesized,Uploadtime,Bfilename,Bfilemodtime,BfileBegin,BfileEnd) VALUES (?,?,?,?,?,?,?,?)"))
        )

     (if (not (equal? "OK" status)) ;; There was something wrong with the b-file?
         (call-with-transaction c
           (lambda () (query-exec c insert-fileload-status Anumber status synthesized-flag uploadtime (ensure-path-is-string infilename) filemodtime (or first_n 0) (or last_n 0)))
         )
         (let* ((data-status
                   (cond ((>= max-absval maxval-for-bigints) "RGS") ;; Restricted Growth Sequence transform needed?
                         (else "NORMAL") ;; The data is OK as it is.
                   )
                )
                (vector-for-RGS-conversion (and (equal? "RGS" data-status) (make-vector (+ 1 (- last_n first_n)))))
               )
            (begin
              (display
                (format "A~a --> Inserting terms to database from file ~a ~a.~n"
                        (Anum->str Anumber) infilename
                        (if (equal? "RGS" data-status)
                            "with RGS (Restricted Growth Sequence) transform applied"
                            "in normal mode (every term as it is)"
                        )
                )
              )
              (call-with-input-file infilename
                (lambda (inport)
                  (let ((insert-row (prepare c "INSERT INTO Aseqs(Anum,Nval,Value) VALUES (?,?,?)")))
                    (call-with-transaction c
                      (lambda ()
                         (begin
                           (store-b-file-to-database-do-it c Anumber infilename inport insert-row first_n vector-for-RGS-conversion)
                           (query-exec c insert-fileload-status Anumber data-status synthesized-flag uploadtime (ensure-path-is-string infilename) filemodtime (or first_n 0) (or last_n 0))
			 ) 
                      )
                      #:option 'immediate
                    )
                  )
                )
              )
            )
         )
     )
  )
)


(define (store-b-file-to-database-do-it c Anumber infile inport insert-row first_n vector-for-RGS-conversion)
  (let ((rgs-occurrences (and vector-for-RGS-conversion (make-hash)))) ;; Make equal? based hash-table
        (let loop ((prev_n #f) (first-unused-number-for-RGS 1))
          (let ((line (read-line inport)))
             (cond ((eof-object? line)
                       (display (format "--> Inserted to database A~a from b-file ~a, terms in range n=~a..~a~n"
                                           (Anum->str Anumber) infile first_n prev_n
                                )
                       )
                   )
                   ((zero? (string-length line)) (loop prev_n first-unused-number-for-RGS)) ;; Skip empty lines.
                   ((equal? (string-ref line 0) #\#) (loop prev_n first-unused-number-for-RGS)) ;; Also comment lines.
                   (else
                      (let* ((sis (open-input-string line))
                             (n_from_file (read sis))
                             (an_from_file (read sis))
                            )
                        (if (eof-object? n_from_file)
                            (loop prev_n first-unused-number-for-RGS) ;; Skip lines that contain just white space.
                            (begin
                               (cond ((zero? (modulo n_from_file 32768)) ;; An "I'm still alive!" blurp.
                                        (display
                                           (format "Inserting to database A~a from b-file ~a, now at: a(~a) = ~a~n"
                                               (Anum->str Anumber) infile n_from_file an_from_file
                                           )
                                        )
                                     )
                               )
    
                               (cond ((not vector-for-RGS-conversion)
                                        (query-exec c insert-row Anumber n_from_file an_from_file) ;; Just as it is
                                        (loop n_from_file first-unused-number-for-RGS)
                                     )
                                     (else ;; Otherwise, we are doing an Restricted Growth Sequence conversion on the fly:
                                       (let ((vecind (- n_from_file first_n))) ;; We have zero-based vectors
                                         (cond ((hash-ref rgs-occurrences an_from_file #f)
                                                 =>
                                                  (lambda (prev-position) ;; This value has been encountered before.
                                                    (begin
                                                      (let ((oldval (vector-ref vector-for-RGS-conversion prev-position)))
                                                        (vector-set!
                                                                vector-for-RGS-conversion
                                                                vecind
                                                                oldval
                                                        )
                                                        (query-exec c insert-row Anumber n_from_file oldval)
                                                        (loop n_from_file first-unused-number-for-RGS)
                                                      )
                                                    )
                                                  )
                                               )
                                               (else ;; Otherwise, a new fresh value.
                                                 (begin
                                                    (hash-set! rgs-occurrences
                                                               an_from_file
                                                               vecind
                                                    )
                                                    (vector-set! vector-for-RGS-conversion vecind first-unused-number-for-RGS)
                                                    (query-exec c insert-row Anumber n_from_file first-unused-number-for-RGS)
                                                    (loop n_from_file (+ 1 first-unused-number-for-RGS))
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
          )
        )
  )
)



(define (bfilename-to-Anumber? filename)
       (and (eq? 11  (string-length filename))
            (eq? #\b (string-ref filename 0))
            (eq? #\. (string-ref filename 7))
            (eq? #\t (string-ref filename 8))
            (eq? #\x (string-ref filename 9))
            (eq? #\t (string-ref filename 10))
            (read (open-input-string (substring filename 1 7)))
       )
)

(define (upload-bfiles-from c directory)
  (let ((uploadtime (current-seconds)))
    (current-directory directory)
    (let loop ((files (directory-list)) (n 0))
       (cond ((null? files) (display (format "Uploaded ~a b-files at ~a.~n" n (date->string (seconds->date uploadtime)))))
             ((bfilename-to-Anumber? (some-system-path->string (first files)))
                =>
                (lambda (Anumber)
                    (store-b-file-to-database c Anumber (first files) uploadtime *MAX-ABSVAL-FOR-BIGINTS*)
                    (loop (rest files) (+ 1 n))
                )
             )
             (else (loop (rest files) n))      
       )
    )
  )
)

;; To upload a single file, do something like:
;; (store-b-file-to-database djinn-c 248955 "C:/Users/karttu/A/matikka/OEIS-Djinn/bfiles-for-A101296/b248955.txt" "2017-12-02" *MAX-ABSVAL-FOR-BIGINTS*)

;; To upload all b-files from a certain directory, do something like:
;; (upload-bfiles-from djinn-c "../bfiles-for-A379000")




(upload-bfiles-from djinn-c "./bfiles/")

;; Some information:

(define n-rows (query-list djinn-c "select count(*) from Aseqs"))

(define Anums-num-of-eclasses (query-rows djinn-c "select Anum, count(distinct Value) from Aseqs GROUP BY Anum"))


(define Anumbers (query-rows djinn-c "select distinct Anum from Aseqs"))

(define How-many-Anumbers (query-rows djinn-c "select count(distinct Anum) from Aseqs"))

(define Anumber-and-max-n (query-rows djinn-c "select Anum, max(Nval) from Aseqs GROUP BY Anum"))

(define Aupload-status (query-rows djinn-c "select * from Afileinfo"))


