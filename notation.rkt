#lang racket

;;; Notes used are MIDI values.
;;; The note C-1 is defined as 0, with all other notes defined as semitones away from C-1

(require alexander)

(provide
 intervals->semitones
 semitones->intervals
 semitones->notes
 notes->semitones
 (contract-out
  [semitone->interval (natural-number/c . -> . string?)]
  [interval->semitone ((or/c string? natural-number/c) . -> . natural-number/c)]
  [read-note (string? . -> . natural-number/c)]
  [print-note ((natural-number/c) (#:accidental symbol? #:octave boolean?)
               . ->* . string?)]
  [note= (natural-number/c natural-number/c . -> . boolean?)]))

(module+ test
  (require rackunit))


(define intervals->semitones (make-hash '((1 . 0) (2 . 2) (3 . 4) (4 . 5)
                                          (5 . 7) (6 . 9) (7 . 11))))

(define semitones->intervals (make-hash '((0 . 1) (2 . 2) (4 . 3) (5 . 4)
                                          (7 . 5) (9 . 6) (11 . 7))))

(define semitones->notes (make-hash '((0 . #\C) (2 . #\D) (4 . #\E) (5 . #\F)
                                      (7 . #\G) (9 . #\A) (11 . #\B))))

(define notes->semitones (make-hash '((#\C . 0) (#\D . 2) (#\E . 4) (#\F . 5)
                                      (#\G . 7) (#\A . 9) (#\B . 11))))

(define (print-note n #:accidental [accidental 'flat] #:octave [octave #f])
  (define (get-note n)
    (if-let ([c (hash-ref semitones->notes n #f)])
      (string c)
      (if (eq? accidental 'flat)
          (string (hash-ref semitones->notes (add1 n))
                  #\b)
          (string (hash-ref semitones->notes (sub1 n))
                  #\#))))
  (let-values ([(q r) (quotient/remainder n 12)])
    (string-append (get-note r)
                   (if octave
                       (format "~a" (sub1 q))
                       ""))))

(module+ test
  (check-equal? (print-note 60)
                "C")
  (check-equal? (print-note 59 #:octave #t)
                "B3")
  (check-equal? (print-note 0 #:octave #t)
                "C-1")
  (check-equal? (print-note 1)
                "Db")
  (check-equal? (print-note 1 #:accidental 'sharp)
                "C#"))


(define (read-note n)
  (let* ([lst (string->list n)]
         [l (reverse (cons (char-upcase (first lst))
                           (rest lst)))]
         [o (string->number (string (first l)))]
         [a 0])
    (when o
      (set! l (rest l))
      (when (equal? (first l) #\-)
        (set! l (rest l))
        (set! o (- o))))
    (for ([c l])
      #:break (not (or (eq? c #\b)
                       (eq? c #\#)
                       (eq? c #\s)))
      (case c
        [(#\b) (dec! a)]
        [(#\# #\s) (inc! a)]))
    (if o
        (+ (hash-ref notes->semitones (char-upcase (last l)))
           (* (add1 o) 12)
           a)
        (clamp-rotate (+ (hash-ref notes->semitones (char-upcase (last l)))
                         a)
                      0 11))))

(module+ test
  (check-equal? (read-note "C")
                0)
  (check-equal? (read-note "Cb")
                11)
  (check-equal? (read-note "Cbb")
                10)
  (check-equal? (read-note "B")
                11)
  (check-equal? (read-note "B#")
                0)
  (check-equal? (read-note "C0")
                12)
  (check-equal? (read-note "C-1")
                0)
  (check-equal? (read-note "B3")
                59)
  (check-equal? (read-note "Cb4")
                59)
  (check-equal? (read-note "Db")
                1)
  (check-equal? (read-note "Dbb")
                0)
  (check-equal? (read-note "C#")
                1))


(define (semitone->interval s [accidental 'flat])
  (define (get-interval s)
    (if-let ([i (hash-ref semitones->intervals s #f)])
       (values i "")
       (if (eq? accidental 'flat)
           (values (hash-ref semitones->intervals (add1 s))
                   "b")
           (values (hash-ref semitones->intervals (sub1 s))
                   "#"))))
  (if (= s 12) "octave"
      (let*-values ([(q r) (quotient/remainder s 12)]
                    [(i a) (get-interval r)])
        (string-append a (number->roman (+ i (* q 7)))))))

(module+ test
  (check-equal? (semitone->interval 0)
                "I")
  (check-equal? (semitone->interval 1)
                "bII")
  (check-equal? (semitone->interval 2)
                "II")
  (check-equal? (semitone->interval 5)
                "IV")
  (check-equal? (semitone->interval 6 'sharp)
                "#IV")
  (check-equal? (semitone->interval 6)
                "bV")
  (check-equal? (semitone->interval 7)
                "V")
  (check-equal? (semitone->interval 12)
                "octave")
  (check-equal? (semitone->interval 14)
                "IX"))

(define (interval->semitone i)
  (define (interval/string->semitone)
    (let*-values ([(accidental interval)
                   (case (first (string->list i))
                     [(#\b) (values -1 (substring i 1))]
                     [(#\#) (values 1 (substring i 1))]
                     [(#\o) (values 0 "VIII")]
                     [else (values 0 i)])]
                  [(n) (roman->number (string-upcase interval))])
      (let-values ([(q r) (quotient/remainder (sub1 n) 7)])
        (+ (hash-ref intervals->semitones (add1 r))
           (* q 12)
           accidental))))
  (define (interval/number->semitone)
    (let-values ([(q r) (quotient/remainder (sub1 i) 7)])
      (+ (hash-ref intervals->semitones (add1 r))
         (* q 12))))
  (if (number? i)
      (interval/number->semitone)
      (interval/string->semitone)))

(module+ test
  (check-equal? (interval->semitone "I")
                0)
  (check-equal? (interval->semitone "bII")
                1)
  (check-equal? (interval->semitone "II")
                2)
  (check-equal? (interval->semitone "IV")
                5)
  (check-equal? (interval->semitone "#IV")
                6)
  (check-equal? (interval->semitone "bV")
                6)
  (check-equal? (interval->semitone "V")
                7)
  (check-equal? (interval->semitone "octave")
                12)
  (check-equal? (interval->semitone "IX")
                14)
  (check-equal? (interval->semitone 3)
                4)
  (check-equal? (interval->semitone 9)
                14)
  (check-equal? (interval->semitone 8)
                12)
  (check-equal? (interval->semitone 1)
                0))

(define (note= n1 n2)
  (= (remainder n1 12) (remainder n2 12)))

(module+ test
  (check-true (note= (read-note "C") (read-note "C4")))
  (check-true (note= (read-note "Db5") (read-note "Db2")))
  (check-true (note= (read-note "G#") (read-note "G#4"))))

(define roman-table
  (hash #\M 1000
        #\D 500
        #\C 100
        #\L 50
        #\X 10
        #\V 5
        #\I 1))

(define (number->roman n)
  (cond
   [(>= n 1000) (string-append "M" (number->roman (- n 1000)))]
   [(>= n 900) (string-append "CM" (number->roman (- n 900)))]
   [(>= n 500) (string-append "D" (number->roman (- n 500)))]
   [(>= n 400) (string-append "CD" (number->roman (- n 400)))]
   [(>= n 100) (string-append "C" (number->roman (- n 100)))]
   [(>= n 90) (string-append "XC" (number->roman (- n 90)))]
   [(>= n 50) (string-append "L" (number->roman (- n 50)))]
   [(>= n 40) (string-append "XL" (number->roman (- n 40)))]
   [(>= n 10) (string-append "X" (number->roman (- n 10)))]
   [(>= n 9) (string-append "IX" (number->roman (- n 9)))]
   [(>= n 5) (string-append "V" (number->roman (- n 5)))]
   [(>= n 4) (string-append "IV" (number->roman (- n 4)))]
   [(>= n 1) (string-append "I" (number->roman (- n 1)))]
   [else ""]))

(define (roman->number r)
  (define (get-value letter)
    (hash-ref roman-table letter))
  (define lst (map get-value (string->list r)))
  (+ (last lst)
     (for/fold ((sum 0))
         ([i (in-list lst)]
          [i+1 (in-list (cdr lst))])
       (+ sum
          (if (> i+1 i)
              (- i)
              i)))))

(module+ test
  (check-equal? (number->roman 1994)
                "MCMXCIV")
  (check-equal? (roman->number "MCMXCIV")
                1994))
