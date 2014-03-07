#lang racket

(require alexander
         "notation.rkt")


(define (get-note)
  (with-handlers
      ([exn:fail? (lambda (ex)
                    (displayln "Not a valid note. Try again: ")
                    (get-note))])
    (read-note (read-line))))

(define (interval-tester [intervals '(3 5 7 9)] #:descending [descending? #f])
  (let* ([root (+ (random 12) 60)]
         [interval (interval->semitone (random-elt intervals))]
         [descending? (if descending?
                         (chance 1 2)
                         #f)]
         [note ((if descending? - +)
                root interval)])
    (printf (if descending?
                "~a down from ~a: " 
                "~a of ~a: ")
            (semitone->interval interval) (print-note root))
    (let ([anwser (get-note)])
      (if (note= anwser note)
          (begin
            (displayln "Correct!")
            #t)
          (begin
            (printf "Wrong: ~a\n" (print-note note))
            #f)))))

(define (tester thunk repeat)
  (let-values ([(correct _ time ^)
                (time-apply (lambda ()
                              (for/sum ([_ (range repeat)])
                                (if (thunk) 1 0)))
                            '())])
    (printf "~a/~a correct\nTook ~a seconds\n"
            (first correct) repeat
            (exact->inexact (/ (exact-round (/ time 100))
                               10)))))



(define descending (make-parameter #f))
(define intervals (make-parameter '(3 5 7 9)))
(define repeat (make-parameter 10))

(with-handlers
    ([exn:fail? (lambda (ex)
                  (prn (exn-message ex))
                  (void))])
  (command-line
   #:program "Music Theory Tester"
   #:once-each
   [("-d" "--descending") "Include descending intervals"
    (descending #t)]
   [("-i" "--intervals") i "Intervals to quiz on e.g: \"3 bIV\" (default '3 5 7 9')"
    (intervals (for/list ([s (string-split i)])
                 (if-let ([n (string->number s)]) 
                         n
                         (with-handlers
                             ([exn:fail? (lambda (ex)
                                           (error (format "Not a valid interval: ~a"
                                                          s)))])
                           (interval->semitone s)
                           s))))]
   [("-r" "--repeat") r (format "Number of times to quiz (default ~a)" (repeat))
    (repeat (if-let ([n (string->number r)]) 
                    n
                    (error "Repeat must be a number.")))]
   #:args ()
   (tester (lambda () (interval-tester (intervals) #:descending (descending)))
           (repeat))
   (void)))
