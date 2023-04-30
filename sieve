;;; Filter:
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;;; Range:
(define (range x)
  (define (countdown x)
    (if (= x 1)
        '(1)
        (cons x (countdown (- x 1)))))
  (reverse (countdown x)))

;;; Sieve of Eratosthenes:
(define (eratosthenes x)
  (define sequence (cdr (range x)))
  (define limit (inexact->exact (floor (sqrt (car (reverse (cdr (range x))))))))
  (define (remove-multiples integer sequence)
    (cons integer (filter (lambda (x) (not (= 0 (remainder x integer)))) sequence)))
  (define (sieve limit table sequence)
    (if (= limit 1)
      sequence
      (sieve (- limit 1) (cdr table) (remove-multiples limit sequence))))
  (cons 1 (sieve limit (reverse (range limit)) sequence)))
