#lang racket
(define (build-vector split-line)
  (cons
   (car split-line)
   (string->number (cadr split-line))))

(define vectors
       (map build-vector
            (map string-split
                 (file->lines "2.input"))))

(define (compute-depths vector)
  (cond
    [(equal? (car vector)   "up") (-(cdr vector))]
    [(equal? (car vector) "down")   (cdr vector) ]
    [else 0]))

(define (compute-horizontals vector)
  (if (equal? (car vector) "forward") (cdr vector) 0))

(define puzzle-1-coordinates (list
  (apply + (map compute-depths vectors))
  (apply + (map compute-horizontals vectors))))

"Puzzle 1 Coordinates:" puzzle-1-coordinates
"Puzzle 1 Solution:" (apply * puzzle-1-coordinates)