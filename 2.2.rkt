#lang racket
(define (build-vector split-line)
  (cons
   (car split-line)
   (string->number (cadr split-line))))

(define vectors
       (map build-vector
            (map string-split
                 (file->lines "2.input"))))

(struct coordinates (x y aim))

(define (compute-depths c vector)
  (cond
    [(equal? (car vector) "up")
     (struct-copy coordinates c
                  [aim (- (coordinates-aim c) (cdr vector))])]
    
    [(equal? (car vector) "down")
     (struct-copy coordinates c
                  [aim (+ (coordinates-aim c) (cdr vector))])]
    
    [(equal? (car vector) "forward")
     (struct-copy coordinates c
                  [x (+ (coordinates-x c) (* (coordinates-aim c) (cdr vector)))]
                  [y (+ (coordinates-y c) (cdr vector))])]
    ))


(define final-coordinates
  (for/fold
    ([c (coordinates 0 0 0)])
    ([v vectors])
  (values
   (begin
     (compute-depths c v)))))

"x:" (coordinates-x final-coordinates)
"y:" (coordinates-y final-coordinates)
"solution:" (* (coordinates-x final-coordinates) (coordinates-y final-coordinates))
