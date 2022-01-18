
(/ (+ 5
      4
      (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))


(define (square x)
  (* x x))


(define (sum-square-two a b)
  (+ (square a) (square b)))


(define (sum-squares a b c)
    (cond ((and (>= a c) (>= b c)) (sum-square-two a b))
          ((and (>= b a) (>= c a)) (sum-square-two b c))
          (else (sum-square-two a c))))
