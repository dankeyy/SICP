;; Chapter 1, Building Abstractions with Procedures


;; Part 1.1 - The Elements of programming

;; ex 1.1
10 ;; 10

(+ 5 3 4) ;; 12

(- 9 1) ;; 8

(/ 6 2) ;; 3

(+ (* 2 4) (- 4 6)) ;; 6

(define a 3) ;; a is 3
(define b (+ a 1)) ;; b is 4
(+ a b (* a b)) ;; 19

( = a b) ;; #f

(if (and (> b a) (< b (* a b)))
  b
  a)
;; 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;; 16

(+ 2 (if (> b a) b a)) ;; 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;; 16


;; ex 1.2
(/ (+ 5
      4
      (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))


;; ex 1.3
(define (square x)
  (* x x))


(define (sum-square-two a b)
  (+ (square a) (square b)))


(define (sum-squares a b c)
    (cond ((and (>= a c) (>= b c)) (sum-square-two a b))
          ((and (>= b a) (>= c a)) (sum-square-two b c))
          (else (sum-square-two a c))))


;; ex 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; if statement will evaulate to either (+ a b) or (- a b),
;; depending on b's sign which's the definition of abs


;; ex 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
  0
  y))

(test 0 (p))
;; (p) infinitly expands in applicative order evaluation
;; on normal order it'd just evaulate to 0


;; finding squares by Newton's method of approximation
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)

;; ex 1.6 - square-iter by Alyssa's new-if implementation
(define (new-if predicate then-clause else-clause)
  (cond (predicate
      then-clause)
  (else
      else-clause)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
    guess
    (new-sqrt-iter (improve guess x) x)))
;; The problem with Alyssa's implementation:
;; As we know the default `if` is a special form;
;; meaning even if the interpreter uses applicative order evaluation,
;; the `if` clause short circuits meaning only one operand will be evaluated.
;; in Alyssa's implementation (if of course the interpreter uses applicative-order evaluation),
;; the program will not stop evaluating the third argument causing the program to infinitly evaluate the third argument.


;; ex 1.7
;; problems with current impl;
;; if numbers are small, tolerence of 0.01 is too big (causing a large margin of error)
;; if numbers are big, machine precision is unable to represent the difference.
;;    The best guess will not be within 0.01 to the root hence the program will not terminate
;; solution??
;; So obviously our implementation of good-enough is not sufficient
;; A very simple solution would be to just keep iterating
;; until the guesses don't change anymore, hence we've arrived at the best solution possible.
(define (good-enough? guess x)
  (= guess (improve guess x)))

;; ex 1.8
(define (improve-cube guess x)
  (/
   (+ (* 2 guess) (/ x (square guess)))
    3))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
    guess
    (cube-root-iter (improve guess x) x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))
