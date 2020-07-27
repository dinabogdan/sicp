;; pretty - printing

(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
    (+ (- 10 7)
      6))

;; define variables

(define size 2)
(define pi 3.14159)
(define radius 10)
(define circumference (* 2 pi radius))

;; procedure definitions

(define (square x) (* x x))

;; compound procedure
(define (sum-of-squares x y) (+ (square x) (square y)))

;; conditional expressions

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))
  )
)

;; logical operations

(define (>= x y)  (or (> x y) (= x y)))

;; Exercise 1.1

10
(+ 5 3 4) ;; 12
(- 9 1) ;; 8
(/ 6 2) ;; 3
(+ (* 2 4) (- 4 6)) ;; 6

(define a 3) ;; nothing printed
(define b (+ a 1)) ;; nothing printed

(+ a b (* a b)) ;; 19
(= a b) ;; #f

(if (and (> b a) (< b (* a b)))
  b
  a
) ;; 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)
) ;; 16

(+ 2 (if (> b a) b a)) ;; 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
    (+ a 1)) ;; 16

;; Exercise 1.2

(/ (+ 5 4 
        (- 2 
            (- 3
                (+ 6 (/ 4 5)))))
    (* 3 (- 6 2)(- 2 7))
)

;; Exercise 1.3
;; it is incomplete because it not takes into consideration the values that are equal
;; instead of using > we should use >=

(define (sum-of-squares-of-larger-numbers x y z)
  (cond ((and (> x y) (> x z))
          (if (> y z) (sum-of-squares x y) (sum-of-squares x z)))
        ((and (> y x) (> y z))
          (if (> x z) (sum-of-squares y x)
          (sum-of-squares y z)))
        ((and (> z x) (> z y))
          (if (> x y) (sum-of-squares z x)
          (sum-of-squares z y)))
  )
)

;; Exercise 1.4

(define (a-plus-abs-b a b) 
  ((if (> b 0) + -) a b))

;; Exercise 1.5

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;; Square Roots by Newton's Method

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)

  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (sqrt-iter guess)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

  (sqrt-iter 1.0 x))

;; recursive factorial definition

;;(define (factorial n)
;;  (if (= n  1)
;;      1
;;      (* n (factorial (- n 1)))))

;; linear iterative implementation of factorial

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product) (+ counter 1) max-count)))

;; Ackermann's function

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

;; recursive and inefficient implementation of Fibonacci
;; this implementation uses tree recursion

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; an iterative and more efficient than the tree-recursive one implementation of Fibonacci

(define (fib2 n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

;; Counting change

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount        (first-denomination kinds-of-coins))
                 kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; Exercise 1.11

;; Recursive implementation

(define (rec-f n)
  (cond ((< n 3) n)
        (else (+ 
                (rec-f (- n 1))
                (* 2 (rec-f (- n 2)))
                (* 3 (rec-f (- n 3)))))))

;; iterative / tailrec implementation

(define (iter-f n) 
  (define (f a b c count) 
    (cond ((< n 3) n) 
          ((<= count 0) a) 
          (else (f (+ a (* 2 b) (* 3 c)) a b (- count 1))))) 
  (f 2 1 0 (- n 2))) 

;; Exercise 1.12 - Pascal Triangle

(define (pascal row column)
  (if (or (= column 1) (= row column))
      1
      (+ (pascal (- row 1) (- column 1))
         (pascal (- row 1) column))))


(define (pascal-triangle row column)
  (cond ((> column row) 0)
        ((< column 0) 0)
        ((= column 1) 1)
        ((+ (pascal-triangle (- row 1) (- column 1))
            (pascal-triangle (- row 1) column)))))