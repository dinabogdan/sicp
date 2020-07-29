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

;; exponentiation

;; recursive implementation

(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

;; iterative / tail-recursive implementation

(define (tailrec-expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1) (* b product))))

;; logarithmically growth implementation

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (mod n 2) 0))

;; exercise 1.16

(define (iter-fast-expt b n)
  (define (iter n b acc)
    (cond ((= 0 n) acc)
          ((even? n) (iter (/ n 2) (square b) acc))
          (else (iter (- n 1) b (* b acc)))))
  (iter n b 1))

;; GCD algorithm / Euclid's Algorithm

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (mod a b))))

;; searching for divisors of n
;; one way to find if n is prime or not

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (mod b a) 0))

;; n is prime iff n is its own smallest divisor

(define (prime? n) (= n (optimized-smallest-divisor n)))

;; fermat test for finding if a number is prime


;; this one computes the exponential of a number modulo another number
;; in fact computes (mod base^exp m)
;; (expmod 2 8 2) = 0, (expmod 2 8 3) = 1

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (mod 
            (square (expmod base (/ exp 2) m)) m))
        (else 
          (mod 
            (* base (expmod base (- exp 1) m)) m))))

;; this is fermat test for checking if a number is prime using Fermat's theorem

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer n))))


;; This function is a hack of simulating runtime primitive
;; The scheme interpreter that I'm currently using doesn't have runtime primitive

(define (date2runtime date)
  ; HACK
  ; wraps around occasionally!
  (+  
     (* (date-hour date) 60 60 1000) 
     (* (date-minute date) 60 1000) 
     (* (date-second date) 1000) 
     (date-millisecond date)
  )
)

(define (runtime) (date2runtime (current-date)))


;; exercise 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((prime? n) (report-prime (- (runtime) start-time)))
      (else (display " not prime"))))
      
(define (report-prime n elapsed-time)
  (newline)
  (display " *** ")
  (display n)
  (display elapsed-time))

(define (search-for-primes startIndex endIndex)
  (define (iter n)
    (cond ((<= n endIndex) 
            (timed-prime-test n)
            (iter (+ n 2)))))
  (iter (if (odd? startIndex) startIndex (+ startIndex 1))))

;; exercise 1.23

(define (next input)
  (if (= input 2) 3 (+ input 2)))

(define (optimized-smallest-divisor n) (optimized-find-divisor n 2))

(define (optimized-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;; 1.3.1 Higher Order Procedures - Procedures as Arguments

(define (cube a)
  (* a a a))

(define (sum-of-integers a b)
  (if (> a b)
      0
      (+ a (sum-of-integers (+ a 1) b))))

(define (sum-of-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-of-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
          (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (ho-sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define (ho-sum-of-integers a b)
  (sum identity a inc b))

(define (ho-pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;; procedure for computing the integral value of a function f between the limits a and b

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

;; usage
;; (integral cube 0 1 0.01)
;; (integral cube 0 1 0.001)

