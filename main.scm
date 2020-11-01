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

;; exercise 1.29
;; Simpson's Rule of calculating integral of a function f

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (simpsons-term k)
    (define y (f (+ a (* k h))))
    (if (or (= k 0) (= k n)) y
        (if (even? k) (* 2 y) (* 4 y))))
  (* (/ h 3) (sum simpsons-term 0 inc n)))

;; exercise 1.30
;; this is an iterative/tail recursive implementation of sum function

(define (iter-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; exercise 1.31 a)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (new-factorial n)
  (product identity 1 inc n))

(define (pi-aproximation-next-term n)
  (if (even? n)
    (/ (+ n 2) (+ n 1))
    (/ (+ n 1) (+ n 2))))

(define (pi-aproximation n)
  (* (product pi-aproximation-next-term 1 inc n) 4))

(define (iter-product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

  ;; exercise 1.32 - abstract even more through accumulate function


;; recursive implementation

(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner 
      (term a) 
      (accumulate combiner null-value term (next a) next b))))

;; iterative implementation

(define  (iter-accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; exercise 1.33
;; filtered-accumulate

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
    null-value
    (if (filter a)
      (combiner
        (term a) 
        (filtered-accumulate combiner null-value term (next a) next b filter))
      (combiner 
        null-value
        (filtered-accumulate combiner null-value term (next a) next b filter)))))

;; The sum of squares of all prime numbers in a given interval a b

(define (sum-of-primes-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))

;; refactoring above defined procedures such as pi-sum or pi-term for using lambdas

(define (new-pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
        a
        (lambda (x) (+ x 4))
        b))

;; Compute f(x, y) = x(1+xy)^2 + y(1-y) + (1+xy)(1-y)
;; use the following expressions:
;;      a = 1 + xy
;;      b = 1 - y
;;     f(x,y) = xa^2 + yb + ab

;; define an auxiliary procedure to bind the local variables
;; this solution is called f1

(define (f1 x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

;; using lambda expression to specify an anonymous procedure for binding local variables
;; this solution is called f2

(define (f2 x y)
  ((lambda (a b)
      (+ (* x (square a))
         (* y b)
         (* a b))
    ) (+ 1 (* x y)) (- 1 y)))

;; using the special form let to define and specify the local variables
;; this solution is called f3

(define (f3 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; exercise 1.34

(define (f g) (g 2))

;; if we will try to evaluate (f f) this will lead to the error: "2 is not a function"
;; first invocation of f will attempt to apply its argument (which is f) to 2
;; the second invocation of f will attempt to apply its argument (which is 2) to 2 and this leads to error


;; finding roots of equations by the half-interval Method

(define (close-enough? x y) 
  (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value) (search f neg-point midpoint))
              ((negative? test-value) (search f midpoint pos-point))
              (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
       (b-value (f b)))
       
    (cond ((and (negative? a-value) (positive? b-value))
            (search f a b))
          ((and (negative? b-value) (positive? a-value))
            (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

;; finding fixed points of a function

(define error-tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) error-tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
            next
            (try next))))
  (try first-guess))

;; exercise 1.35

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;; exercise 1.36

(define (x-to-the-x y)
  (fixed-point (lambda (x) (/ (log y) (log x))) 10.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (new-sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (average-damped-new-sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

;; derivative of a function

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

;; express Newton's method as a fixed-point process

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; new implementation of sqrt function by using the newtons-method

(define (sqrt x)
  (newtons-method
    (lambda (y) (- (square y) x)) 1.0))

;; exercise 1.40

(define (add x)
  (+ x 1))

(define (cubic a b c)
  (lambda (x) (+ (cube x) 
                 (* a (square x))
                 (* b x)
                 c)))

(define (solve-eq a b c)
  (newtons-method (cubic a b c) 1))

;; exercise 1.41

(define (double f)
  (lambda (x) (f (f x))))

;; exercise 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

;; exercise 1.43

(define (repeated f times)
  (if (> times 0)
      (compose f (repeated f (- times 1)))
      (lambda (x) x)))


;;;;; chapter 2

;; 2.1

;; make-rat is constructor
;; numer and denom are selectors


;;(define (make-rat n d) 
;;  (let ((g (gcd n d)))
;;    (cons (/ n g) (/ d g))))

;; exercise 2.1
(define (make-rat n d) 
  (define (sign x) (if (< x 0) - +)) 
  (let ((g (gcd n d))) 
    (cons ((sign d) (/ n g)) 
              (abs (/ d g))))) 

(define (numer x) (car x))
(define (denom x) (cdr x))

;; add two rational numbers
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

;; subtract two rational numbers
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)
                  (numer y) (denom x)))
            (* (denom x) (denom y))))


;; multiply two rational numbers
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

;; divide two rational numbers
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

;; check if two rational numbers are equal
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; print rational numbers

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

;; exercise 2.2

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment starting-point ending-point) (cons starting-point ending-point))
(define (start-segment segment) (print-point (car segment)))
(define (end-segment segment) (print-point (cdr segment)))

;; defining cons 

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)
(define (car z) (z 0))
(define (cdr z) (z 1))


;; exercise 2.5

(define (exp base n)
  (define (iter x result)
    (if (= 0 x)
        result
        (iter (- x 1) (* base result))))
  (iter n 1))

(define (count-0-remainder-divisions n divisor)
  (define (iter try-exp)
    (if (= 0 (mod n (exp divisor try-exp)))
    (iter (+ try-exp 1))
    (- try-exp 1)))
  (iter 1))

(define (special-cons a b) 
  (* (exp 2 a)
     (exp 3 b)))

(define (special-car z)
  (count-0-remainder-divisions z 2))
(define (special-cdr z)
  (count-0-remainder-divisions z 3))

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))