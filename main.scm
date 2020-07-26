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


