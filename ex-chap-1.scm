;;; 1.1
10

;;; 1.2

;;; 1.3
;; 3引数のうち大きい2つの2乗の和を求めよ
(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (ret-sum-of-square x y z)
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
        ((and (< y x) (< y z)) (sum-of-squares x z))
        ((and (< z x) (< z y)) (sum-of-squares x y))))
