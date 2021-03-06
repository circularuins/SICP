(define (square x)
  (* x x))


;;; 1.7
;; (define (sqrt x)
;;   (the y (and (>= y 0)
;;               (= (square y) x))))

;; Newton法（特別的）による平方根を求める手続き
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 2)
(sqrt 3)
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
