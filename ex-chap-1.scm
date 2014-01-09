;;; 1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))


;;; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))


;;; 1.3
;; 3引数のうち大きい2つの2乗の和を求めよ
(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (ret-sum-of-square x y z)
  (cond ((and (<= x y) (<= x z)) (sum-of-squares y z))
        ((and (<= y x) (<= y z)) (sum-of-squares x z))
        ((and (<= z x) (<= z y)) (sum-of-squares x y))))

(ret-sum-of-square 1 2 3)
(ret-sum-of-square 2 2 3)
(ret-sum-of-square 2 3 2)
(ret-sum-of-square 3 2 2)
(ret-sum-of-square 2 2 2)
(ret-sum-of-square -1 -2 -3)
(ret-sum-of-square 0 0 0)


;;; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 1 2)
(a-plus-abs-b 1 -2)
(a-plus-abs-b -1 -2)


;;; 1.5
"作用的順序ではpは無限自己参照なので、無限ループになる。正規順序の場合、先にifが評価されるため、pが評価されず0を返す"

;; (define (p) (p))
;; (define (test x y)
;;   (if (= x 0)
;;       0
;;       y))

;; (test 0 (p))


;;; 1.6
"作用的順序の評価のnew-ifでは、good-enough?が真となっても、再帰の第三引数が必ず評価されてしまうため、無限ループになる"

;; 特殊形式ではない"new-if"を定義する
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter2 (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt2 x)
  (sqrt-iter2 1.0 x))


;;; 1.7
;;　再起の終了条件の評価方法を変更し、精度を上げた
(define (sqrt x)
  (sqrt-iter 1.0 10.0 x))

(define (sqrt-iter guess previousGuess x)
  (if (good-enough? guess previousGuess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (good-enough? guess previousGuess)
  (< (abs (- 1 (/ guess previousGuess))) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(print (sqrt 0.0004))
(print (sqrt 10000000000000))


;;; 1.8
;; 立方根を求める
(define (crt x)
  (crt-iter 1.0 10.0 x))

(define (crt-iter guess previousGuess x)
  (if (good-enough? guess previousGuess)
      guess
      (crt-iter (improve2 guess x) guess x)))

(define (improve2 guess x)
  (/ (+ (/ x (* guess guess))
        (* 2.0 guess))
     3.0))

(crt 10)
(crt 27)



;;; 1.34
(define (f g)
  (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))

(f f)
