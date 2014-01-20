;;; 1.1.7 Newton法による平方根
(defn average [x y]
  (/ (+ x y) 2))

(defn improve
  "予測値は、それ自身と、被開平数とそれ自身の商の、平均として改善される"
  [guess x]
  (average guess (/ x guess)))

(defn abs [x]
  (if (< x 0) (- x) x))

(defn square [x]
  (* x x))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))


;;; 問題1.7（改良型平方根）
(defn good-enough-imp?
  "前回の予測値と今回の予測値の変化量に注目する"
  [guess previous-guess]
  (< (abs (- 1 (/ guess previous-guess))) 0.001))

(defn sqrt-iter-imp [guess previous-guess x]
  (if (good-enough-imp? guess previous-guess)
    guess
    (sqrt-iter-imp (improve guess x) guess x)))

(defn sqrt-imp [x]
  (sqrt-iter-imp 1.0 10.0 x))

(sqrt-imp 2)
(square (sqrt-imp 2))
(square (sqrt 2))
(sqrt-imp 0.0004)
(sqrt-imp 10000000000000)
(square (sqrt-imp 10000000000000))


;;; 問題1.8（立方根）
(defn improve-crt [guess x]
  (/ (+ (/ x (square guess)) (* 2.0 guess)) 3))

(defn crt-itr [guess previous-guess x]
  (if (good-enough-imp? guess previous-guess)
    guess
    (crt-itr (improve-crt guess x) guess x)))

(defn crt [x]
  (crt-itr 1.0 10.0 x ))

(defn cube [x]
  (* x x x))
(crt 10)
(cube (crt 10))
(crt 27)
(cube (crt 27))


;;; 1.1.8
