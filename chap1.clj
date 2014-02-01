;;; 1.1.7 Newton法による平方根
(defn average [x y]
  (/ (+ x y) 2))

(defn improve
  "予測値は、それ自身と、被開平数とそれ自身の商の、平均として改善される"
  [guess x]
  (average guess (/ x guess)))

(defn square [x]
  (* x x))

(defn good-enough? [guess x]
  (< (Math/abs (- (square guess) x)) 0.001))

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

;; 再帰ではなく遅延シーケンスを使ってみた
(defn sqrt2 [x]
  (defn good-enough? [guess]
    (< (Math/abs (- (square guess) x)) 0.001))

  (defn improve [guess]
    (average guess (/ x guess)))

  (defn sqrt-seq
    "被開閉数xの平方根候補の無限遅延リストを返す"
    [guess]
    (iterate (partial improve) guess))

  (defn sqrt-filt
    "遅延リストにgood-enough?フィルターを適応する"
    [guess]
    (first (take 1 (drop-while (complement (partial good-enough?)) (sqrt-seq guess)))))

  (sqrt-filt 1.0))

(sqrt2 2)
(sqrt 2)
(sqrt2 9)
(sqrt 9)


;;; 問題1.7（改良型平方根）
(defn good-enough-imp?
  "前回の予測値と今回の予測値の変化量に注目する"
  [guess previous-guess]
  (< (Math/abs (- 1 (/ guess previous-guess))) 0.001))

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


;;; 1.2.1 線形再帰と反復
;; 再帰的プロセス
(defn factorial [n]
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

;; 反復的プロセス
(defn factorial [n]
  (fact-iter 1 1 n))

(defn fact-iter [product counter max-count]
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))


;;; 問題1.10
;; Ackermann関数
(defn A [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (A (- x 1)
                 (A x (- y 1)))))


;;; 1.2.2 木構造再帰
;; 木構造再帰的なFibonacci
(defn fib-tree [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib-tree (- n 1))
                 (fib-tree (- n 2)))))

;; 反復的なFibonacci
(defn fib [n]
  (fib-iter 1 0 n))

(defn fib-iter [a b count]
  (if (= count 0)
    b
    (fib-iter (+ a b) a (dec count))))
