;;; 1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)

(def a 3)
(def b (+ a 1))
(= a b)
(if (and (> b a) (< b (* a b)))
  b
  a)
(cond (= a 4) 6
      (= b 4) (+ 6 7 a)
      :else 25)
(+ 2 (if (> b a) b a))
(* (cond (> a b) a
         (< a b) b
         :else -1)
   (+ a 1))


;;; 1.3
(defn square [x]
  (* x x))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))

(defn ret-sum-of-square [x y z]
  (cond (and (<= x y) (<= x z)) (sum-of-squares y z)
        (and (<= y x) (<= y z)) (sum-of-squares x z)
        (and (<= z x) (<= z y)) (sum-of-squares x y)))

(ret-sum-of-square 1 2 3)
(ret-sum-of-square 2 2 3)
(ret-sum-of-square 2 3 2)
(ret-sum-of-square 3 2 2)
(ret-sum-of-square -1 -2 -3)
(ret-sum-of-square 0 0 0)
