(ns chapters.1
  (:require [clojure.tools.trace :as trace]))

;; 1.1

(comment
  10
  (+ 5 3 4)
  (- 9 1)
  (/ 6 2)
  (+ (* 2 4) (- 4 6))
  (def a 3)
  (def b (+ a 1))
  (= a b)
  (if (and (> b a) (< b (* a b)))
    b
    a)
  (cond
    (= a 4) 6
    (= b 4) (+ 6 7 a)
    :else 25)
  (+ 2 (if (> b a) b a))
  (* (cond (> a b) a
           (< a b) b
           :else -1)
     (+ a 1))
  )

;; 1.2

(comment
  (/ (+ 5 3 (- 2 (- 3 (+ 6 4/5))))
     (* 3 (- 6 2) (- 2 7)))
  )

;; 1.3

(defn square [x] (* x x))

(defn sum-greatest-squares [a b c]
  (reduce + (map square (drop 1 (sort [a b c])))))

(comment
  (sum-greatest-squares 1 2 3))

;; 1.4

; the function as defined
(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))
; will add them if b is positive and subtract if b is negative, making it equivalent to
(defn a-plus-abs-b* [a b]
  (+ a (abs b)))

(comment
  (= (a-plus-abs-b 12 41)
     (a-plus-abs-b* 12 41))
  (= (a-plus-abs-b 12 -41)
     (a-plus-abs-b* 12 -41))
  )

;; 1.5

(defn p [] (p))

(defn test* [x y]
  (if (= x 0)
    0
    y))

(comment
  ; this will cause a stack overflow in clojure, because it's "applicative-order"
  (test* 0 (p))

  ; however, evaluating the `if` directly with the variables inlined works fine
  (if (= 0 0)
    0
    (p))
  ; because `if` is a special form with it's own evaluation order (predicate first, then only the corresponding case)
  )


;; 1.6

; using `new-if` would evaluate all arguments first, before running the `cond` to select which one to return
; this will cause endless recursive calls to `sqrt-iter`, causing a stack overflow


; code from book
(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

(comment
  (sqrt 9)
  (sqrt (+ 100 37))
  (sqrt (+ (sqrt 2) (sqrt 3)))
  (square (sqrt 1000))

  )

; but since clojure has macros, it can emulate if evaluation semantics
; and actually clojure's `cond` does evaluate only th e

; new-if example
(defn new-if [predicate then-clause else-clause]
  (cond predicate then-clause
        :else else-clause))

(defn new-sqrt-iter [guess x]
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(defn new-sqrt [x]
  (new-sqrt-iter 1.0 x))

(comment
  ; works fine
  (new-sqrt 9)
  )

;; 1.7

(defn good-enough?* [guess x]
  (prn (- guess (improve guess x)))
  (< (abs (- guess (improve guess x))) 0.001))

(defn sqrt-iter* [guess x]
  (if (good-enough?* guess x)
    guess
    (sqrt-iter* (improve guess x)
                x)))

(defn sqrt* [x]
  (sqrt-iter* 1.0 x))

(comment
  (sqrt* 9)
  (sqrt* 1000)
  )

; 1.8

(defn approx-cube-root [guess x]
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

(defn good-enough?-3 [guess x]
  (prn (- guess (approx-cube-root guess x)))
  (< (abs (- guess (approx-cube-root guess x))) 0.001))

(defn cube-root-iter [guess x]
  (if (good-enough?-3 guess x)
    guess
    (cube-root-iter (approx-cube-root guess x)
                    x)))

(defn cube-root [x]
  (cube-root-iter 1.0 x))

(comment
  (cube-root 8)
  (cube-root 27)
  )

;; 1.9

; the first is recursive and the second is iterative
; because of the tail-position of the recursive call in the second

(defn +-rec [a b]
  (if (= a 0)
    b
    (inc (+-rec (dec a) b))))

(defn +-iter [a b]
  (if (= a 0)
    b
    (+-iter (dec a) (inc b))))


(comment
  (trace/trace-vars #'+-rec #'+-iter)
  (+-rec 3 4)
  ; this becomes
  ; (inc (+-rec (dec 3) 4))
  ; (inc (inc (+-rec (dec 2) 4)))
  ; (inc (inc (inc (+-rec (dec 1) 4))))
  ; (inc (inc (inc 4)))
  ; (inc (inc 5))
  ; (inc 6)
  ; 7

  (+-iter 3 4)
  ; this becomes
  ; (+-iter (dec 3) (inc 4))
  ; (+-iter (dec 2) (inc 5))
  ; (+-iter (dec 1) (inc 6))
  ; 7
  )

;; 1.10

(defn A [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (A (- x 1)
                 (A x (- y 1)))))

(comment
  (A 1 10)
  (A 2 4)
  (A 3 3)
  (A 4 2)
  )

(defn f [n] (A 0 n))

(comment
  ; f(n) = 2n
  (f 4)
  (f 100)
  )

(defn g [n] (A 1 n))
(comment
  ; g(n) = 2^n
  (g 1)
  (g 2)
  (g 3)
  )

(defn h [n] (A 2 n))
(comment
  (trace/trace-vars #'A)
  ; h(n) = 2^h(n-1)
  (h 1)
  (h 2)
  (h 3)
  (h 4)
  )

;; 1.11

(defn f-rec [n]
  (if (< n 3)
    n
    (+ (f-rec (- n 1))
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3))))))

(defn f-iter [n]
  (loop [a 0
         b 1
         c 2
         count n]
    (if (= 0 count)
      a
      (recur b c (+ c (* 2 b) (* 3 a)) (dec count)))))

(comment
  (time (f-rec 35))
  (time (f-iter 35))
  )

;; 1.12

(defn pascals-triangle [height]
  (if (= height 1)
    [[1]]
    (let [triangle (pascals-triangle (- height 1))
          prev (last triangle)]
      (concat triangle
              [(mapv + (concat [0] prev) (concat prev [0]))]))))

(defn pascals-triangle-iter [height]
  (loop [triangle []
         height   height]
    (if (= height 0)
      triangle
      (let [prev (last triangle)]
        (recur (concat triangle
                       [(mapv + (concat [0] prev) (concat prev [0]))])
               (dec height))))))

(comment
  (time (pascals-triangle 60))
  (time (pascals-triangle-iter 60))

  )


;; 1.13
;; inductive proof bla bla (https://github.com/jlollis/sicp-solutions/blob/master/Chapter%201%20Exercises/1.13.scm)
