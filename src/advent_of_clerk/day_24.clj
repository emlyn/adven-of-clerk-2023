;; # 🎄 Advent of Clerk: Day 24
(ns advent-of-clerk.day-24
  (:require [nextjournal.clerk]
            [clojure.string :as str]))

(def example "19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3")

(defn parse-input
  [s]
  (->> s
       str/split-lines
       (map #(str/split % #" *@ *"))
       (map (fn [[p v]]
              [(mapv parse-long (str/split p #", *")) (mapv parse-long (str/split v #", *"))]))))

(parse-input example)

;; x1 + t1 * vx1 = x2 + t2 * vx2 (1)
;; y1 + t1 * vy1 = y2 + t2 * vy2 (2)
;; x1*vy2 + t1*vx1*vy2 = x2*vy2 + t2*vx2*vy2 [(1) * vy2] (3)
;; y1*vx2 + t1*vy1*vx2 = y2*vx2 + t2*vy2*vx2 [(2) * vx2] (4)
;; x1*vy2 + t1*vx1*vy2 - y1*vx2 - t1*vy1*vx2 = x2*vy2 - y2*vx2 [(3) - (4)] (5)
;; t1*vx1*vy2 - t1*vy1*vx2 = x2*vy2 - y2*vx2 - x1*vy2 + y1*vx2
;; t1 = (x2*vy2 - y2*vx2 - x1*vy2 + y1*vx2) / (vx1*vy2 - vy1*vx2)
;; t1 = (vy2 * (x2 - x1) + vx2 * (y1 - y2)) / (vx1*vy2 - vy1*vx2)
;; x1 + t1 * vx1 = x2 + t2 * vx2 =>
;; t2 = (x1 + t1 * vx1 - x2) / vx2

(defn intercept
  "Returns the intercept point of two lines if one exists,
   and it is in the future for moth lines."
  [[[x1 y1 _] [vx1 vy1 _]] [[x2 y2 _] [vx2 vy2 _]]]
  (let [num1 (+ (* vy2 (- x2 x1))
                (* vx2 (- y1 y2)))
        denom1 (- (* vx1 vy2) (* vy1 vx2))
        t1 (when-not (zero? denom1) (/ num1 denom1))
        t2 (when (and t1 (not (zero? vx2)))
             (/ (+ x1 (* t1 vx1) (- x2))
                vx2))]
    (when (and t1 t2 (pos? t1) (pos? t2))
      [(double (+ x1 (* t1 vx1)))
       (double (+ y1 (* t1 vy1)))])))

(intercept [[19 13 30] [-2 1 -2]] [[18 19 22] [-1 -1 -2]])

(defn part1
  "Solve part 1."
  [s & [lo hi]]
  (let [lo (or lo 200000000000000)
        hi (or hi 400000000000000)
        hail (vec (parse-input s))]
    (count
     (for [a (range (dec (count hail)))
           b (range (inc a) (count hail))
           :let [p (intercept (hail a) (hail b))]
           :when (and p
                      (<= lo (first p) hi)
                      (<= lo (last p) hi))]
       p))))

(part1 example 7 27)

#_(part1 (slurp "input_24.txt"))

(comment
  ;; Use z3 to solve part 2, here: https://jfmc.github.io/z3-play/
  (declare-const x Int)
  (declare-const y Int)
  (declare-const z Int)
  (declare-const vx Int)
  (declare-const vy Int)
  (declare-const vz Int)
  (declare-const t1 Int)
  (declare-const t2 Int)
  (declare-const t3 Int)


  ;; x + t1 * vx = 156689809620606 + t1 * -26
  (assert (= (+ x (* t1 vx)) (+ 156689809620606 (* t1 -26))))

  ;; y + t1 * vy = 243565579389165 + t1 * 48
  (assert (= (+ y (* t1 vy)) (+ 243565579389165 (* t1 48))))

  ;; z + t1 * vz = 455137247320393 + t1 * -140
  (assert (= (+ z (* t1 vz)) (+ 455137247320393 (* t1 -140))))

  ;; x + t2 * vx = 106355761063908 + t2 * 73
  (assert (= (+ x (* t2 vx)) (+ 106355761063908 (* t2 73))))

  ;; y + t2 * vy = 459832650718033 + t2 * -206
  (assert (= (+ y (* t2 vy)) (+ 459832650718033 (* t2 -206))))

  ;; z + t2 * vz = 351953299411025 + t2 * -52
  (assert (= (+ z (* t2 vz)) (+ 351953299411025 (* t2 -52))))

  ;; x + t3 * vx = 271915251832336 + t3 * 31
  (assert (= (+ x (* t3 vx)) (+ 271915251832336 (* t3 31))))

  ;; y + t3 * vy = 487490927073225 + t3 * -414
  (assert (= (+ y (* t3 vy)) (+ 487490927073225 (* t3 -414))))

  ;; z + t3 * vz = 398003502953444 + t3 * -304
  (assert (= (+ z (* t3 vz)) (+ 398003502953444 (* t3 -304))))

  (check-sat)

  (get-model)
  )

(comment
  ;; Returns the following:
  sat
  ((define-fun t3 () Int
     474506740599)
   (define-fun t2 () Int
     829702369046)
   (define-fun vy () Int
     (- 6))
   (define-fun y () Int
     293892176908833)
   (define-fun vz () Int
     155)
   (define-fun z () Int
     180204909018503)
   (define-fun t1 () Int
     931974028142)
   (define-fun x () Int
     446533732372768)
   (define-fun vx () Int
     (- 337)))
  )

;; So add up the values of x, y & z:
(+ 446533732372768 293892176908833 180204909018503)
