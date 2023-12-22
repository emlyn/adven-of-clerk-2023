;; # 🎄 Advent of Clerk: Day 22
(ns advent-of-clerk.day-22
  (:require [nextjournal.clerk]
            [clojure.string :as str]))

(def example "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9")

(defn parse-input
  "Parse input string to seq of [[x1 y1 z1] [x2 y2 z2]] bricks."
  [s]
  (->> s
       str/split-lines
       (map #(str/split % #"~"))
       (map #(mapv (fn [s] (mapv parse-long (str/split s #","))) %))))

(parse-input example)

(defn place-brick
  "Place brick down at its current x-y position until it is resting on another brick or the ground."
  [tops [[x1 y1 z1] [x2 y2 z2]]]
  (let [height ;; highest point under this brick
        (->> (for [x (range x1 (inc x2))
                   y (range y1 (inc y2))]
               (last (last (tops [x y]))))
             (remove nil?)
             (apply max 0))
        thickness (- z2 z1)
        dropped [[x1 y1 (inc height)] [x2 y2 (+ 1 height thickness)]]]
    [(into tops (for [x (range x1 (inc x2))
                      y (range y1 (inc y2))]
                  [[x y] dropped]))
     [dropped (set (for [x (range x1 (inc x2))
                         y (range y1 (inc y2))
                         :let [[[_ _ _] [_ _ z2] :as support] (tops [x y])]
                         :when (= height z2)]
                     support))]]))

(place-brick {} [[1 0 1] [1 2 1]])
(place-brick {[1 1] [[1 1 1] [1 1 1]]} [[1 0 1] [1 2 1]])

(defn place-bricks
  "Place bricks on top of each other, returning the final state.
   `tops` is a map of x-y coordinate to topmost brick at that coordinate
   the height at that coordinate is equal to the z2-coordinate of the brick.
   `bricks` is a seq of tuples of brick and a set of all bricks it is resting on."
  [tops bricks]
  (reduce (fn [[tops bricks] brick]
            (let [[t b] (place-brick tops brick)]
              [t (conj bricks b)]))
          [tops []]
          bricks))

(place-bricks {} [[[1 0 1] [1 2 1]]
                  [[0 0 2] [2 0 2]]])

(place-bricks {} [[[1 0 1] [1 2 1]]
                  [[0 0 2] [2 0 2]]
                  [[0 2 3] [2 2 3]]])

(defn part1
  "Solve(?) part 1."
  [s]
  (let [[_tops bricks] (place-bricks {} (parse-input s))

        bricks ;; re-shape into map
        (into {}
              (map (fn [[b s]] [b {:supported-by s :supports #{}}])
                   bricks))

        bricks ;; fill in :supports info
        (reduce-kv (fn [bricks brick {:keys [supported-by]}]
                     (reduce (fn [bricks supported-by]
                               (update-in bricks [supported-by :supports] conj brick))
                             bricks
                             supported-by))
                   bricks
                   bricks)]
    (->> bricks
         ;; remove any bricks that support a brick that is only supported by one brick
         (remove (fn [[_brick {:keys [supports]}]]
                   (some (fn [b] (->> b bricks :supported-by count (= 1)))
                         supports)))
         count)))

(part1 example)

#_(
;; 1,0,1~1,2,1 ;; A
;; 0,0,2~2,0,2 ;; B **
;; 0,2,3~2,2,3 ;; C **
;; 0,0,4~0,2,4 ;; D **
;; 2,0,5~2,2,5 ;; E **
;; 0,1,6~2,1,6 ;; F
;; 1,1,8~1,1,9 ;; G **

{
 [[1 0 1] [1 2 1]] {:supported-by #{} :supports #{[[0 0 2] [2 0 2]] [[0 2 2] [2 2 2]]}}
 [[0 0 2] [2 0 2]] {:supported-by #{[[1 0 1] [1 2 1]]} :supports #{[[0 0 3] [0 2 3]] [[2 0 3] [2 2 3]]}}
 [[0 2 2] [2 2 2]] {:supported-by #{[[1 0 1] [1 2 1]]} :supports #{[[0 0 3] [0 2 3]] [[2 0 3] [2 2 3]]}}
 [[0 0 3] [0 2 3]] {:supported-by #{[[0 0 2] [2 0 2]] [[0 2 2] [2 2 2]]} :supports #{[[0 1 4] [2 1 4]]}}
 [[2 0 3] [2 2 3]] {:supported-by #{[[0 0 2] [2 0 2]] [[0 2 2] [2 2 2]]} :supports #{[[0 1 4] [2 1 4]]}}
 [[0 1 4] [2 1 4]] {:supported-by #{[[0 0 3] [0 2 3]] [[2 0 3] [2 2 3]]} :supports #{[[1 1 5] [1 1 6]]}}
 [[1 1 5] [1 1 6]] {:supported-by #{[[0 1 4] [2 1 4]]} :supports #{}}
 }
)

(part1 (slurp "input_22.txt")) ;; 449 - too high
