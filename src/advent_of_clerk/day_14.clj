;; # 🎄 Advent of Clerk: Day 14
(ns advent-of-clerk.day-14
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(def example "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(defn part-load
  "Load on one strip after stones have shifted as far as they can."
  [strip]
  (loop [distance (count strip)
         part distance
         total 0
         [rock & rest] strip]
    (case rock
      nil total
      \.  (recur (dec distance) part total rest)
      \#  (recur (dec distance) (dec distance) total rest)
      \O  (recur (dec distance) (dec part) (+ total part) rest))))

(defn part1
  "Solve part 1."
  [s]
  (->> s
       str/split-lines
       (apply (partial map str))
       (map part-load)
       (apply +)))

(part1 example)

#_(part1 (slurp "input_14.txt"))

(defn rstr
  "Reverse a string"
  [s]
  (apply str (reverse s)))

(defn rotate90
  "Rotate a vector of strings 90 degrees clockwise"
  [platform]
  (map rstr (apply (partial map str) platform)))

(rotate90
 ["1234"
  "abcd"
  "6789"
  "wxyz"])

(defn rotate180
  [platform]
  (map rstr (reverse platform)))

(rotate180
 ["1234"
  "abcd"
  "6789"
  "wxyz"])

(defn update-stones
  "Helper to update a partial strip with rolled stones."
  [existing stones length]
  (str existing
       (apply str (repeat stones \O))
       (apply str (repeat (- length stones (count existing)) \.))))

(defn roll
  "Roll stones to the beginning of a strip"
  [strip]
  (loop [distance 1
         num 0
         result ""
         [rock & rest] strip]
    (case rock
      nil (update-stones result num (dec distance))
      \.  (recur (inc distance) num result rest)
      \#  (recur (inc distance) 0 (str (update-stones result num (dec distance)) \#) rest)
      \O  (recur (inc distance) (inc num) result rest))))

(roll ".O..O#O..##.O.O.")

(defn step
  "Perform one step of a cycle (rotate 90 degrees and roll stones)"
  [platform]
  (->> platform
       rotate90
       (map roll)))

(def init-platform (->> example str/split-lines rotate180))

(-> init-platform step rotate180)

(defn cycle-platform
  "Perform a full cycle of the platform."
  [platform]
  (->> platform step step step step))

(->> init-platform cycle-platform rotate180)

(defn part-load2
  "Load on one strip (without moving the stones)."
  [s]
  (->> s
       (map-indexed (fn [i c]
                      (if (= c \O) (- (count s) i) 0)))
       (apply +)))

(->> init-platform
     step
     (map part-load2)
     (apply +))

(defn part2
  "Solve part 2."
  [s]
  (loop [platform (-> s str/split-lines rotate180)
         cycles 0
         seen {}]
    (if (seen platform)
      (let [remaining (mod (- 1000000 1 cycles)
                           (- cycles (seen platform)))]
        (->> platform
             (iterate cycle-platform)
             (take remaining)
             last
             rotate90
             (map part-load2)
             (apply +)))
      (recur (cycle-platform platform)
             (inc cycles)
             (assoc seen platform cycles)))))

(part2 example)

#_(part2 (slurp "input_14.txt"))
