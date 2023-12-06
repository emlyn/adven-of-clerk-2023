;; # 🎄 Advent of Clerk: Day 6
(ns advent-of-clerk.day-06
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(def example "Time:      7  15   30
Distance:  9  40  200")

(defn parse-input
  [s]
  (->> s
       str/split-lines
       (map #(str/split % #": *"))
       (map second)
       (map #(str/split % #" +"))
       (map #(mapv parse-long %))))

(parse-input example)

(defn num-times
  [time distance]
  (let [t (/ (Math/sqrt (- (* time time) (* 4 distance))) 2)]
    (if (zero? (mod time 2))
      (dec (* 2 (Math/ceil t)))
      (* 2 (Math/ceil (- t 0.5))))))

(defn part1
  [s]
  (->> s
       parse-input
       (apply map num-times)
       (apply *)))

(part1 example)

#_(part1 (slurp "input_06.txt"))

(defn parse-input2
  [s]
  (->> s
       str/split-lines
       (map #(str/split % #": *"))
       (map second)
       (map #(str/split % #" +"))
       (map #(apply str/join % ""))
       (mapv parse-long)))

(parse-input2 example)

(defn part2
  [s]
  (let [[t d] (parse-input2 s)]
    (num-times t d)))

(part2 example)

#_(part2 (slurp "input_06.txt"))
