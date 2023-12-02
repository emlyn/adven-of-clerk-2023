;; # 🎄 Advent of Clerk: Day 2
(ns advent-of-clerk.day-02
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(defn parse-draw [s]
  (let [colours (str/split s #", *")]
    (reduce (fn [m s]
              (let [[n colour] (str/split s #" +")]
                (assoc m colour (parse-long n))))
            {} colours)))

(parse-draw "3 blue, 4 red")

(defn parse-game [s]
  (let [[a b] (str/split s #": *" 2)
        draws (str/split b #"; *")]
    (into [(parse-long (last (str/split a #" +")))]
          (map parse-draw draws))))

(parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

(defn possible-draw? [cubes draw]
  (every? (fn [[colour n]]
            (<= n (get cubes colour 0)))
          draw))

(defn possible-game? [cubes game]
  (every? (partial possible-draw? cubes) (rest game)))

(defn part1 [init-cubes s]
  (->> s
       str/split-lines
       (map parse-game)
       (filter (partial possible-game? init-cubes))
       (map first)
       (apply +)))

(def initial-cubes
  {"red" 12,
   "green" 13,
   "blue" 14})

(def example "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(part1 initial-cubes example)

(part1 initial-cubes (slurp "input_02.txt"))

(defn min-cubes [game]
  (apply merge-with max (rest game)))

(defn power [draw]
  (apply * (vals draw)))

(defn part2 [s]
  (->> s
       str/split-lines
       (map parse-game)
       (map min-cubes)
       (map power)
       (apply +)))

(part2 example)

(part2 (slurp "input_02.txt"))
