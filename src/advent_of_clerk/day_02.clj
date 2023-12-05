;; # 🎄 Advent of Clerk: Day 2
(ns advent-of-clerk.day-02
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(defn parse-draw
  "Parse a draw (e.g. '3 blue, 4 red') into a map of colour to count"
  [s]
  (let [colours (str/split s #", *")]
    (reduce (fn [m s]
              (let [[n colour] (str/split s #" +")]
                (assoc m colour (parse-long n))))
            {} colours)))

(parse-draw "3 blue, 4 red")

(defn parse-game
  "Parse a game (game ID plus sequence semicolon-separated of draws)"
  [s]
  (let [[a b] (str/split s #": *" 2)
        draws (str/split b #"; *")]
    (into [(parse-long (last (str/split a #" +")))]
          (map parse-draw draws))))

(parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

(defn possible-draw?
  "Is a draw possible given the cubes available?"
  [cubes draw]
  (every? (fn [[colour n]]
            (<= n (get cubes colour 0)))
          draw))

(defn possible-game?
  "Is a game possible given the cubes available?"
  [cubes game]
  (every? (partial possible-draw? cubes) (rest game)))

(def initial-cubes
  {"red" 12,
   "green" 13,
   "blue" 14})

(defn part1
  "Solve part 1"
  [s]
  (->> s
       str/split-lines
       (map parse-game)
       (filter (partial possible-game? initial-cubes))
       (map first)
       (apply +)))

(def example "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(part1 example)

#_(part1 (slurp "input_02.txt"))

(defn min-cubes
  "Compute the minimum number of cubes needed to play a game"
  [game]
  (apply merge-with max (rest game)))

(defn power
  "Compute the 'power' of a set of cubes"
  [cubes]
  (apply * (vals cubes)))

(defn part2
  "Solve part 2"
  [s]
  (->> s
       str/split-lines
       (map parse-game)
       (map min-cubes)
       (map power)
       (apply +)))

(part2 example)

#_(part2 (slurp "input_02.txt"))
