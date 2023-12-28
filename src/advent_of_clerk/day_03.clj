;; # 🎄 Advent of Clerk: Day 3
(ns advent-of-clerk.day-03
  (:require [nextjournal.clerk]
            [clojure.string :as str]
            [emlyn.grid :as g]))

(def example "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defn is-digit?
  "Is character a digit?"
  [c]
  (let [digits (into #{} "0123456789")]
    (digits c)))

(mapv is-digit? "1.$2")

(defn is-symbol?
  "Is character a symbol?"
  [c]
  (let [nonsymbols #{\space \newline \.}]
    (and c
         (not (or (is-digit? c)
                  (nonsymbols c))))))

(mapv is-symbol? "1.$2")

(defn neighbours
  "Get neighbours of a cell"
  [x y]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not (and (zero? dx) (zero? dy)))]
    [(+ x dx) (+ y dy)]))

(neighbours 0 2)

(defn part1
  "Solve part 1"
  [s]
  (let [grid (g/grid s)]
    (loop [positions (g/map-kv (fn [[x y] v]
                                 (and (is-digit? v)
                                      (some #(is-symbol? (grid %))
                                            (neighbours x y))))
                               grid)]
      (let [nextpositions (g/map-kv (fn [[x y] v]
                                      (or v
                                          (and (is-digit? (grid [x y]))
                                               (or (positions [(dec x) y])
                                                   (positions [(inc x) y])))))
                                    positions)]
        (if (not= positions nextpositions)
          (recur nextpositions)
          (->> positions
               (g/map-kv (fn [[x y] v]
                           (if v (grid [x y]) \.)))
               (g/as-rows)
               (map (partial apply str))
               (str/join \newline)
               (re-seq #"\d+")
               (map parse-long)
               (apply +)))))))

(part1 example)

#_(part1 (slurp "input_03.txt"))

(defn get-num
  "Get the number touching a cell"
  [grid pos x y]
  (let [xmin (last (take-while #(is-digit? (grid [% y]))
                               (range x -1 -1)))
        xmax (last (take-while #(is-digit? (grid [% y]))
                               (range x (g/width grid))))]
    [(reduce #(assoc %1 [%2 y] false) pos (range xmin (inc xmax)))
     (parse-long (apply str (map #(grid [% y]) (range xmin (inc xmax)))))]))

(defn get-nums
  "Get numbers reachable from a cell"
  [grid x y]
  (let [[_pos nums]
        (reduce (fn [[pos nums] [xx yy]]
                  (if (and (pos [xx yy])
                           (is-digit? (grid [xx yy])))
                    (let [[pos* num] (get-num grid pos xx yy)]
                      [pos* (conj nums num)])
                    [pos nums]))
                [(g/map-vals (constantly true) grid)
                 []]
                (neighbours x y))]
    nums))

(defn part2 [s]
  (let [grid (g/grid s)]
    (apply +
           (reduce (fn [gears [x y]]
                     (if (= \* (grid [x y]))
                       (let [nums (get-nums grid x y)]
                         (if (= 2 (count nums))
                           (conj gears (apply * nums))
                           gears))
                       gears))
                   []
                   (keys grid)))))

(part2 example)

#_(part2 (slurp "input_03.txt"))
