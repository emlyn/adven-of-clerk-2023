;; # 🎄 Advent of Clerk: Day 21
(ns advent-of-clerk.day-21
  (:require [nextjournal.clerk]
            [clojure.string :as str]))

(def example "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........")

(defn ->2darray
  "Convert string to 2D array (vector of vector of chars)"
  [s]
  (mapv vec (str/split-lines s)))

(defn cell
  "Get cell value from 2D array"
  [grid [x y]]
  (get-in grid [y x]))

(defn set-cell [grid [x y] v]
  (assoc-in grid [y x] v))

(defn map-grid
  "Map a function over a 2D array"
  [f grid]
  (reduce (fn [g [x y]]
            (set-cell g [x y] (f (cell grid [x y]) x y)))
          grid
          (for [x (range (count (first grid)))
                y (range (count grid))]
            [x y])))

(defn neighbours
  "Get neighbours of a cell"
  [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn part1
  "Solve part 1."
  [s & [steps]]
  (let [grid (->2darray s)]
    (loop [pos (remove nil?
                       (for [x (range (count (first grid)))
                             y (range (count grid))
                             :when (= \S (cell grid [x y]))]
                         [x y]))
           remaining (or steps 64)]
      (if (zero? remaining)
        (count pos)
        (recur (->> pos
                    (mapcat neighbours)
                    (set)
                    (filter #(#{\. \S} (cell grid %))))
               (dec remaining))))))

(part1 example 16)

(part1 (slurp "input_21.txt"))

(count (->2darray (slurp "input_21.txt")))
(count (first (->2darray (slurp "input_21.txt"))))

(defn canonical
  [grid [x y]]
  [(mod y (count grid)) (mod x (count (first grid)))])

(defn cell2
  "Get cell value from 2D array"
  [grid [x y]]
  (get-in grid (canonical grid [x y])))

(defn part2
  "Solve part 2."
  [s & [steps]]
  (let [grid (->2darray s)]
    (loop [pos (remove nil?
                       (for [x (range (count (first grid)))
                             y (range (count grid))
                             :when (= \S (cell2 grid [x y]))]
                         [x y]))
           remaining steps]
      (if (zero? remaining)
        (count pos)
        (recur (->> pos
                    (mapcat neighbours)
                    (set)
                    (filter #(#{\. \S} (cell2 grid %))))
               (dec remaining))))))

(part2 example 10)
(part2 example 50)
(part2 example 100)
(part2 example 500)

;; grid is 131x131 - maybe pattern repeats every 131 steps?
;; target = 26501365 = 131 * 202300 + 65.

;; 2D, so quadratic growth, so take 3 points:

#_(part2 (slurp "input_21.txt") 65)
#_(part2 (slurp "input_21.txt") 196)
#_(part2 (slurp "input_21.txt") 327)

;; after x=65 steps, y=3821,
;; after x=65+131=196, y=34234,
;; after x=65+262=327, y=94963.

;; y = a * x^2 + b * x + c:
;; a = 3971396 / 4496182,
;; b = 7300630 / 4496182,
;; c = - 73777628 / 4496182.

;; y = 3971396 / 4496182 * 26501365 * 26501365 + 7300630 / 4496182 * 26501365 - 73777628 / 4496182
;;   = 620,348,631,910,321
