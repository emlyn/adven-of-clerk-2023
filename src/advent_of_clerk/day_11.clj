;; # 🎄 Advent of Clerk: Day 11
(ns advent-of-clerk.day-11
  (:require [nextjournal.clerk]
            [clojure.string :as str]))

(def example "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(defn ->2darray
  "Convert string to 2D array (vector of vector of chars)"
  [s]
  (mapv vec (str/split-lines s)))

(->2darray example)

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

(defn part1
  "Solve part 1 (and part 2!)"
  [s & [ex]]
  (let [grid (->2darray s)
        ymap (first (reduce (fn [[m n] [y r]]
                                (if (not-any? #{\#} r)
                                  [m (+ n (or ex 2))]
                                  [(assoc m y n) (inc n)]))
                              [{} 0]
                              (map vector (range) grid)))
        xmap (first (reduce (fn [[m n] x]
                                (if (not-any? #{\#} (map #(cell grid [x %])
                                                         (range (count grid))))
                                  [m (+ n (or ex 2))]
                                  [(assoc m x n) (inc n)]))
                              [{} 0]
                              (range (count (first grid)))))
        stars (->> grid
                   (map-grid (fn [c x y]
                               (when (= c \#)
                                 [x y])))
                   (mapcat #(remove nil? %))
                   vec)]
    (apply + (for [s1 (range (dec (count stars)))
                   s2 (range (inc s1) (count stars))
                   :let [[x1 y1] (stars s1)
                         [x2 y2] (stars s2)]]
               (+ (abs (- (xmap x1) (xmap x2)))
                  (abs (- (ymap y1) (ymap y2))))))))

(part1 example)

#_(part1 (slurp "input_11.txt"))

;; No need for any changes for part 2 :-)

(part1 example 10)

(part1 example 100)

#_(part1 (slurp "input_11.txt") 1000000)
