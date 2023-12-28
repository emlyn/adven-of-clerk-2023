;; # 🎄 Advent of Clerk: Day 11
(ns advent-of-clerk.day-11
  (:require [nextjournal.clerk]
            [emlyn.grid :as g]))

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

(defn part1
  "Solve part 1 (and part 2!)"
  [s & [ex]]
  (let [grid (g/grid s)
        ymap (first (reduce (fn [[m n] y]
                              (if (not-any? #{\#} (map #(grid [% y])
                                                       (range (g/width grid))))
                                [m (+ n (or ex 2))]
                                [(assoc m y n) (inc n)]))
                            [{} 0]
                            (range (g/height grid))))
        xmap (first (reduce (fn [[m n] x]
                              (if (not-any? #{\#} (map #(grid [x %])
                                                       (range (g/height grid))))
                                [m (+ n (or ex 2))]
                                [(assoc m x n) (inc n)]))
                            [{} 0]
                            (range (g/width grid))))
        stars (->> grid
                   (g/map-kv (fn [[x y] c]
                               (when (= c \#)
                                 [x y])))
                   g/as-rows
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
