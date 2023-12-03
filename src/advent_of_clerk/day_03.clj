;; # 🎄 Advent of Clerk: Day 3
(ns advent-of-clerk.day-03
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

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

(defn ->2darray
  "Convert string to 2D array (vector of vector of chars)"
  [s]
  (mapv vec (str/split-lines s)))

(->2darray example)

(defn cell
  "Get cell value from 2D array"
  [grid [x y]]
  (get-in grid [y x]))

(cell (->2darray example) [0 0])
(cell (->2darray example) [3 1])
(cell (->2darray example) [-1 6])

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

(map-grid (fn [v x y]
           (+ v x y))
          [[2 4 6]
           [3 7 9]])

(defn part1
  "Solve part 1"
  [s]
  (let [grid (->2darray s)]
    (loop [positions (map-grid (fn [v x y]
                                 (and (is-digit? v)
                                      (some #(is-symbol? (cell grid %))
                                            (neighbours x y))))
                               grid)]
      (let [nextpositions (map-grid (fn [v x y]
                                      (or v
                                          (and (is-digit? (cell grid [x y]))
                                               (or (cell positions [(dec x) y])
                                                   (cell positions [(inc x) y])))))
                                    positions)]
        (if (not= positions nextpositions)
          (recur nextpositions)
          (->> positions
               (map-grid (fn [v x y]
                           (if v (cell grid [x y]) \.)))
               (map (partial apply str))
               (str/join \newline)
               (re-seq #"\d+")
               (map parse-long)
               (apply +)))))))

(part1 example)

(part1 (slurp "input_03.txt"))

(defn get-num
  "Get the number touching a cell"
  [grid pos x y]
  (let [xmin (last (take-while #(is-digit? (cell grid [% y]))
                               (range x -1 -1)))
        xmax (last (take-while #(is-digit? (cell grid [% y]))
                               (range x (count (first grid)))))]
    [(reduce #(set-cell %1 [%2 y] false) pos (range xmin (inc xmax)))
     (parse-long (apply str (map #(cell grid [% y]) (range xmin (inc xmax)))))]))

(defn get-nums
  "Get numbers reachable from a cell"
  [grid x y]
  (let [[_pos nums]
        (reduce (fn [[pos nums] [xx yy]]
                  (if (and (cell pos [xx yy])
                           (is-digit? (cell grid [xx yy])))
                    (let [[pos* num] (get-num grid pos xx yy)]
                      [pos* (conj nums num)])
                    [pos nums]))
                [(map-grid (constantly true) grid)
                 []]
                (neighbours x y))]
    nums))

(defn part2 [s]
  (let [grid (->2darray s)]
    (apply +
           (reduce (fn [gears [x y]]
                     (if (= \* (cell grid [x y]))
                       (let [nums (get-nums grid x y)]
                         (if (= 2 (count nums))
                           (conj gears (apply * nums))
                           gears))
                       gears))
                   []
                   (for [x (range (count (first grid)))
                         y (range (count grid))]
                     [x y])))))

(part2 example)

(part2 (slurp "input_03.txt"))
