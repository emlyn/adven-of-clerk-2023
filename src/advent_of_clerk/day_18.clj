;; # 🎄 Advent of Clerk: Day 18
(ns advent-of-clerk.day-18
  (:require [nextjournal.clerk]
            [clojure.string :as str]))

(def example "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
")

(defn parse-input
  "Extract directions and numbers of steps from input string."
  [s]
  (->> s
       str/split-lines
       (map #(str/split % #" +"))
       (map (fn [[d n _c]]
              [d (parse-long n)]))))

(parse-input example)

(defn move
  "Move `pos` one step in direction `dir`"
  [dir pos]
  (mapv + pos
        (case dir
          "R" [1 0]
          "L" [-1 0]
          "U" [0 -1]
          "D" [0 1])))

(defn path
  "Get the path traced by the given input as a map of cell coordinate
   to path direction(s) passing through that cell."
  [s]
  (loop [grid {}
         pos [0 0]
         [[d n] & more] (parse-input s)]
    (if d
      (let [moves (take (inc n) (iterate (partial move d) pos))]
        (recur (reduce (fn [g p]
                         (update g p #(conj (or % #{}) d)))
                       grid
                       moves)
               (last moves)
               more))
      grid)))

(path example)

(defn inside?
  "Given a sequence of cells from the edge of the grid to a cell,
   determine if the final cell is inside the path."
  [cells]
  (letfn [(half [n] (/ n 2))]
    (->> cells
         (map {#{"U"} 2
               #{"D"} 2
               #{"L" "D"} 1
               #{"R" "D"} 1
               #{"L" "U"} -1
               #{"R" "U"} -1})
         (remove nil?)
         (apply +)
         half
         odd?)))

(inside? [])

(defn part1
  "Solve part 1."
  [s]
  (let [path (path s)
        [minx maxx] (apply (juxt min max) (map first (keys path)))
        [miny maxy] (apply (juxt min max) (map last (keys path)))]
    (count
     (for [x (range minx (inc maxx))
           y (range miny (inc maxy))
           :when (or (path [x y])
                     (inside? (map #(path [% y]) (range minx x))))]
       1))))

(part1 example)

#_(part1 (slurp "input_18.txt"))

(defn parse-input2
  "Extract directions and numbers of steps from input string hex codes for part 2."
  [s]
  (->> s
       str/split-lines
       (map #(str/split % #" +"))
       (map last)
       (map (juxt #(Long/parseLong (subs % 2 7) 16)
                  (comp last butlast)))))

(parse-input2 example)

(defn move2
  "Move `pos` `num` steps in direction `dir`"
  [pos [num dir]]
  (mapv + pos
        (case dir
          \0 [num 0]     ;; R
          \2 [(- num) 0] ;; L
          \3 [0 (- num)] ;; U
          \1 [0 num])))  ;; D

(defn part2
  "Solve part 2."
  [s]
  (let [data (parse-input2 s)
        perimeter (->> data (map first) (apply +))]
    (->> data
         ;; convert to list of corner positions (vertices):
         (reductions move2 [0 0])
         ;; Get every pair of neighbouring vertices along the path:
         (partition 2 1)
         ;; Shoelace formula (area of polygon given vertices):
         (map (fn [[[x1 y1] [x2 y2]]]
                (- (* x1 y2) (* y1 x2))))
         (apply +)
         ;; Ensure positive area (could be negative depending on direction of path):
         abs
         ;; Add (perimeter/2 + 1) to account for width of edge
         (+ 2 perimeter)
         (* 0.5))))

(part2 example)

#_(part2 (slurp "input_18.txt"))
