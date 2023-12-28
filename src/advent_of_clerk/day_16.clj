;; # 🎄 Advent of Clerk: Day 16
(ns advent-of-clerk.day-16
  (:require [nextjournal.clerk]
            [emlyn.grid :as g]))

(def example ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")

(defn next-dirs
  "Get the next directions a beam will go after entering a cell"
  [c dx dy]
  (case c
    nil []
    \.  [[dx dy]]
    \\  [[dy dx]]
    \/  [[(- dy) (- dx)]]
    \-  (if (zero? dx)
          [[-1 0] [1 0]]
          [[dx dy]])
    \|  (if (zero? dx)
          [[dx dy]]
          [[0 -1] [0 1]])
    (throw (Exception. (str "Unrecognised cell: " c)))))

(defn step
  "Get the beam(s) after a beam moves one step through the grid"
  [grid [x y dx dy]]
  (let [x (+ x dx)
        y (+ y dy)]
    (for [[dx dy] (next-dirs (grid [x y]) dx dy)]
      [x y dx dy])))

(defn energized
  "Count the number of cells energized given a starting beam"
  [grid x y dx dy]
  (loop [beams (for [[dx dy] (next-dirs (grid [x y]) dx dy)]
                 [x y dx dy])
         seen (set beams)]
    (if-let [next (->> beams
                       (mapcat (partial step grid))
                       (remove seen)
                       seq)]
      (recur next (into seen next))
      (->> seen
           (map (juxt first second))
           set
           count))))

(defn part1
  "Solve part 1."
  [s]
  (energized (g/grid s) 0 0 1 0))

(part1 example)

#_(part1 (slurp "input_16.txt"))

(defn part2
  "Solve part 2."
  [s]
  (let [grid (g/grid s)]
    (apply max
     (concat
      (for [y (range (g/height grid))
            [x dx] [[0 1] [(dec (g/width grid)) -1]]]
        (energized grid x y dx 0))
      (for [x (range (g/width grid))
            [y dy] [[0 1] [(dec (g/height grid)) -1]]]
        (energized grid x y 0 dy))))))

(part2 example)

#_(part2 (slurp "input_16.txt"))
