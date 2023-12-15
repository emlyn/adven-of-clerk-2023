;; # 🎄 Advent of Clerk: Day 15
(ns advent-of-clerk.day-15
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(defn HASH
  "Holiday ASCII String Helper algorithm."
  [s]
  (reduce #(mod (* 17 (+ %1 (int %2))) 256) 0 s))

(HASH "HASH")

(def example "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn part1
  "Solve part 1."
  [s]
  (-> s
      str/trim
      (str/split #",")
      (->> (map HASH)
           (apply +))))

(part1 example)

#_(part1 (slurp "input_15.txt"))

(defn add-lens
  "Insert a lens in a box, either replacing the same-labelled lens, or at the end."
  [box label focal]
  (reduce (fn [[done result] slot]
            (if (= slot :end)
              (if done
                (reduced result)
                (reduced (conj result [label focal])))
              (if (= label (first slot))
                [true (conj result [label focal])]
                [done (conj result slot)])))
          [false []]
          (conj box :end)))

(add-lens [] "mm" 42)
(add-lens [["aa" 1]] "mm" 42)
(add-lens [["aa" 1] ["mm" 22]] "mm" 42)
(add-lens [["aa" 1] ["mm" 22] ["zz" 99]] "mm" 42)

(defn update-boxes
  "Update the list of boxes according to the next rule."
  [boxes rule]
  (let [[_ label cmd focal] (re-matches #"([a-z]+)(=|-)([0-9]*)" rule)]
    (case cmd
      "-" (update boxes (HASH label) #(vec (remove (comp #{label} first) %)))
      "=" (update boxes (HASH label) add-lens label (parse-long focal))
      (throw (Exception. (str "Bad rule: " rule))))))

(update-boxes [[["rn" 1]]
               [["qp" 3]]]
              "cm=2")

(defn fpowers
  "Focal powers of all lenses in a box."
  [n box]
  (map-indexed (fn [i [_label focal]]
                 (* (inc n) (inc i) focal))
               box))

(defn part2
  "Solve part 2."
  [s]
  (->> (-> s str/trim (str/split #",$?"))
       (reduce update-boxes
               (into [] (repeat 256 [])))
       (mapcat fpowers (range))
       (apply +)))

(part2 example)

#_(part2 (slurp "input_15.txt"))
