;; # 🎄 Advent of Clerk: Day 25
(ns advent-of-clerk.day-25
  (:require [nextjournal.clerk]
            [clojure.string :as str]))

(def example "jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr")

(defn parse-input
  "Parse input string into a graph. Represented as a map of source node
   to map of destination node to number of wires (initially always 1)."
  [s]
  (->> s
       str/split-lines
       (map #(str/split % #" *: *"))
       (map (fn [[name deps]]
              [name (str/split deps #" +")]))
       (reduce (fn [m [n ns]]
                 (reduce (fn [m nn]
                           (-> m
                               (update-in [nn n] (fnil inc 0))
                               (update-in [n nn] (fnil inc 0))))
                         m
                         ns))
               {})))

(parse-input example)

(defn contract
  "Contract two nodes in a graph into a single supernode.
   The new node will be named by concatenating the names of the two nodes separated by a point."
  [g [n1 n2]]
  (let [supernode (str n1 \. n2)
        connected (merge-with +
                              (dissoc (g n1) n2)
                              (dissoc (g n2) n1))]
    (reduce (fn [g con]
              (let [c1 (get-in g [con n1] 0)
                    c2 (get-in g [con n2] 0)]
                (-> g
                    (update con dissoc n1)
                    (update con dissoc n2)
                    (update con assoc supernode (+ c1 c2)))))
               (-> g
                   (dissoc n1)
                   (dissoc n2)
                   (assoc supernode connected))
               (keys connected))))

(contract {"a" {"b" 1 "c" 1}
           "b" {"a" 1 "c" 1}
           "c" {"a" 1 "b" 1}}
          ["a" "b"])

(defn rand-edge
  "Select an edge at random from a graph."
  [g]
  (let [edges (vec (for [[n1 conns] g
                         [n2 num] conns]
                     [n1 n2 num]))
        probs (vec (reductions + (map last edges)))
        r (rand-int (last probs))]
    (loop [lo 0
           hi (count probs)]
      (if (= lo hi)
        (let [[n1 n2 _] (edges lo)]
          [n1 n2])
        (let [mid (quot (+ lo hi) 2)
              p (probs mid)]
          (if (<= p r)
            (recur (inc mid) hi)
            (recur lo mid)))))))

(frequencies (repeatedly 1000 #(rand-edge {"a" {"b" 1 "c" 2}
                                           "b" {"a" 1 "c" 2}
                                           "c" {"a" 2 "b" 2}})))

(defn partition-graph
  "Partition a graph in two by contracting edges at random until only two nodes remain."
  [g]
  (if (<= (count g) 2)
    g
    (let [e (rand-edge g)]
      (recur (contract g e)))))

(partition-graph (parse-input example))

(defn part1
  "Solve part 1."
  [s]
  (let [g (parse-input s)]
    (loop [best nil
           num 0]
      ;; Repeatedly try partitioning the graph until we come up with
      ;; a partition separated by 3 wires (Karger's algorithm).
      (let [g (partition-graph g)
            wires (first (vals (first (vals g))))]
        (cond
          (<= wires 3) (let [[n1 gg] (first g)
                             [n2 _] (first gg)]
                         (* (count (str/split n1 #"[.]"))
                            (count (str/split n2 #"[.]"))))
          (> num 100000) (throw (Exception. "Too many iterations"))
          (nil? best) (recur wires (inc num))
          (< wires best) (recur wires (inc num))
          :else (recur best (inc num)))))))

(part1 example)

#_(part1 (slurp "input_25.txt"))

(defn part2
  "There is no part 2."
  [])
