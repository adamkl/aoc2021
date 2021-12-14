(ns aoc2021.9
  (:require [clojure.string :as string]
            [clojure.set :as set]))

;; --- Day 9: Smoke Basin ---

;; These caves seem to be lava tubes. Parts are even still volcanically active; small hydrothermal vents release smoke into the caves that slowly settles like rain.

;; If you can model how the smoke flows through the caves, you might be able to avoid it and be that much safer. The submarine generates a heightmap of the floor of the nearby caves for you (your puzzle input).

;; Smoke flows to the lowest point of the area it's in. For example, consider the following heightmap:

;; 2199943210
;; 3987894921
;; 9856789892
;; 8767896789
;; 9899965678
;; Each number corresponds to the height of a particular location, where 9 is the highest and 0 is the lowest a location can be.

;; Your first goal is to find the low points - the locations that are lower than any of its adjacent locations. Most locations have four adjacent locations (up, down, left, and right); locations on the edge or corner of the map have three or two adjacent locations, respectively. (Diagonal locations do not count as adjacent.)

;; In the above example, there are four low points, all highlighted: two are in the first row (a 1 and a 0), one is in the third row (a 5), and one is in the bottom row (also a 5). All other locations on the heightmap have some lower adjacent location, and so are not low points.

;; The risk level of a low point is 1 plus its height. In the above example, the risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels of all low points in the heightmap is therefore 15.

;; Find all of the low points on your heightmap. What is the sum of the risk levels of all low points on your heightmap?

;; --- Part Two ---

;; Next, you need to find the largest basins so you know what areas are most important to avoid.

;; A basin is all locations that eventually flow downward to a single low point. Therefore, every low point has a basin, although some basins are very small. Locations of height 9 do not count as being in any basin, and all other locations will always be part of exactly one basin.

;; The size of a basin is the number of locations within the basin, including the low point. The example above has four basins.

;; The top-left basin, size 3:

;; 2199943210
;; 3987894921
;; 9856789892
;; 8767896789
;; 9899965678
;; The top-right basin, size 9:

;; 2199943210
;; 3987894921
;; 9856789892
;; 8767896789
;; 9899965678
;; The middle basin, size 14:

;; 2199943210
;; 3987894921
;; 9856789892
;; 8767896789
;; 9899965678
;; The bottom-right basin, size 9:

;; 2199943210
;; 3987894921
;; 9856789892
;; 8767896789
;; 9899965678
;; Find the three largest basins and multiply their sizes together. In the above example, this is 9 * 14 * 9 = 1134.

;; What do you get if you multiply together the sizes of the three largest basins?

(defn parse-input [input]
  (let [parsed-input (->> input
                          string/split-lines
                          (map seq)
                          (map (fn [nums] (->> nums (map #(Character/digit % 10)) vec)))
                          vec)
        y-max (-> parsed-input count)
        x-max (-> parsed-input first count)
        dimensions [x-max y-max]
        points (-> parsed-input flatten vec)]
    {:dimensions dimensions
     :points points
     :input parsed-input}))

(defn translate-coords [x y [x-max]]
  (+ x (* y x-max)))

;; n s e w
(defn calc-offsets [x y [x-max y-max]]
  [(if (= y 0) nil [0 -1])
   (if (= (inc y) y-max) nil [0 1])
   (if (= (inc x) x-max) nil [1 0])
   (if (= x 0) nil [-1 0])])

(defn get-adj [x y points dimensions]
  (->> (calc-offsets x y dimensions)
       (filter some?)
       (map (fn [[x-off y-off]] (get points (translate-coords (+ x x-off) (+ y y-off) dimensions))))))

(defn lowest-point? [x y points dimensions]
  (let [point (get points (translate-coords x y dimensions))
        adj (get-adj x y points dimensions)]
    (if (every? identity (map #(> 0 (- point %)) adj)) ;; point lower than all adjacent? 
      [x y]
      nil)))

(defn edge? [x [x-max]]
  (= (inc x) x-max))

(defn get-lowest-points [points [x-max y-max :as dimensions]]
  (loop [x 0
         y 0
         lowest-points []]
    (if (< (translate-coords x y dimensions) (* x-max y-max))
      (recur (if (edge? x dimensions) 0 (inc x))
             (if (edge? x dimensions) (inc y) y)
             (conj lowest-points (lowest-point? x y points dimensions)))
      (filter some? lowest-points))))


(defn get-total-risk [{:keys [points dimensions]}]
  (->> (get-lowest-points points dimensions)
       (map (fn [[x y]] (-> (get points (translate-coords x y dimensions)) (+ 1))))
       (apply +)))

(defn get-adj-coords [[x y] dimensions]
  (->> (calc-offsets x y dimensions)
       (filter some?)
       (map (fn [[x-off y-off]] [(+ x x-off) (+ y y-off)]))))

(defn search-sub [[x y :as curr] input dimensions visited]
  (let [val (-> input (get y) (get x))]
    (if (and (every? identity (map (partial not= curr) @visited))
             (< val 9))
      (do
        (swap! visited conj curr)
        (+ 1 (->> (get-adj-coords curr dimensions)
                  (map #(search-sub % input dimensions visited))
                  (apply +))))
      0)))

(defn search [curr input dimensions]
  (let [visited (atom [])]
    (search-sub curr input dimensions visited)))

(defn get-basin-score [{:keys [points dimensions input]}]
  (let [lowest-points (get-lowest-points points dimensions)
        basins (->> lowest-points
                    (map #(search % input dimensions))
                    (sort-by -))]
    (->> basins
         (take 3)
         (apply *))))

(comment
  (def test-input "2199943210
3987894921
9856789892
8767896789
9899965678")

  (def input (slurp "data/9.txt"))

  ;; 9a
  (-> test-input
      parse-input
      get-total-risk) ;; 15

  (-> input
      parse-input
      get-total-risk) ;; 462

  ;; 9b
  (-> test-input
      parse-input
      get-basin-score) ;; 1134

  (-> input
      parse-input
      get-basin-score) ;; 1397760

  (comment))