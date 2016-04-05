(ns rl-engine.dungeon-generator.bsp
  (:require [rl-engine.dungeon-generator.bsp.tree :refer :all]))

(def FLOOR 0)
(def WALL 1)

(defn get-empty-room-cell [current-height current-width max-height max-width]
  (cond
    (= 0 current-height) WALL
    (= 0 current-width) WALL
    (= (dec max-height) current-height) WALL
    (= (dec max-width) current-width) WALL
    :else FLOOR))

(defn generate-floor
  [max-height max-width cell-func]
  (into []
        (map #((constantly
                 (let [current-height %]
                   (into []
                         (map (fn [current-width]
                                ((constantly
                                   (cell-func current-height current-width))))
                              (take max-width (range)))))))
             (take max-height (range)))))

(defn generate-empty-floor
  [height width]
  (generate-floor height width (constantly FLOOR)))

(defn get-floor-cell
  [floor x y]
  (get (get floor x) y))

(defn resize-tree-floor
  [tree floor max-height max-width]
  (let [height-offset (:top tree)
        width-offset (:left tree)]
    (generate-floor
      max-height
      max-width
      (fn
        [current-height current-width]
        (let [cell (get-floor-cell
                     floor
                     (- current-height height-offset)
                     (- current-width width-offset))]
          (if (nil? cell)
            0
            cell))))))

(defn sum-floors
  [floor-a floor-b]
  (let [height (count floor-a)
        width (count (first floor-a))]
    (generate-floor
      height
      width
      (fn [x y]
        (let [cell-a (get-floor-cell floor-a x y)
              cell-b (get-floor-cell floor-b x y)]
          (if (or (> cell-a 0) (> cell-b 0))
            WALL
            FLOOR))))))

(defn sum-tree-floors
  [tree-floors max-height max-width]
  (reduce sum-floors (map #(resize-tree-floor (:tree %) (:floor %) max-height max-width) tree-floors)))

(defn generate-floor-from-tree
  [tree]
  (let [max-height (:height tree)
        max-width (:width tree)
        leaf-a (:leaf-a tree)
        leaf-b (:leaf-b tree)
        tree-root-map (generate-floor
                        max-height
                        max-width
                        #(get-empty-room-cell %1 %2 max-height max-width))]
    (if (and (nil? leaf-a) (nil? leaf-b))
      tree-root-map
      (sum-tree-floors [{:floor (generate-floor-from-tree leaf-a)
                         :tree  leaf-a},
                        {:floor (generate-floor-from-tree leaf-b)
                         :tree  leaf-b}]
                       max-height
                       max-width))))

(defn generate-dungeon
  "Generates new dungeon floor."
  ([height width]
   (generate-dungeon height width #(generate-rooms-tree height width)))
  ([height width tree-generator]
   (let [tree (tree-generator)]
     (if (= {} tree)
       (generate-empty-floor height width)
       (generate-floor-from-tree tree)))))
