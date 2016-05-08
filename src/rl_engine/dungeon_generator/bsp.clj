(ns rl-engine.dungeon-generator.bsp
  (:require [rl-engine.dungeon-generator.bsp.tree :refer :all]))

(def FLOOR 0)
(def WALL 1)
(def DOOR 2)

(defn get-room-cell [current-height current-width
                     max-height max-width
                     entrance]
  (let [entrance-height (:top entrance)
        entrance-width (:left entrance)]
    (cond
      (and (= entrance-height current-height)
           (= entrance-width current-width)) DOOR
      (= 0 current-height) WALL
      (= 0 current-width) WALL
      (= (dec max-height) current-height) WALL
      (= (dec max-width) current-width) WALL
      :else FLOOR)))

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
    {:floor (generate-floor
              max-height
              max-width
              (fn
                [current-height current-width]
                (get-floor-cell
                  floor
                  (- current-height height-offset)
                  (- current-width width-offset))))
     :tree  tree}))

(defn connect-floor-entrances
  [floor entrance-a entrance-b]
  (let [height (count floor)
        width (count (first floor))
        entrance-a-height (:top entrance-a)
        entrance-a-width  (:left entrance-a)
        entrance-b-height (:top entrance-b)
        entrance-b-width  (:left entrance-b)
        horizontal-entrance-corridor? (= entrance-a-height entrance-b-height)
        should-be-floor? (fn [position lower-bound upper-bound]
                           (and (< position upper-bound)
                                (> position lower-bound)))]
    (generate-floor height
                    width
                    (fn [current-height current-width]
                      (let [in-corridor-position (if horizontal-entrance-corridor?
                                                   current-width
                                                   current-height)
                            corridor-position (if horizontal-entrance-corridor?
                                                current-height
                                                current-width)
                            required-corridor-position (if horizontal-entrance-corridor?
                                                         entrance-a-height
                                                         entrance-a-width)
                            lower-bound (if horizontal-entrance-corridor?
                                          (min entrance-a-width entrance-b-width)
                                          (min entrance-a-height entrance-b-height))
                            upper-bound (if horizontal-entrance-corridor?
                                          (max entrance-a-width entrance-b-width)
                                          (max entrance-a-height entrance-b-height))]
                        (if (and (= required-corridor-position corridor-position)
                                 (should-be-floor? in-corridor-position lower-bound upper-bound))
                          FLOOR
                          (get-floor-cell
                            floor
                            current-height
                            current-width)))))))

(defn sum-same-size-tree-floors
  [tree-floor-a tree-floor-b]
  (let [floor-a (:floor tree-floor-a)
        floor-b (:floor tree-floor-b)
        height (count floor-a)
        width (count (first floor-a))]
    (let [new-floor (generate-floor
                      height
                      width
                      (fn [x y]
                        (let [cell-a (get-floor-cell floor-a x y)
                              cell-b (get-floor-cell floor-b x y)]
                          (cond
                            (and (nil? cell-a)
                                 (nil? cell-b)) WALL
                            (nil? cell-a) cell-b
                            (nil? cell-b) cell-a
                            :else (max cell-a cell-b)))))]
      (let [tree-a (:tree tree-floor-a)
            tree-b (:tree tree-floor-b)
            entrance-a (:entrance tree-a)
            entrance-b (:entrance tree-b)
            offset-a (select-keys tree-a (keys entrance-a))
            offset-b (select-keys tree-b (keys entrance-b))
            corrected-entrance-a (merge-with + entrance-a offset-a)
            corrected-entrance-b (merge-with + entrance-b offset-b)]
        (connect-floor-entrances new-floor
                                 corrected-entrance-a
                                 corrected-entrance-b)))))

(defn sum-tree-floors
  [tree-floors max-height max-width]
  (reduce sum-same-size-tree-floors
          (map #(resize-tree-floor (:tree %) (:floor %) max-height max-width)
               tree-floors)))

(defn generate-floor-from-tree
  [tree]
  (let [max-height (:height tree)
        max-width (:width tree)
        leaf-a (:leaf-a tree)
        leaf-b (:leaf-b tree)
        entrance (:entrance tree)
        tree-root-map (generate-floor
                        max-height
                        max-width
                        #(get-room-cell %1 %2 max-height max-width entrance))]
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
