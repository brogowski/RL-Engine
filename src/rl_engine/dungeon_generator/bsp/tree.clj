(ns rl-engine.dungeon-generator.bsp.tree)

(defn is-space-for-room?
  [height width]
  (not (or (< height 3) (< width 3))))

(defn can-split-room?
  [height width ratio]
  (let [can-split-dimension? #(and
                              (>= (int (* % ratio)) 1)
                              (>= (int (* % (- 1 ratio))) 1))]
    (or
      (can-split-dimension? (- width 2))
      (can-split-dimension? (- height 2)))))


(defn split-room
  [height width ratio]
  {:height height
   :width width
   :left 0
   :top 0
   :leaf-a {}
   :leaf-b {}})

(defn generate-rooms-tree
  "Generates tree of rooms."
  ([height width]
   (generate-rooms-tree height width (constantly (rand))))
  ([height width randomizer]
   (when (is-space-for-room? height width)
     (let [ratio (randomizer)]
       (if (can-split-room? height width ratio)
         (split-room height width ratio)
         {:height height
          :width width
          :left 0
          :top 0})))))