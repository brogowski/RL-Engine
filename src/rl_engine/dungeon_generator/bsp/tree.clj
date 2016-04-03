(ns rl-engine.dungeon-generator.bsp.tree)

(def HEIGHT "HEIGHT")
(def WIDTH "WIDTH")
(def MINIMUM_SIZE 3)

(defn is-space-for-room?
  [height width]
  (not (or (< height MINIMUM_SIZE) (< width MINIMUM_SIZE))))

(defn split-dimension-for-leaf-a
  [size ratio]
  (int (* size ratio)))

(defn split-dimension-for-leaf-b
  [size ratio]
  (if (= 0 (mod size 2))
         (int (* (- size 1) (- 1 ratio)))
         (int (* size (- 1 ratio)))))

(defn can-split-dimension?
  [size ratio]
  (and
    (>= (split-dimension-for-leaf-a (- size 2) ratio) 1)
    (>= (split-dimension-for-leaf-b (- size 2) ratio) 1)))

(defn can-split-room?
  [height width ratio]
  (or
    (can-split-dimension? width ratio)
    (can-split-dimension? height ratio)))

(defn get-split-dimension
  [height width ratio]
  (if (can-split-dimension? height ratio)
    HEIGHT
    WIDTH))

(defn split-room
  [height width ratio]
  (let [split-dimension (get-split-dimension height width ratio)]
    {:height height
     :width  width
     :left   0
     :top    0
     :leaf-a {:height (if (= HEIGHT split-dimension)
                        (+ 1 (split-dimension-for-leaf-a height ratio))
                        height)
              :width  (if (= WIDTH split-dimension)
                        (+ 1 (split-dimension-for-leaf-a width ratio))
                        width)
              :left   0
              :top    0}
     :leaf-b {:height (if (= HEIGHT split-dimension)
                        (+ 1 (split-dimension-for-leaf-b height ratio))
                        height)
              :width  (if (= WIDTH split-dimension)
                        (+ 1 (split-dimension-for-leaf-b width ratio))
                        width)
              :left   0
              :top    0}}))

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
          :width  width
          :left   0
          :top    0})))))