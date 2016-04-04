(ns rl-engine.dungeon-generator.bsp.tree)

;TODO: change to recur

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

(defn split-coordinate-for-leaf-a
  [min-position max-position ratio]
  min-position)

(defn split-coordinate-for-leaf-b
  [min-position max-position ratio]
  (+ min-position (int (* max-position ratio))))

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
  [height width ratio randomizer]
  (if (and
        (can-split-dimension? height ratio)
        (can-split-dimension? width ratio))
    (if (>= (randomizer) 0.5)
      WIDTH
      HEIGHT)
    (if (can-split-dimension? height ratio)
      HEIGHT
      WIDTH)))

(defn get-leaf
  [root split-dimension ratio split-dimension-func split-coordinate-func]
  (let [height (:height root)
        width (:width root)
        top (:top root)
        left (:left root)]
    {:height (if (= HEIGHT split-dimension)
               (+ 1 (split-dimension-func height ratio))
               height)
     :width  (if (= WIDTH split-dimension)
               (+ 1 (split-dimension-func width ratio))
               width)
     :top    (- (if (= HEIGHT split-dimension)
                  (split-coordinate-func top height ratio)
                  top)
                top)
     :left   (- (if (= WIDTH split-dimension)
                  (split-coordinate-func left width ratio)
                  left)
                left)}))

(defn get-leaf-a
  [root split-dimension ratio]
  (get-leaf root split-dimension ratio
            split-dimension-for-leaf-a
            split-coordinate-for-leaf-a))

(defn get-leaf-b
  [root split-dimension ratio]
  (get-leaf root split-dimension ratio
            split-dimension-for-leaf-b
            split-coordinate-for-leaf-b))

(defn split-room
  [root randomizer]
  (let [height (:height root)
        width (:width root)
        top (:top root)
        left (:left root)
        ratio (randomizer)]
    (if (can-split-room? height width ratio)
      (let [split-dimension (get-split-dimension height width ratio randomizer)]
        {:height height
         :width  width
         :top    top
         :left   left
         :leaf-a (split-room (get-leaf-a root split-dimension ratio) randomizer)
         :leaf-b (split-room (get-leaf-b root split-dimension ratio) randomizer)})
      root)))

(defn generate-rooms-tree
  "Generates tree of rooms."
  ([height width]
   (generate-rooms-tree height width (constantly (rand))))
  ([height width randomizer]
   (if (is-space-for-room? height width)
     (let [root {:height height
                 :width  width
                 :left   0
                 :top    0}]
       (split-room root randomizer))
     {})))