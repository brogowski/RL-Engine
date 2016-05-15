(ns rl-engine.dungeon-generator.bsp.tree)

;TODO: change to recur

(def HEIGHT "HEIGHT")
(def WIDTH "WIDTH")
(def MINIMUM_SIZE 3)

(defn not-has-leafs?
  [root]
  (and (nil? (:leaf-a root))
       (nil? (:leaf-b root))))

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
  (let [possible-wall-positions (vec (take (- size 4) (map #(+ % 2) (range))))]
    (some #(= % (int (* size ratio))) possible-wall-positions)))

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

(defn trim-dimension-and-position
  [dimension original-position entrance-position randomizer]
  (let [ratio (randomizer "room-dimension-trim")
        trimmed-dimension (int (* dimension ratio))
        is-entrance-on-the-same-dimension-as-trim? (or
                                                     (= entrance-position (dec dimension))
                                                     (= entrance-position 0))
        entrance-should-be-repositioned? is-entrance-on-the-same-dimension-as-trim?
        contains-entrance? #(and
                             (not is-entrance-on-the-same-dimension-as-trim?)
                             (< % entrance-position)
                             (> (dec (+ % trimmed-dimension)) entrance-position))
        offset-can-be-used? (fn [offset]
                              (or (nil? entrance-position)
                                  (contains-entrance? offset)
                                  is-entrance-on-the-same-dimension-as-trim?))]
    (if (< trimmed-dimension MINIMUM_SIZE)
      {:dimension dimension
       :position  original-position
       :entrance  entrance-position}
      (let [free-space (- dimension trimmed-dimension)
            offset-possibilities (filter offset-can-be-used? (range (inc free-space)))
            ratio (randomizer "room-position-trim")
            random-offset-index (int (* ratio
                                        (count offset-possibilities)))
            offset (nth offset-possibilities random-offset-index)]
        (let [result {:dimension trimmed-dimension
                      :position  (+ original-position offset)
                      :entrance  entrance-position}
              fix-entrance #(if (= entrance-position (dec dimension))
                             (dec trimmed-dimension)
                             offset)]
          (if entrance-should-be-repositioned?
            (assoc result :entrance (fix-entrance))
            result))))))

(defn trim-room
  [room randomizer]
  (let [trimmed-width (trim-dimension-and-position
                        (:width room)
                        (:left room)
                        (:left (:entrance room))
                        randomizer)
        trimmed-height (trim-dimension-and-position
                         (:height room)
                         (:top room)
                         (:top (:entrance room))
                         randomizer)
        new-width (:dimension trimmed-width)
        new-left (:position trimmed-width)
        new-height (:dimension trimmed-height)
        new-top (:position trimmed-height)
        new-entrance {:left (:entrance trimmed-width)
                      :top  (:entrance trimmed-height)}]
    (let [new-room {:height new-height
                    :top    new-top
                    :width  new-width
                    :left   new-left}]
      (if (and (not (nil? (:left new-entrance)))
               (not (nil? (:top new-entrance))))
        (assoc new-room :entrance new-entrance)
        new-room))))

(defn link-sub-rooms
  [tree randomizer split-dimension]
  (let [room-a (:leaf-a tree)
        room-b (:leaf-b tree)
        ratio (randomizer "room-entrance")
        get-offset #(inc (int (* ratio (- % 2))))
        height-offset (get-offset (:height tree))
        width-offset (get-offset (:width tree))
        link-rooms #(let [room-a (assoc room-a :entrance %1)
                          room-b (assoc room-b :entrance %2)]
                     (assoc tree :leaf-a room-a :leaf-b room-b))]
    (if (= split-dimension WIDTH)
      (link-rooms {:top  height-offset
                   :left (dec (:width room-a))}
                  {:top  height-offset
                   :left 0})
      (link-rooms {:top  (dec (:height room-a))
                   :left width-offset}
                  {:top  0
                   :left width-offset}))))

(defn split-room
  [root randomizer]
  (let [height (:height root)
        width (:width root)
        top (:top root)
        left (:left root)
        ratio (randomizer "room-split")]
    (if (can-split-room? height width ratio)
      (let [split-dimension (get-split-dimension height width ratio randomizer)]
        (let [new-root (link-sub-rooms {:height height
                                        :width  width
                                        :top    top
                                        :left   left
                                        :leaf-a (split-room (get-leaf-a root split-dimension ratio) randomizer)
                                        :leaf-b (split-room (get-leaf-b root split-dimension ratio) randomizer)}
                                       randomizer
                                       split-dimension)]
          (if (and (not-has-leafs? (:leaf-a new-root))
                   (not-has-leafs? (:leaf-b new-root)))
            (assoc new-root
              :leaf-a (trim-room (:leaf-a new-root) randomizer)
              :leaf-b (trim-room (:leaf-b new-root) randomizer))
            new-root)))
      root)))

(defn generate-rooms-tree
  "Generates tree of rooms."
  ([height width]
   (generate-rooms-tree
     height
     width
     (fn [& _] (rand))))
  ([height width randomizer]
   (if (is-space-for-room? height width)
     (let [root {:height height
                 :width  width
                 :left   0
                 :top    0}]
       (let [new-root (split-room root randomizer)]
         (if (not-has-leafs? new-root)
           (trim-room new-root randomizer)
           new-root)))
     {})))