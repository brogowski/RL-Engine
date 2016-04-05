(ns rl-engine.dungeon-generator.empty)

(def FLOOR 0)
(def WALL 1)

(defn get-cell [current-height current-width max-height max-width]
  (cond
    (= 0 current-height) WALL
    (= 0 current-width) WALL
    (= (dec max-height) current-height) WALL
    (= (dec max-width) current-width) WALL
    :else FLOOR))

  (defn get-row [current-height max-height max-width]
    (map
      #(if (or (< max-height 3) (< max-width 3))
        FLOOR
        (get-cell current-height % max-height max-width))
      (take max-width (range))))

(defn generate-dungeon
  "Generates new dungeon floor."
  [height width]
  (map
    #(get-row % height width)
    (take height (range))))