(ns rl-engine.dungeon-generator.bsp)

(def FLOOR 0)

(defn generate-dungeon
  "Generates new dungeon floor."
  [height width]
  (map (constantly
         (map
           (constantly FLOOR)
           (take width (range))))
       (take height (range))))
