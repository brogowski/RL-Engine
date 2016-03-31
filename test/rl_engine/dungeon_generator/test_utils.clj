(ns rl-engine.dungeon-generator.test-utils)

(defn dungeon-height
  [dungeon]
  (count dungeon))

(defn dungeon-width
  [dungeon]
  (count (first dungeon)))