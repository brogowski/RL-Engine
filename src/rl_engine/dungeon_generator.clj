(ns rl-engine.dungeon-generator
  (:require [rl-engine.dungeon-generator.empty]
            [rl-engine.dungeon-generator.bsp]))

(def generators
  {"empty" rl-engine.dungeon-generator.empty/generate-dungeon
   "bsp"   rl-engine.dungeon-generator.bsp/generate-dungeon})

(defn list-generators
  "Returns vector with names of all avaliable dungeon generators."
  []
  (keys generators))

(defn get-generator
  "Returns generator with given name."
  [name]
  (get generators name))