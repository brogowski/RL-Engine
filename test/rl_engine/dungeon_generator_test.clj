(ns rl-engine.dungeon-generator-test
  (:require [clojure.test :refer :all]
            [rl-engine.dungeon-generator :refer :all]
            [rl-engine.dungeon-generator.empty]
            [rl-engine.dungeon-generator.bsp]))

(def all-generators
  ["empty",
   "bsp"])

(deftest avaliable-generators
  (testing "returns list of all avaliable generators"
    (is (= (list-generators) all-generators))))

(deftest getting-generator
  (testing "getting generator by name"
    (is (= (get-generator "empty")
          rl-engine.dungeon-generator.empty/generate-dungeon))
    (is (= (get-generator "bsp")
           rl-engine.dungeon-generator.bsp/generate-dungeon))))