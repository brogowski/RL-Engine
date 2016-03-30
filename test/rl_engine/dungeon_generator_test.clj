(ns rl-engine.dungeon-generator-test
  (:require [clojure.test :refer :all]
            [rl-engine.dungeon-generator :refer :all]))

(def all-generators
  ["empty"])

(deftest avaliable-generators
  (testing "returns list of all avaliable generators"
    (is (= (generators) all-generators))))