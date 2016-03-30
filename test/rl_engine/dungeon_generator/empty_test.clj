(ns rl-engine.dungeon-generator.empty_test
  (:require [clojure.test :refer :all]
            [rl-engine.dungeon-generator.empty :refer :all]))

(defn height
  [dungeon]
  (count dungeon))

(defn width
  [dungeon]
  (count (first dungeon)))

(deftest zero-dungeon-size
  (testing "can generate 0 size dungeon"
    (is (= [] (generate-dungeon 0 0)))))

(deftest incorrect-dungeon-size
  (testing "returns empty vector when height is less then 0"
    (is (= [] (generate-dungeon -1 0))))
  (testing "returns empty vector when width is less then 0"
    (is (= [] (generate-dungeon 0 -1)))))

(deftest correct-dungeon-size
  (testing "returns vector with correct height"
    (is (= 7 (height (generate-dungeon 7 0)))))
  (testing "returns vector with correct width"
    (testing "all rows have the same with"
      (is (= 1 (count (set (map count (generate-dungeon 1 7)))))))
    (is (= 7 (width (generate-dungeon 1 7))))))

(deftest dungeon-has-outer-wall
  (testing "when dungeon size is less then 3x3 all cells have 0"
    (is (= [[0]] (generate-dungeon 1 1)))
    (is (= [[0,0],
            [0,0]] (generate-dungeon 2 2)))
    (is (= [[0,0,0],
            [0,0,0]] (generate-dungeon 2 3)))
    (is (= [[0,0],
            [0,0],
            [0,0]] (generate-dungeon 3 2))))
  (testing "dungeon floor have single wall along edges"
     (is (= [[1, 1, 1],
             [1, 0, 1],
             [1, 1, 1]] (generate-dungeon 3 3)))
     (is (= [[1, 1, 1, 1, 1],
             [1, 0, 0, 0, 1],
             [1, 1, 1, 1, 1]] (generate-dungeon 3 5)))
     (is (= [[1, 1, 1, 1, 1],
             [1, 0, 0, 0, 1],
             [1, 0, 0, 0, 1],
             [1, 0, 0, 0, 1],
             [1, 1, 1, 1, 1]] (generate-dungeon 5 5)))))