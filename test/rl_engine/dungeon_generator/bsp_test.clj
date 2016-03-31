(ns rl-engine.dungeon-generator.bsp-test
  (:require [clojure.test :refer :all]
            [rl-engine.dungeon-generator.test-utils :refer :all]
            [rl-engine.dungeon-generator.bsp :refer :all]))

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
    (is (= 7 (dungeon-height (generate-dungeon 7 0)))))
  (testing "returns vector with correct width"
    (testing "all rows have the same with"
      (is (= 1 (count (set (map count (generate-dungeon 1 7)))))))
    (is (= 7 (dungeon-width (generate-dungeon 1 7))))))