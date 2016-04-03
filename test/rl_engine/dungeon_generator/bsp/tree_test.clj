(ns rl-engine.dungeon-generator.bsp.tree-test
  (:require [clojure.test :refer :all]
            [rl-engine.dungeon-generator.test-utils :refer :all]
            [rl-engine.dungeon-generator.bsp.tree :refer :all]))

;TODO: change utils to recur

(defn rooms-count
  [rooms-tree]
  (if (nil? rooms-tree)
    0
    (if (and (nil? (:leaf-a rooms-tree)) (nil? (:leaf-b rooms-tree)))
      1
      (+ (rooms-count (:leaf-a rooms-tree))
         (rooms-count (:leaf-b rooms-tree))))))

(defn flatten-rooms
  [rooms-tree]
  (if (nil? rooms-tree)
    []
    (if (and (nil? (:leaf-a rooms-tree)) (nil? (:leaf-b rooms-tree)))
      [(:height rooms-tree), (:width rooms-tree)]
      [(flatten-rooms (:leaf-a rooms-tree)),
       (flatten-rooms (:leaf-b rooms-tree))])))

(deftest generates-empty-tree
  (testing "when size is invalid returns empty tree"
    (is (= 0 (rooms-count (generate-rooms-tree -1 1))))
    (is (= 0 (rooms-count (generate-rooms-tree 1 -1)))))
  (testing "when space is too small for even one room"
    (is (= 0 (rooms-count (generate-rooms-tree 0 0))))
    (is (= 0 (rooms-count (generate-rooms-tree 1 1))))
    (is (= 0 (rooms-count (generate-rooms-tree 2 2))))
    (is (= 0 (rooms-count (generate-rooms-tree 3 2))))
    (is (= 0 (rooms-count (generate-rooms-tree 2 3))))))

(deftest generates-tree-with-single-room
  (testing "when space allows for only one room"
    ;1 1 1
    ;1 0 1
    ;1 1 1
    (is (= 1 (rooms-count (generate-rooms-tree 3 3))))
    ;1 1 1 1
    ;1 0 0 1
    ;1 0 0 1
    ;1 1 1 1
    (is (= 1 (rooms-count (generate-rooms-tree 4 4 (constantly 0.5)))))
    (testing "room has correct dimensions"
      (is (= 3 (:height (generate-rooms-tree 3 3))))
      (is (= 3 (:width (generate-rooms-tree 3 3)))))
    (testing "room has correct coordinates"
      (is (= 0 (:top (generate-rooms-tree 3 3))))
      (is (= 0 (:left (generate-rooms-tree 3 3))))))
  ;1 1 1 1 1
  ;1 0 0 0 1
  ;1 1 1 1 1
  (testing "when space allows for two rooms"
    (testing "when randomizer do not allow for room split"
      (is (= 1 (rooms-count (generate-rooms-tree 3 5 (constantly 0)))))
      (is (= 1 (rooms-count (generate-rooms-tree 3 5 (constantly 0.32)))))
      (is (= 1 (rooms-count (generate-rooms-tree 3 5 (constantly 0.67)))))
      (is (= 1 (rooms-count (generate-rooms-tree 3 5 (constantly 1.0))))))))

(deftest generates-tree-with-two-rooms
  (testing "when space allows for two rooms"
    (testing "when randomizer allows for room split")
    (let [randomizer (constantly 0.5)]
      (testing "room number is 2"
        ;1 1 1
        ;1 0 1
        ;1 1 1
        ;1 0 1
        ;1 1 1
        (is (= 2 (rooms-count (generate-rooms-tree 3 5 randomizer))))
        ;1 1 1 1 1
        ;1 0 1 0 1
        ;1 1 1 1 1
        (is (= 2 (rooms-count (generate-rooms-tree 5 3 randomizer)))))
      (testing "dimensions are correct"
        (testing "odd dimension"
          ;1 1 1
          ;1 0 1
          ;1 1 1
          ;1 0 1
          ;1 1 1
          (is (= [[3, 3], [3, 3]] (flatten-rooms (generate-rooms-tree 3 5 randomizer))))
          ;1 1 1 1 1
          ;1 0 1 0 1
          ;1 1 1 1 1
          (is (= [[3, 3], [3, 3]] (flatten-rooms (generate-rooms-tree 5 3 randomizer)))))
        (testing "even dimension"
          ; 1 1 1
          ; 1 0 1
          ; 1 0 1
          ; 1 1 1
          ; 1 0 1
          ; 1 1 1
          (is (= [[4, 3], [3, 3]] (flatten-rooms (generate-rooms-tree 6 3 randomizer))))
          ;1 1 1 1 1 1
          ;1 0 0 1 0 1
          ;1 1 1 1 1 1
          (is (= [[3, 4], [3, 3]] (flatten-rooms (generate-rooms-tree 3 6 randomizer))))))
      (testing "coordinates are correct"
        (is false)))))