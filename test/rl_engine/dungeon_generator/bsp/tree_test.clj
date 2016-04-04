(ns rl-engine.dungeon-generator.bsp.tree-test
  (:require [clojure.test :refer :all]
            [rl-engine.dungeon-generator.test-utils :refer :all]
            [rl-engine.dungeon-generator.bsp.tree :refer :all]))

;TODO: change to recur

(defn rooms-count
  [rooms-tree]
  (if (nil? rooms-tree)
    0
    (if (and (nil? (:leaf-a rooms-tree)) (nil? (:leaf-b rooms-tree)))
      (if (= {} rooms-tree)
        0
        1)
      (+ (rooms-count (:leaf-a rooms-tree))
         (rooms-count (:leaf-b rooms-tree))))))

(defn flatten-rooms
  [rooms-tree]
  (if (nil? rooms-tree)
    []
    (if (and (nil? (:leaf-a rooms-tree)) (nil? (:leaf-b rooms-tree)))
      rooms-tree
      (flatten [(flatten-rooms (:leaf-a rooms-tree)),
                (flatten-rooms (:leaf-b rooms-tree))]))))

(defn flatten-rooms-dimensions
  [rooms-tree]
  (map
    #((constantly [(:height %), (:width %)]))
    (flatten-rooms rooms-tree)))

(defn flatten-rooms-coordinates
  [rooms-tree]
  (map
    #((constantly [(:top %), (:left %)]))
    (flatten-rooms rooms-tree)))

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
          (is (= [[3, 3], [3, 3]] (flatten-rooms-dimensions (generate-rooms-tree 3 5 randomizer))))
          ;1 1 1 1 1
          ;1 0 1 0 1
          ;1 1 1 1 1
          (is (= [[3, 3], [3, 3]] (flatten-rooms-dimensions (generate-rooms-tree 5 3 randomizer)))))
        (testing "even dimension"
          ; 1 1 1
          ; 1 0 1
          ; 1 0 1
          ; 1 1 1
          ; 1 0 1
          ; 1 1 1
          (is (= [[4, 3], [3, 3]] (flatten-rooms-dimensions (generate-rooms-tree 6 3 randomizer))))
          ;1 1 1 1 1 1
          ;1 0 0 1 0 1
          ;1 1 1 1 1 1
          (is (= [[3, 4], [3, 3]] (flatten-rooms-dimensions (generate-rooms-tree 3 6 randomizer))))))
      (testing "coordinates are correct"
        (testing "odd dimension"
          ;1 1 1
          ;1 0 1
          ;1 1 1
          ;1 0 1
          ;1 1 1
          (is (= [[0, 0], [2, 0]] (flatten-rooms-coordinates (generate-rooms-tree 5 3 randomizer))))
          ;1 1 1 1 1
          ;1 0 1 0 1
          ;1 1 1 1 1
          (is (= [[0, 0], [0, 2]] (flatten-rooms-coordinates (generate-rooms-tree 3 5 randomizer))))
          )
        (testing "even dimension"
          ; 1 1 1
          ; 1 0 1
          ; 1 0 1
          ; 1 1 1
          ; 1 0 1
          ; 1 1 1
          (is (= [[0, 0], [3, 0]] (flatten-rooms-coordinates (generate-rooms-tree 6 3 randomizer))))
          ;1 1 1 1 1 1
          ;1 0 0 1 0 1
          ;1 1 1 1 1 1
          (is (= [[0, 0], [0, 3]] (flatten-rooms-coordinates (generate-rooms-tree 3 6 randomizer)))))))))

(deftest generates-recursive-rooms
  (testing "when space allows for four rooms"
    (testing "when randomizer allows for room split"
      (let [randomizer (constantly 0.5)
            rooms-tree (generate-rooms-tree 6 6 randomizer)]
        ; 1 1 1 1 1 1
        ; 1 0 0 1 0 1
        ; 1 0 0 1 0 1
        ; 1 1 1 1 1 1
        ; 1 0 0 1 0 1
        ; 1 1 1 1 1 1
        (testing "room count is equal to 4"
          (is (= 4 (rooms-count rooms-tree))))
        (testing "dimensions are correct"
          (is (= [[4, 4], [3, 4], [4, 3], [3, 3]] (flatten-rooms-dimensions rooms-tree))))
        (testing "coordinates are correct"
          (is (= [[0, 0], [3, 0], [0, 3], [3, 3]] (flatten-rooms-coordinates rooms-tree))))))))

(deftest split-collisions
  (testing "when split can be done on both dimensions - randomizer decides"
    ; 1 1 1 1 1
    ; 1 0 0 0 1
    ; 1 0 0 0 1
    ; 1 0 0 0 1
    ; 1 1 1 1 1
    (let [height 5
          width 5
          first-two-rooms-dimensions #(constantly [[(:height (:leaf-a %)), (:width (:leaf-a %))],
                                                   [(:height (:leaf-b %)), (:width (:leaf-b %))]])]
      (testing "when randomizer returns 0.5 or greater - split is done verticaly"
        (let [tree (generate-rooms-tree height width (constantly 0.5))]
          (is (= [[5, 3], [5, 3]] ((first-two-rooms-dimensions tree))))))
      (testing "when randomizer returns less then 0.5 - split is done horizontally"
        (let [tree (generate-rooms-tree height width (constantly 0.49))]
          (is (= [[3, 5], [3, 5]] ((first-two-rooms-dimensions tree)))))))))