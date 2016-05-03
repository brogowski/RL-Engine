(ns rl-engine.dungeon-generator.bsp.tree-test
  (:require [clojure.test :refer :all]
            [rl-engine.dungeon-generator.test-utils :refer :all]
            [rl-engine.dungeon-generator.bsp.tree :refer :all]))

;TODO: change to recur

(def empty-randomizer-definition
  {"room-entrance"       0.0
   "room-dimension-trim" 0.0
   "room-position-trim"  0.0
   "room-split"          0.0})

(defn remove-entrances
  [tree]
  (clojure.walk/postwalk #(if (map? %)
                           (dissoc % :entrance)
                           %) tree))

(defn get-randomizer
  [map default]
  (fn
    ([] default)
    ([key] (get map key))))

(defn get-split-only-randomizer
  [split-ratio default]
  (get-randomizer (assoc empty-randomizer-definition "room-split" split-ratio)
                  default))

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

(defn change-coordinates-from-relative-to-absolute
  [leaf-room root-room]
  {:height (:height leaf-room)
   :width  (:width leaf-room)
   :top    (+ (:top leaf-room) (:top root-room))
   :left   (+ (:left leaf-room) (:left root-room))
   :leaf-a (:leaf-a leaf-room)
   :leaf-b (:leaf-b leaf-room)})

(defn flatten-rooms
  [rooms-tree]
  (if (nil? rooms-tree)
    []
    (if (and (nil? (:leaf-a rooms-tree)) (nil? (:leaf-b rooms-tree)))
      rooms-tree
      (flatten [(flatten-rooms (change-coordinates-from-relative-to-absolute
                                 (:leaf-a rooms-tree)
                                 rooms-tree)),
                (flatten-rooms (change-coordinates-from-relative-to-absolute
                                 (:leaf-b rooms-tree)
                                 rooms-tree))]))))

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

(defn get-entrance
  [room]
  (let [entrance (:entrance room)]
    [(:top entrance),
     (:left entrance)]))

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
    (let [randomizer (get-split-only-randomizer 0.5 0.5)]
      ;1 1 1
      ;1 0 1
      ;1 1 1
      (is (= 1 (rooms-count (generate-rooms-tree 3 3 randomizer))))
      ;1 1 1 1
      ;1 0 0 1
      ;1 0 0 1
      ;1 1 1 1
      (is (= 1 (rooms-count (generate-rooms-tree 4 4 randomizer))))
      (testing "room has correct dimensions"
        (is (= 3 (:height (generate-rooms-tree 3 3 randomizer))))
        (is (= 3 (:width (generate-rooms-tree 3 3 randomizer)))))
      (testing "room has correct coordinates"
        (is (= 0 (:top (generate-rooms-tree 3 3 randomizer))))
        (is (= 0 (:left (generate-rooms-tree 3 3 randomizer)))))))
  ;1 1 1 1 1
  ;1 0 0 0 1
  ;1 1 1 1 1
  (testing "when space allows for two rooms"
    (testing "when randomizer do not allow for room split"
      (let [get-room-split-randomizer #(get-split-only-randomizer % 0.5)]
        (is (= 1 (rooms-count (generate-rooms-tree 3 5 (get-room-split-randomizer 0)))))
        (is (= 1 (rooms-count (generate-rooms-tree 3 5 (get-room-split-randomizer 0.32)))))
        (is (= 1 (rooms-count (generate-rooms-tree 3 5 (get-room-split-randomizer 0.67)))))
        (is (= 1 (rooms-count (generate-rooms-tree 3 5 (get-room-split-randomizer 1.0)))))
        (is (= 1 (rooms-count (generate-rooms-tree 3 6 (get-room-split-randomizer 0.32)))))
        (is (= 1 (rooms-count (generate-rooms-tree 3 6 (get-room-split-randomizer 0.67)))))))))

(deftest generates-tree-with-two-rooms
  (testing "when space allows for two rooms"
    (testing "when randomizer allows for room split")
    (let [randomizer (get-split-only-randomizer 0.5 0.5)]
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
      (let [randomizer (get-split-only-randomizer 0.5 0.5)
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
        (let [tree (generate-rooms-tree height width (get-split-only-randomizer 0.5 0.5))]
          (is (= [[5, 3], [5, 3]] ((first-two-rooms-dimensions tree))))))
      (testing "when randomizer returns less then 0.5 - split is done horizontally"
        (let [tree (generate-rooms-tree height width (get-split-only-randomizer 0.5 0.49))]
          (is (= [[3, 5], [3, 5]] ((first-two-rooms-dimensions tree)))))))))

(deftest room-trimming
  (testing "trimming below minimum room size is not permitted"
    (testing "when trim ratio produces too small room - room is not trimmed"
      (let [randomizer (get-randomizer (assoc empty-randomizer-definition
                                         "room-dimension-trim" 1/4
                                         "room-position-trim" 0.5)
                                       0.5)]
        (let [tree (generate-rooms-tree 4 3 randomizer)]
          (is (= {:height 4
                  :width  3
                  :top    0
                  :left   0}
                 tree))))))
  (testing "single room is trimmed"
    (testing "room can be trimmed and repositioned by width"
      (let [height 3
            width 4]
        ; 1 1 1 1
        ; 1 0 0 1
        ; 1 1 1 1
        ; =>
        ; 1 1 1 1
        ; 1 0 1 1
        ; 1 1 1 1
        (let [randomizer (get-randomizer (assoc empty-randomizer-definition
                                           "room-dimension-trim" 3/4
                                           "room-position-trim" 0.3)
                                         0.5)]
          (let [tree (generate-rooms-tree height width randomizer)]
            (is (= {:height 3
                    :width  3
                    :top    0
                    :left   0}
                   tree))))
        ; 1 1 1 1
        ; 1 0 0 1
        ; 1 1 1 1
        ; =>
        ; 1 1 1 1
        ; 1 1 0 1
        ; 1 1 1 1
        (let [randomizer (get-randomizer (assoc empty-randomizer-definition
                                           "room-dimension-trim" 3/4
                                           "room-position-trim" 0.6)
                                         0.5)]
          (let [tree (generate-rooms-tree height width randomizer)]
            (is (= {:height 3
                    :width  3
                    :top    0
                    :left   1}
                   tree))))))
    (testing "room can be trimmed and repositioned by height"
      (let [height 4
            width 3]
        ; 1 1 1
        ; 1 0 1
        ; 1 0 1
        ; 1 1 1
        ; =>
        ; 1 1 1
        ; 1 1 1
        ; 1 0 1
        ; 1 1 1
        (let [randomizer (get-randomizer (assoc empty-randomizer-definition
                                           "room-dimension-trim" 3/4
                                           "room-position-trim" 0.6)
                                         0.5)]
          (let [tree (generate-rooms-tree height width randomizer)]
            (is (= {:height 3
                    :width  3
                    :top    1
                    :left   0}
                   tree))))
        ; 1 1 1
        ; 1 0 1
        ; 1 0 1
        ; 1 1 1
        ; =>
        ; 1 1 1
        ; 1 0 1
        ; 1 1 1
        ; 1 1 1
        (let [randomizer (get-randomizer (assoc empty-randomizer-definition
                                           "room-dimension-trim" 3/4
                                           "room-position-trim" 0.3)
                                         0.5)]
          (let [tree (generate-rooms-tree height width randomizer)]
            (is (= {:height 3
                    :width  3
                    :top    0
                    :left   0}
                   tree)))))))
  (testing "when tree has more then one room"
    (testing "only deepest rooms (leafs) are trimmed"
      (let [height 3
            width 6]
        ; 1 1 1 1 1 1
        ; 1 0 0 0 0 1
        ; 1 1 1 1 1 1
        ;=>
        ; 1 1 1 1 1 1
        ; 1 0 0 1 0 1
        ; 1 1 1 1 1 1
        ;=>
        ; 1 1 1 1 1 1
        ; 1 0 1 1 0 1
        ; 1 1 1 1 1 1
        (let [randomizer (get-randomizer (assoc empty-randomizer-definition
                                           "room-split" 1/2
                                           "room-dimension-trim" 3/4
                                           "room-position-trim" 1/3)
                                         0.5)]
          (let [tree (remove-entrances (generate-rooms-tree height width randomizer))]
            (is (= {:height 3
                    :width  6
                    :top    0
                    :left   0
                    :leaf-a {:height 3
                             :width  3
                             :top    0
                             :left   0}
                    :leaf-b {:height 3
                             :width  3
                             :top    0
                             :left   3}}
                   tree))))))))

(deftest room-linking
  (let [randomizer (get-split-only-randomizer 0.5 0.5)
        build-entrance-randomizer #(get-randomizer
                                    (assoc empty-randomizer-definition
                                      "room-entrance" %
                                      "room-split" 0.5)
                                    0.5)]
    (testing "single room do not have entrance"
      (let [tree (generate-rooms-tree 4 4 randomizer)]
        (is (nil? (:entrance tree)))))
    (testing "both rooms have correct entrances"
      (let [get-entrances
            (fn [randomizer dimensions]
              (let [tree (generate-rooms-tree (:height dimensions) (:width dimensions) randomizer)
                    room-a (:leaf-a tree)
                    room-b (:leaf-b tree)]
                {:room-a (get-entrance room-a)
                 :room-b (get-entrance room-b)}))]
        (testing "when distance between rooms is 0"
          (testing "when only one entrance can be set"
            ; 1 1 1 1 1
            ; 1 0 X 0 1
            ; 1 1 1 1 1
            (is (= {:room-a [1, 2]
                    :room-b [1, 0]}
                   (get-entrances randomizer
                                  {:height 3
                                   :width  5}))))
          (testing "when two entrances can be set"
            ; 1 1 1 1 1
            ; 1 0 1 0 1
            ; 1 0 1 0 1
            ; 1 1 1 1 1
            (let [dimensions {:height 4
                              :width  5}]
              (testing "when randomizer returns less then 0.5"
                ; 1 1 1 1 1
                ; 1 0 X 0 1
                ; 1 0 1 0 1
                ; 1 1 1 1 1
                (is (= {:room-a [1, 2]
                        :room-b [1, 0]}
                       (get-entrances (build-entrance-randomizer 0.45)
                                      dimensions))))
              (testing "when randomizer returns more then 0.5"
                ; 1 1 1 1 1
                ; 1 0 1 0 1
                ; 1 0 X 0 1
                ; 1 1 1 1 1
                (is (= {:room-a [2, 2]
                        :room-b [2, 0]}
                       (get-entrances (build-entrance-randomizer 0.65)
                                      dimensions))))))
          (testing "when three entrances can be set"
            ; 1 1 1 1 1
            ; 1 0 1 0 1
            ; 1 0 1 0 1
            ; 1 0 1 0 1
            ; 1 1 1 1 1
            (let [dimensions {:height 5
                              :width  5}]
              (testing "when randomizer returns less then 0.33"
                ; 1 1 1 1 1
                ; 1 0 X 0 1
                ; 1 0 1 0 1
                ; 1 0 1 0 1
                ; 1 1 1 1 1
                (is (= {:room-a [1, 2]
                        :room-b [1, 0]}
                       (get-entrances (build-entrance-randomizer 0.32)
                                      dimensions))))
              (testing "when randomizer returns more then 0.33 and less then 0.66"
                ; 1 1 1 1 1
                ; 1 0 1 0 1
                ; 1 0 X 0 1
                ; 1 0 1 0 1
                ; 1 1 1 1 1
                (is (= {:room-a [2, 2]
                        :room-b [2, 0]}
                       (get-entrances (build-entrance-randomizer 0.50)
                                      dimensions))))
              (testing "when randomizer returns more then 0.66"
                ; 1 1 1 1 1
                ; 1 0 1 0 1
                ; 1 0 1 0 1
                ; 1 0 X 0 1
                ; 1 1 1 1 1
                (is (= {:room-a [3, 2]
                        :room-b [3, 0]}
                       (get-entrances (build-entrance-randomizer 0.67)
                                      dimensions)))))))
        (testing "when distance between rooms is 1"
          (testing "when only one entrance can be set"
            ; 1 1 1 1 1 1
            ; 1 0 X X 0 1
            ; 1 1 1 1 1 1
            (is (= {:room-a [1, 2]
                    :room-b [1, 0]}
                   (get-entrances (get-randomizer (assoc empty-randomizer-definition
                                                    "room-dimension-trim" 0.75
                                                    "room-split" 0.5)
                                                  0.5)
                                  {:height 3
                                   :width  6}))))
          (testing "when two entrances can be set"
            (testing "when randomizer returns less then 0.5"
              (let [get-room-randomizer
                    #(get-randomizer (assoc empty-randomizer-definition
                                       "room-dimension-trim" 0.75
                                       "room-split" 0.5
                                       "room-entrance" %)
                                     0.5)]
                ; 1 1 1 1 1 1
                ; 1 0 X X 0 1
                ; 1 0 0 0 0 1
                ; 1 1 1 1 1 1
                (is (= {:room-a [1, 2]
                        :room-b [1, 0]}
                       (get-entrances (get-room-randomizer 0.45)
                                      {:height 4
                                       :width  6})))
                (testing "when randomizer returns more then 0.5"
                  ; 1 1 1 1 1 1
                  ; 1 0 0 0 0 1
                  ; 1 0 X X 0 1
                  ; 1 1 1 1 1 1
                  (is (= {:room-a [2, 2]
                          :room-b [2, 0]}
                         (get-entrances (get-room-randomizer 0.65)
                                        {:height 4
                                         :width  6}))))))))))))