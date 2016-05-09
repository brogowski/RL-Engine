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

(deftest correctly-renders-tree
  (testing "empty tree"
    (is (= [[0 0]
            [0 0]]
           (generate-dungeon 2 2 (constantly {}))))
    (is (= [[0]]
           (generate-dungeon 1 1 (constantly {})))))
  (testing "one room tree"
    (is (= [[1 1 1]
            [1 0 1]
            [1 1 1]]
           (generate-dungeon 3 3 (constantly
                                   {:height 3
                                    :width  3
                                    :left   0
                                    :top    0}))))
    (is (= [[1 1 1 1]
            [1 0 0 1]
            [1 0 0 1]
            [1 1 1 1]]
           (generate-dungeon 4 4 (constantly
                                   {:height 4
                                    :width  4
                                    :left   0
                                    :top    0}))))
    (testing "two room tree"
      (is (= [[1 1 1 1 1]
              [1 0 1 0 1]
              [1 0 1 0 1]
              [1 1 1 1 1]]
             (generate-dungeon 4 5 (constantly {:height 4
                                                :width  5
                                                :left   0
                                                :top    0
                                                :leaf-a {:height 4
                                                         :width  3
                                                         :left   0
                                                         :top    0}
                                                :leaf-b {:height 4
                                                         :width  3
                                                         :left   2
                                                         :top    0}}))))
      (is (= [[1 1 1]
              [1 0 1]
              [1 1 1]
              [1 0 1]
              [1 1 1]]
             (generate-dungeon 5 3 (constantly {:height 5
                                                :width  3
                                                :left   0
                                                :top    0
                                                :leaf-a {:height 3
                                                         :width  3
                                                         :left   0
                                                         :top    0}
                                                :leaf-b {:height 3
                                                         :width  3
                                                         :left   0
                                                         :top    2}})))))
    (testing "four room tree"
      (is (= [[1 1 1 1 1]
              [1 0 1 0 1]
              [1 1 1 1 1]
              [1 0 1 0 1]
              [1 1 1 1 1]]
             (generate-dungeon 5 5 (constantly {:height 5
                                                :width  5
                                                :left   0
                                                :top    0
                                                :leaf-a {:height 5
                                                         :width  3
                                                         :left   0
                                                         :top    0
                                                         :leaf-a {:height 3
                                                                  :width  3
                                                                  :left   0
                                                                  :top    0}
                                                         :leaf-b {:height 3
                                                                  :width  3
                                                                  :left   0
                                                                  :top    2}}
                                                :leaf-b {:height 5
                                                         :width  3
                                                         :left   2
                                                         :top    0
                                                         :leaf-a {:height 3
                                                                  :width  3
                                                                  :left   0
                                                                  :top    0}
                                                         :leaf-b {:height 3
                                                                  :width  3
                                                                  :left   0
                                                                  :top    2}}})))))))

(deftest correctly-renders-horizontal-entrances
  (testing "when both entrances are at the same spot"
    (is (= [[1 1 1 1 1]
            [1 0 2 0 1]
            [1 1 1 1 1]]
           (generate-dungeon 3 5 (constantly
                                   {:height 3
                                    :width  5
                                    :left   0
                                    :top    0
                                    :leaf-a {:height 3
                                             :width  3
                                             :left   0
                                             :top    0
                                             :entrance {:left 2
                                                        :top  1}}
                                    :leaf-b {:height 3
                                             :width  3
                                             :left   2
                                             :top    0
                                             :entrance {:left 0
                                                        :top  1}}})))))
  (testing "when entrances have distance 1"
    (is (= [[1 1 1 1 1 1]
            [1 0 2 2 0 1]
            [1 1 1 1 1 1]]
           (generate-dungeon 3 6 (constantly
                                   {:height 3
                                    :width  6
                                    :left   0
                                    :top    0
                                    :leaf-a {:height 3
                                             :width  3
                                             :left   0
                                             :top    0
                                             :entrance {:left 2
                                                        :top  1}}
                                    :leaf-b {:height 3
                                             :width  3
                                             :left   3
                                             :top    0
                                             :entrance {:left 0
                                                        :top  1}}})))))
  (testing "when entrances have distance 2"
    (is (= [[1 1 1 1 1 1 1]
            [1 0 1 1 1 0 1]
            [1 0 2 0 2 0 1]
            [1 0 1 1 1 0 1]
            [1 1 1 1 1 1 1]]
           (generate-dungeon 5 7 (constantly
                                   {:height 5
                                    :width  7
                                    :left   0
                                    :top    0
                                    :leaf-a {:height 5
                                             :width  3
                                             :left   0
                                             :top    0
                                             :entrance {:left 2
                                                        :top  2}}
                                    :leaf-b {:height 5
                                             :width  3
                                             :left   4
                                             :top    0
                                             :entrance {:left 0
                                                        :top  2}}})))))
  (testing "when entrances have distance 3"
    (is (= [[1 1 1 1 1 1 1 1]
            [1 0 1 1 1 1 0 1]
            [1 0 2 0 0 2 0 1]
            [1 0 1 1 1 1 0 1]
            [1 1 1 1 1 1 1 1]]
           (generate-dungeon 5 8 (constantly
                                   {:height 5
                                    :width  8
                                    :left   0
                                    :top    0
                                    :leaf-a {:height 5
                                             :width  3
                                             :left   0
                                             :top    0
                                             :entrance {:left 2
                                                        :top  2}}
                                    :leaf-b {:height 5
                                             :width  3
                                             :left   5
                                             :top    0
                                             :entrance {:left 0
                                                        :top  2}}}))))))

(deftest correctly-renders-vertical-entrances
  (testing "when both entrances are at the same spot"
    (is (= [[1 1 1]
            [1 0 1]
            [1 2 1]
            [1 0 1]
            [1 1 1]]
           (generate-dungeon 5 3 (constantly
                                   {:height 5
                                    :width  3
                                    :left   0
                                    :top    0
                                    :leaf-a {:height 3
                                             :width  3
                                             :left   0
                                             :top    0
                                             :entrance {:left 1
                                                        :top  2}}
                                    :leaf-b {:height 3
                                             :width  3
                                             :left   0
                                             :top    2
                                             :entrance {:left 1
                                                        :top  0}}})))))
  (testing "when entrances have distance 1"
    (is (= [[1 1 1]
            [1 0 1]
            [1 2 1]
            [1 2 1]
            [1 0 1]
            [1 1 1]]
           (generate-dungeon 6 3 (constantly
                                   {:height 6
                                    :width  3
                                    :left   0
                                    :top    0
                                    :leaf-a {:height 3
                                             :width  3
                                             :left   0
                                             :top    0
                                             :entrance {:left 1
                                                        :top  2}}
                                    :leaf-b {:height 3
                                             :width  3
                                             :left   0
                                             :top    3
                                             :entrance {:left 1
                                                        :top  0}}})))))
  (testing "when entrances have distance 2"
    (is (= [[1 1 1 1 1]
            [1 0 0 0 1]
            [1 1 2 1 1]
            [1 1 0 1 1]
            [1 1 2 1 1]
            [1 0 0 0 1]
            [1 1 1 1 1]]
           (generate-dungeon 7 5 (constantly
                                   {:height 7
                                    :width  5
                                    :left   0
                                    :top    0
                                    :leaf-a {:height 3
                                             :width  5
                                             :left   0
                                             :top    0
                                             :entrance {:left 2
                                                        :top  2}}
                                    :leaf-b {:height 3
                                             :width  5
                                             :left   0
                                             :top    4
                                             :entrance {:left 2
                                                        :top  0}}}))))))

(deftest correctly-renders-multiple-entrances
  (testing "when multiple entrance levels exist"
    (is (= [[1 1 1 1 1 1 1 1]
            [1 0 0 1 1 1 0 1]
            [1 0 0 2 0 2 0 1]
            [1 2 1 1 1 1 0 1]
            [1 2 1 1 1 1 1 1]
            [1 0 1 1 1 1 1 1]
            [1 1 1 1 1 1 1 1]]
           (generate-dungeon 7 8 (constantly
                                   {:height 7
                                    :width  8
                                    :left   0
                                    :top    0
                                    :leaf-a {:height 7
                                             :width  4
                                             :left   0
                                             :top    0
                                             :entrance {:left 3
                                                        :top  2}
                                             :leaf-a {:height 4
                                                      :width  4
                                                      :left   0
                                                      :top    0
                                                      :entrance {:left 1
                                                                 :top  3}}
                                             :leaf-b {:height 3
                                                      :width  3
                                                      :left   0
                                                      :top    4
                                                      :entrance {:left 1
                                                                 :top  0}}}
                                    :leaf-b {:height 5
                                             :width  3
                                             :left   5
                                             :top    0
                                             :entrance {:left 0
                                                        :top  2}}}))))))