(ns clojure-ds-and-algs.trees.red-black-test
  (:require [clojure.test :refer :all]
            [clojure-ds-and-algs.trees.red-black :as rb]))

(deftest create-tree-node-test
  (testing "constructs a simple tree node"
    (is (= (rb/tree-node 100 :black) {:value 100 :color :black :left-child nil :right-child nil}))))

(deftest insert-root-if-tree-empty
  (testing "add-tree-node inserts a root node if tree passed in is empty"
    (is (= (rb/add-tree-node :root 100 {:root nil}) {:root {:value 100 :color :black :left-child nil :right-child nil}}))))

; (deftest insert-root-if-tree-node-present
;   (testing "add-tree-node inserts a node if root is already present"
;     (let [tree (rb/add-tree-node :root 100 {:root nil})]
;       (is (= (rb/add-tree-node :root 200 tree) {:root {:value 200 :color :black}})))))

(deftest nested-update-right
  (testing "update-nested-node adds a node with value greater than the root's value as the root's right-child"
    (let [tree {:root {:value 100 :color :black :left-child nil :right-child nil}}
          new-node {:value 120 :color :red :left-child nil :right-child nil}
          expected-tree {:root {:value 100 :color :black :left-child nil :right-child new-node}}]
      (is (= (rb/update-nested-node tree :root 120) expected-tree)))))

(deftest nested-update-left
  (testing "update-nested-node adds a node with value less than the root's value as the root's left-child"
    (let [tree {:root {:value 100 :color :black :left-child nil :right-child nil}}
          new-node {:value 80 :color :red :left-child nil :right-child nil}
          expected-tree {:root {:value 100 :color :black :left-child new-node :right-child nil}}]
      (is (= (rb/update-nested-node tree :root 80) expected-tree)))))