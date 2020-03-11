(ns clojure-ds-and-algs.trees.red-black-test
  (:require [clojure.test :refer :all]
            [clojure-ds-and-algs.trees.red-black :as rb]))

(deftest create-tree-node-test
  (testing "constructs a simple tree node"
    (is (= (rb/tree-node 100 :black) {:value 100 :color :black :left-child nil :right-child nil}))))

(deftest insert-root-if-tree-empty
  (testing "add-tree-node inserts a root node if tree passed in is empty"
    (let [tree {:root nil}
          key :root
          val 100]
      (is (= (rb/add-tree-node tree key val) {:root {:value 100 :color :black :left-child nil :right-child nil}})))))

(deftest add-node-to-tree-nested
  (testing "add-node-to-tree adds a node to a tree w/ nested nodes, returning only the subtree which the newly
            added node was inserted into"
    (let [right-node {:value 110 :color :red :left-child nil :right-child nil}
          left-node {:value 90 :color :red :left-child nil :right-child nil}
          tree {:root {:value 100 :color :black :left-child left-node :right-child right-node}}
          new-val 120
          new-node {:value 120 :color :red :left-child nil :right-child nil}
          nested-node-to-update (:right-child (:root tree))
          expected-tree {:value 110, :color :red, :left-child nil, :right-child new-node}]
      (is (= (rb/add-tree-node nested-node-to-update :right-child new-val) expected-tree)))))

(deftest add-node-to-tree-root
  (testing "add-node-to-tree adds a node to an empty tree as the root node"
    (let [tree {:root nil}
          new-val 120
          new-node {:value 120 :color :black :left-child nil :right-child nil}
          expected-tree {:root new-node}]
      (is (= (rb/add-node-to-tree tree :root 120) expected-tree)))))

(deftest add-node-to-tree-right
  (testing "add-node-to-tree adds a node with value greater than the root's value as the root's right-child"
    (let [tree {:root {:value 100 :color :black :left-child nil :right-child nil}}
          new-node {:value 120 :color :red :left-child nil :right-child nil}
          expected-tree {:root {:value 100 :color :black :left-child nil :right-child new-node}}]
      (is (= (rb/add-node-to-tree tree :root 120) expected-tree)))))

(deftest add-node-to-tree-left
  (testing "add-node-to-tree adds a node with value less than the root's value as the root's left-child"
    (let [tree {:root {:value 100 :color :black :left-child nil :right-child nil}}
          new-node {:value 80 :color :red :left-child nil :right-child nil}
          expected-tree {:root {:value 100 :color :black :left-child new-node :right-child nil}}]
      (is (= (rb/add-node-to-tree tree :root 80) expected-tree)))))

(deftest add-node-to-tree-subsequent-entries
  (testing "add-node-to-tree adds multiple nodes to the tree"
    (let [tree {:root {:value 100 :color :black :left-child nil :right-child nil}}
          first-new-node {:value 80 :color :red :left-child nil :right-child nil}
          second-new-node {:value 70 :color :red :left-child nil :right-child nil}
          expected-tree {:root {:value 100 :color :black :left-child first-new-node :right-child nil}}
          tree-with-one-node (rb/add-node-to-tree tree :root 80)
          tree-with-two-nodes (rb/add-node-to-tree tree-with-one-node :root 70)]
      (is (= (rb/add-node-to-tree tree-with-two-nodes :root 60) {:root {:value 100, :color :black, :left-child {:value 80, :color :red, :left-child {:value 70, :color :red, :left-child {:value 60, :color :red, :left-child nil, :right-child nil}, :right-child nil}, :right-child nil}, :right-child nil}})))))



;; (deftest maintain-red-black-invariants
;;   (testing "starting to implement red-black tree invariant adherance"
;;     (let [nodes-to-add [10 20 30 40 50 60 70 80 90 100]
;;           work-fn (fn [acc val]
;;                     ;; (println "acc is")
;;                     ;; (println acc)
;;                     (rb/add-node-to-tree acc :root val))
;;           tree (reduce work-fn {:root nil} nodes-to-add)]
;;       (is (= tree "WHAAT")))))

(deftest left-left-balance-test
  (testing "This shiz betta work"
    (let [violation-node {:value 80 :color :red :left-child nil :right-child nil}
          node {:value 90 :color :red :left-child violation-node :right-child nil}
          tree {:root {:value 100 :color :black :left-child node :right-child nil}}]
      (is (= (rb/left-left-balance (:root tree)) "WUHH")))))

;; (deftest balance-test
;;   (testing "This shiz betta work"
;;     (let [violation-node {:value 80 :color :red :left-child nil :right-child nil}
;;           node {:value 90 :color :red :left-child violation-node :right-child nil}
;;           tree {:root {:value 100 :color :black :left-child node :right-child nil}}]
;;       (is (= (rb/balance (:root tree)) "WUHH")))))