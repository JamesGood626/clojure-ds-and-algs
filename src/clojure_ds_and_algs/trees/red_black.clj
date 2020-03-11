(ns clojure-ds-and-algs.trees.red-black
  (:require [clojure.core.match :refer [match]]))

(defn tree-node [val color]
  {:value val
   :color color
   :left-child nil
   :right-child nil})

(defn add-tree-node
  "
  After implementing add-node-to-tree and then re-reading the old implementation
  of add-tree-node... realized that add-tree-node had superfluous logic.

  add-node-to-tree does the work of finding a nil :left-child or :right-child
  in which the new node value should be inserted.
  "
  [node key val]
  (let [color (if (= key :root) :black :red)]
    (assoc node key (tree-node val color))))


(defn add-node-to-tree
  "
  node can be passed in as any of the following:
  1. The root of the tree, from which the root node will be accessed:
     {:root {:value 100 :color :black :left-child nil :right-child nil}
     in which case the key should be :root in order to access the map which
     contains the node's :value.

  2. A parent node, from which a child node will be accessed:
     {:value 100 :color :black
      :left-child {:value 80 :color :red :left-child nil :right-child nil}
      :right-child nil
     }
     in which case the key should be either :left-child or :right-child
     in order to access the map which contains the node's :value.

  Those two cases comprise the set of possible (valid) inputs to the function.

  val is the value which will be added to the tree as a new node.


  The structure of accomodating nested updates with assoc:
  (assoc tree :root
         (assoc (:root tree) :right-child
                (assoc (:right-child (:root tree)) :right-child 'WOOT')))

  {:root
   {:value 100, :color :black
    :left-child nil
    :right-child {:value 120, :color :red
                  :left-child nil, :right-child 'WOOT'}}}
  "
  [node key val]
  (let [currently-visited-node (key node)
        node-val (:value currently-visited-node)
        add-nested (fn [direction-key] (assoc node key (add-tree-node currently-visited-node direction-key val)))
        traverse (fn [direction-key] (assoc node key (add-node-to-tree currently-visited-node direction-key val)))]
    (if (and (= key :root) (= currently-visited-node nil))
      (add-tree-node currently-visited-node :root val)
      (cond
      ; Conditions where we update the nil node with our new node
        (and (> node-val val) (= (:left-child currently-visited-node) nil)) (add-nested :left-child)
        (and (< node-val val) (= (:right-child currently-visited-node) nil)) (add-nested :right-child)
      ; Conditions where we continue traversing
        (> node-val val) (traverse :left-child)
        (< node-val val) (traverse :right-child)))))

; The four configurations which violate the red-black tree invariants:
; 1. left-left (a black node has a red left node, which in turn has a red left node)
; 2. left-right (a black node has a red left node, which in turn has a red right node)
; 3. right-right (a black node has a red right node, which in turn has a red right node)
; 4. right-left (a black node has a red right node, which in turn has a red left node)

; TODO: use (match) for implementing the balance method
;; (let [x true
;;       y true
;;       z true]
;;   (match [x y z]
;;     [_ false true] 1
;;     [false true _] 2
;;     [_ _ false] 3
;;     [_ _ true] 4
;;     :else 5))
;=> 4

(defn left-left-balance
  "
  gp-node = grand-parent-node
  p-node = parent-node 
  c-node = child-node

  1. p-node will become the root node
     - if p-node's right-child isn't nil, then assign it to be
       gp-node's left child. Well.. even if it is nil, that will
       still end up being assigned as gp-node's left-child. Also,
       update gp-node's color to :red.
  2. Assign updated-gp-node to be p-node's right child
  "
  [{{c-node :left-child :as p-node} :left-child :as gp-node}]
  (let [updated-gp-node (->
                         (assoc gp-node :left-child (:right-child p-node))
                         (assoc :color :red))]
    ; TODO: Figure out how the coloring of the nodes being inserted is supposed to be..
    ; because the notes I have in my ipad on this matter are ambiguous.
    (println updated-gp-node)))

(defn balance
  "
   {:root node}
   will never end up being passed in as input from balance's usage in add-node-to-tree.
  "
  [tree]
  (match [tree]
    [{:color :black
      :left-child {:color :red
                   :left-child {:color :red}}}] (left-left-balance tree)
    :else "WTF"))

; (defn create-tree [[x & xs] tree]
;   (if (= xs 0)
;     tree))

; Multi-method to facilitate default args
; (defn create-tree
;     ([x & xs] (add-tree-node x {}))
;     ([[x & xs] tree] (Integer/parseInt s base)))