(ns clojure-ds-and-algs.trees.red-black)


(defn tree-node [val color]
  {:value val
   :color color
   :left-child nil
   :right-child nil})


(defn add-tree-node [node key val]
  "
   After implementing add-node-to-tree and then re-reading the old implementation
   of add-tree-node... realized that add-tree-node had superfluous logic.

  add-node-to-tree does the work of finding a nil :left-child or :right-child
  in which the new node value should be inserted.
  "
  (let [color (if (= key :root) :black :red)]
    (assoc node key (tree-node val color))))


(defn add-node-to-tree [node key val]
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
  (let [currently-visited-node (key node)
        node-val (:value currently-visited-node)
        add-nested (fn [node direction-key] (assoc node key (add-tree-node currently-visited-node direction-key val)))
        traverse (fn [node direction-key] (assoc node key (add-node-to-tree currently-visited-node direction-key val)))]
    (if (and (= key :root) (= currently-visited-node nil))
      (add-tree-node currently-visited-node :root val)
      (cond
      ; Conditions where we update the nil node with our new node
        (and (> node-val val) (= (:left-child currently-visited-node) nil)) (add-nested node :left-child)
        (and (< node-val val) (= (:right-child currently-visited-node) nil)) (add-nested node :right-child)
      ; Conditions where we continue traversing
        (> node-val val) (traverse node :left-child)
        (< node-val val) (traverse node :right-child)))))


; (defn create-tree [[x & xs] tree]
;   (if (= xs 0)
;     tree))

; Multi-method to facilitate default args
; (defn create-tree
;     ([x & xs] (add-tree-node x {}))
;     ([[x & xs] tree] (Integer/parseInt s base)))