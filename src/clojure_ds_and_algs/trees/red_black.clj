(ns clojure-ds-and-algs.trees.red-black)

(defn tree-node [val color]
  {:value val
   :color color
   :left-child nil
   :right-child nil})

(defn add-tree-node [key val tree]
  (let [node (key tree)
        node-value (:value node)
        color (if (= key :root) :black :red)
        add-left (fn [node] (assoc node :left-child (add-tree-node :left-child val node)))
        add-right (fn [node] (assoc node :right-child (add-tree-node :right-child val node)))]
    (if (= node nil)
      (assoc tree key (tree-node val color))
      ((cond
         (> node-value val) (assoc tree key (add-left node))
         (< node-value val) (assoc tree key (add-right node))
         :else (assoc tree key (tree-node val :red)))))))

(defn update-nested-node [node key val]
  (let [currently-visited-node (key node)
        node-val (:value currently-visited-node)
        add-nested (fn [node direction-key] (assoc node key (add-tree-node direction-key val (key node))))
        traverse (fn [node direction-key] (assoc node key (update-nested-node node direction-key val)))]
    (cond
      ; Conditions where we update the nil node with our new node
      (and (> node-val val) (= (:left-child node) nil)) (add-nested node :left-child)
      (and (< node-val val) (= (:right-child node) nil)) (add-nested node :right-child)
      ; Conditions where we continue traversing
      (> node-val val) (traverse node :left-child)
      (< node-val val) (traverse node :right-child))))

; (if (= (key node) nil)
;   (assoc node key (tree-node val :red))
;   (cond
;     (and (> (:value node) val) (= (:left-child node) nil)) (assoc (:left-child node) :left-child (update-nested-node :left-child val node))
;     (and (< (:value node) val) (= (:right-child node) nil)) (assoc (:right-child node) :right-child (update-nested-node :right-child val node))

;     (> (:value node) val) (assoc (:left-child node) key (update-nested-node :left-child val node))
;     (< (:value node) val) (assoc (:right-child node) key (update-nested-node :right-child val node))
;     :else (assoc node key (tree-node val :red))))

; (defn test-case [x]
;   (cond
;     (< x 10) (assoc {:root nil} :root "Is less than 10")
;     (and (> x 10) (< x 20)) (assoc {:root nil} :root "Is greater than 10 and less than 20")
;     (>= x 20) (assoc {:root nil} :root "Is greater than or equal to 20")))

; (defn create-tree [[x & xs] tree]
;   (if (= xs 0)
;     tree))

; Multi-method to facilitate default args
; (defn create-tree
;     ([x & xs] (add-tree-node x {}))
;     ([[x & xs] tree] (Integer/parseInt s base)))