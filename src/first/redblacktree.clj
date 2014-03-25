;;; Need to check with units tests!!
(ns first.redblacktree
  (:gen-class))
(use 'com.phansen.clojure.adt.core)
(use '[clojure.core.match :only (match)])

(def-adt color
  (R r)
  (B b)
  )

(def-adt rb-tree
  (E)
  (T c l v r)
  )


(defn rb-tree-search [value rb-tree]
  (match [rb-tree]
         [{:c _ :v v :l l :r r}] (cond 
                                  (< value v) (rb-tree-search value l)
                                  (> value v) (rb-tree-search value l)
                                  :else true
                                  )
         [{}]  false
         ))

(defn rb-tree-balance [tuple]
  (match [tuple]
         [[{:b _} {:c {:r _} :l {:c {:r _} :l a :v x :r b} :v y :r c} z d]] (T (R 'r) (T (B 'b) a x b) y (T (B 'b) c z d))
         [[{:b _} {:c {:r _} :l a :v x :r {:c {:r _} :l b :v y :r c}} z d]] (T (R 'r) (T (B 'b) a x b) y (T (B 'b) c z d))
         [[{:b _} a x {:c {:r _} :l {:c {:r _} :l b :v y :r c} :v z :r d}]] (T (R 'r) (T (B 'b) a x b) y (T (B 'b) c z d))
         [[{:b _} a x {:c {:r _} :l b :v y :r {:c {:r _} :l c :v z :r d}}]] (T (R 'r) (T (B 'b) a x b) y (T (B 'b) c z d))
         [[a b c d]] (T a b c d)
         ))

(defn rb-tree-insert [x s]
  (let [ins #(match [%]
                    [{}] (T (R 'r) (E) x (E))
                    [{:c c :l a :v y :r b}] (cond
                                             (< x y) (rb-tree-balance [c (ins a) y b])
                                             (> x y) (rb-tree-balance [c a y (ins b)])
                                             :else %
                                             ))]
    (match [(ins s)]
           [{:c _ :l a :v y :r b}] (T (B 'b) a y b))))
