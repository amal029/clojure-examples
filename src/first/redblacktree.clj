;;; Red-black tree: purely functional data-structure
;;; You can modify it to have values if you so-please
;;; The key comparison uses the polymorphism of the dynamic clojure language!
;;; Adopted from haskell version at: http://www.cs.kent.ac.uk/people/staff/smk/redblack/Untyped.hs 
;;; Author: Avinash
;;; Tue Mar 25 17:36:41 NZDT 2014

(ns first.redblacktree
  (:gen-class))
(use 'com.phansen.clojure.adt.core)
(use '[clojure.core.match :only (match)])

(def-adt color
  (R r)
  (B b))

(def-adt rb-tree
  (E)
  (T c l v r))


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
         [[{:c {:r _} :l a :v x :r b} y {:c {:r _} :l c :v z :r d}]] (T (R 'r) (T (B 'b) a x b) y (T (B 'b) c z d))
         [[{:c {:r _} :l {:c {:r _} :l a :v x :r b} :v y :r c} z d]] (T (R 'r) (T (B 'b) a x b) y (T (B 'b) c z d))
         [[{:c {:r _} :l a :v x :r {:c {:r _} :l b :v y :r c}} z d]] (T (R 'r) (T (B 'b) a x b) y (T (B 'b) c z d))
         [[a x {:c {:r _} :l {:c {:r _} :l b :v y :r c} :v z :r d}]] (T (R 'r) (T (B 'b) a x b) y (T (B 'b) c z d))
         [[a x {:c {:r _} :l b :v y :r {:c {:r _} :l c :v z :r d}}]] (T (R 'r) (T (B 'b) a x b) y (T (B 'b) c z d))
         [[b c d]] (T (B 'b) b c d)
         ))

(defn rb-tree-insert [x s]
  (let [ins #(match [%]
                    [{}] (T (R 'r) (E) x (E))
                    [{:c c :l a :v y :r b}] (cond
                                             (< x y) (rb-tree-balance [(ins a) y b])
                                             (> x y) (rb-tree-balance [a y (ins b)])
                                             :else %
                                             ))]
    (match [(ins s)]
           [{:c _ :l a :v y :r b}] (T (B 'b) a y b))))

(defn- sub1 [tuple]
  (match [tuple]
         [{:c {:b _} :l a :v x :r b}] (T (R 'r) a x b)))


(defn- balleft [tuple]
  (match [tuple]
         [[{:c {:r _} :l a :v x :r b} y c]] (T (R 'r) (T (B 'b) a x b) y c)
         [[bl x {:c {:b _} :l a :v y :r b}]] (rb-tree-balance [bl x (T (R 'r) a y b)])
         [[bl x {:c {:r _} :l {:c {:b _} :l a :v y :r b} :v z :r c}]] (T (R 'r) (T (B 'b) bl x a) y (rb-tree-balance [b z (sub1 c)]))))

(defn- ballright [tuple]
  (match [tuple]
         [[a x {:c {:r _} :l b :v y :r c}]]  (T (R 'r) a x (T (B 'b) b y c))
         [[{:c {:b _} :l a :v x :r b} y bl]] (rb-tree-balance [(T (R 'r) a x b) y bl])
         [[{:c {:r _} :l a :v x :r {:c {:b _} :l b :v y :r c}} z bl]] (T (R 'r) (rb-tree-balance [(sub1 a) x b]) y (T (B 'b) c z bl))))

(defn- app [tuple]
  (match [tuple]
         [[{} a]] a
         [[a {}]] a
         [[{:c {:r _} :l a :v x :r b} {:c {:r _} :l c :v y :r d}]] (match [(app [b c])]
                                                                          [{:c {:r _} :l b' :v z :r c'}] (T (R 'r) (T (R 'r) a x b') z (T (R 'r) c' y d))
                                                                          [{:c c :l l :v v :r r}] (T (R 'r) a x (T (R 'r) (T c l v r) y d)))
         [[{:c {:b _} :l a :v x :r b} {:c {:b _} :l c :v y :r d}]] (match [(app [b c])]
                                                                          [{:c {:r _} :l b' :v z :r c'}] (T (R 'r) (T (B 'b) a x b') z (T (B 'b) c' y d))
                                                                          [{:c c :l l :v v :r r}] (balleft a x (T (B 'b) c' y d)))
         [[a {:c {:r _} :l b :v x :r c}]] (T (R 'r) (app [a b]) x c)
         [[{:c {:r _} :l a :v x :r b} c]] (T (R 'r) a x (app [b c]))))

(defn rb-tree-delete [x s]
  (let [del #(match [%]
                    [{}] (E)
                    [{:c c :l a :v y :r b}] (cond 
                                             (x < y) (match [a]
                                                            [{:c {:b _} :l _ :v _ :r r}] (balleft [(del a) y b])
                                                            [_] (T (R 'r) (del a) y b)
                                                            )
                                             (x > y) (match [b]
                                                            [{:c {:b _} :l _ :v _ :r r}] (ballright [a y (del b)])
                                                            [_] (T (R 'r) a y (del b))
                                                            )
                                             :else (app [a b])
                                             ))]
    (match [(del s)]
           [{:c _ :l a :v y :r b}] (T (B 'b) a y b))))
