;;; This is a binary tree implementation
;;; The match macro in clojure sucks big balls.
;;; Be careful to always match the longest sequence first, else you will get wrng matches!
;;; This is because at least the match macro I am using does a cond!
;;; Compared to ocaml, clojure does not autobind the next value to the same name, be careful!

(ns first.binarytree
  (:gen-class))
(use 'com.phansen.clojure.adt.core)
(use '[clojure.core.match :only (match)])


(def-adt bt
  (Leaf v)
  (Tree v lbt rbt)
  )

(defn bt-search [value bt]
  (if (nil? bt) (do (println "binary tree is nil!") bt)
      (match [bt]
             [{:v v :lbt lbt :rbt rbt}] (cond
                                         (= v value) bt
                                         (< value v) (bt-search value lbt)
                                         :else (bt-search value rbt))
             [{:v v}] (cond 
                       (= v value) bt
                       :else nil))))

(defn bt-insert [bt value]
  (if (nil? bt) (Leaf value)
      (match [bt]
             [{:v v :lbt l :rbt r}] (cond 
                                     (< value v) (Tree v (bt-insert l value) r)
                                     (> value v) (Tree v l (bt-insert r value))
                                     :else bt
                                     )
             [{:v v}] (cond 
                       (< value v) (Tree v (bt-insert nil value) nil)
                       (> value v) (Tree v nil (bt-insert nil value))
                       :else bt
                       ))))


(defn bt-to-list [bt]
  (if (nil? bt) []
      (match [bt]
             [{:v v :lbt l :rbt r}] (concat [v] (bt-to-list l) (bt-to-list r))
             [{:v v}] [v])))

(defn bt-delete [bt value]
  (if (nil? bt) bt
      (match [bt]
             [{:v v :lbt l :rbt r}] (cond 
                                     (= value v) (match [l]
                                                        [{:v vi :lbt li :rbt ri}] (cond
                                                                                   (nil? ri) (Tree vi li r)
                                                                                   :else 
                                                                                   (let [cbt (atom r)]
                                                                                     (println "before entering map")
                                                                                     (println @cbt)
                                                                                     (println (count (bt-to-list ri)))
                                                                                     (map 
                                                                                      #(swap! cbt bt-insert @cbt %)
                                                                                      (do (println (bt-to-list ri)) (bt-to-list ri)))
                                                                                     (Tree vi li @cbt)))
                                                        [{:v vi}] (Tree vi nil r)
                                                        )
                                     (> value v) (Tree v l (bt-delete r value))
                                     :else (Tree v (bt-delete l value) r))
             [{:v v}] (if (= value v) nil bt))))

(def abt (Tree 10 (Leaf 6) (Leaf 11)))
(bt-search 6 first.binarytree/abt)
abt
(map #(+ 2 %) (bt-to-list abt))
(bt-delete (bt-insert (bt-insert abt 4) 7) 10)
