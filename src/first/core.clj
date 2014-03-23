(ns first.core
  (:gen-class))

(use 'com.phansen.clojure.adt.core)
(use '[clojure.core.match :only (match)])


(def-adt tree
  (L v)
  (T l r op))


(defn opt [ tree]
  (match [tree]
         [{:l l :r r :op op}] (op (opt l) (opt r))
         [{:v v}] v
         ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (do (print '(T (T (L 3.0) (T (L 78) (L 8) -) *) (L 4) +)) 
      (print " = ")
      (println (opt (T (T (L 3.0) (T (L 78) (L 8) -) *) (L 4) +)))))


