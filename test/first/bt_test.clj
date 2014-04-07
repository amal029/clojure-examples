(ns first.bt-test
  (:require [clojure.test :refer :all]
            [first.binarytree :refer :all]))

;;; This is some global tree to test on
(def abt (Tree 10 (Leaf 6) (Leaf 11)))

(deftest bt-search-test
  (testing "Test searching in a b-tree"
    (is (not (nil? (bt-search 6 abt))))))

(deftest bt-enumerate-test
  (testing "Convert btree to Coll"
    (is (= '(10 6 11) (bt-to-list abt)))))

(deftest bt-insert-test
  (testing "Inserting into a btree"
    (is (= {:rbt {:v 11}, :lbt {:rbt {:v 7}, :lbt nil, :v 6}, :v 10} (bt-insert abt 7)))))

(deftest bt-delete-test
  (testing "Deleting from a btree"
    (is (= {:rbt {:rbt nil, :lbt {:v 7}, :v 11}, :lbt {:v 4}, :v 6} (bt-delete (bt-insert (bt-insert abt 4) 7) 10)))))
