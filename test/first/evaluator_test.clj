(ns first.evaluator-test
  (:require [clojure.test :refer :all]
            [first.evaluator :refer :all]))

(deftest a-test
  (testing "Testing expression evaluation"
    (is (= 214.0 (opt (T (T (L 3.0) (T (L 78) (L 8) -) *) (L 4) +))))))
