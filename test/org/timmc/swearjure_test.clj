(ns org.timmc.swearjure-test
  (:refer-clojure :exclude (compile))
  (:use clojure.test
        org.timmc.swearjure))

(deftest naming
  (is (= (scope-name* '[foo bar baz]) "foo-bar-baz")))

(deftest literals
  (are [i o] (= (compile-number i) o)
       0 '(+)
       3 '(+ (*) (*) (*))
       -2 '(- (+ (*) (*)))))

(deftest combining-forms
  (testing "vecs"
    (is (= (compile-form '[top] [])
           {:body []
            :helpers {}
            :scope '[top]}))
    (is (= (compile-form '[top] '[:_ [1]])
           {:body '[:_ [(+ (*))]]
            :helpers {}
            :scope '[top]})))
  (testing "maps"
    (is (= (compile-form '[top] '{:_ [1]})
           {:body '{:_ [(+ (*))]}
            :helpers {}
            :scope '[top]}))))

(deftest convenience
  (testing "inc and dec"
    (is (= (compile-form [] '[(inc (dec foo))])
           {:body '[(+ (- foo (*)) (*))]
            :helpers {}
            :scope []}))))
