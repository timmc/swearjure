(ns org.timmc.swearjure-test
  (:use clojure.test)
  (:require [org.timmc.swearjure :as s]))

(deftest form-builders
  (is (= (eval s/ev-true) true))
  (is (= (eval s/ev-false) false))
  (is (= (eval s/ev-nil) nil))
  (is (= (map (comp eval s/wrap-boolean)
              [nil false true 1 [1 2]])
         [false false true true true])))

(deftest naming
  (is (= (s/scope-name '[foo bar baz]) "foo-bar-baz")))

(deftest literals
  (are [i o] (= (s/compile-number i) o)
       0 '(+)
       3 '(+ (*) (*) (*))
       -2 '(- (+ (*) (*)))))

(deftest combining-forms
  (testing "vecs"
    (is (= (s/compile-form '[top] [])
           {:body []
            :helpers {}
            :scope '[top]}))
    (is (= (s/compile-form '[top] '[:_ [1]])
           {:body '[:_ [(+ (*))]]
            :helpers {}
            :scope '[top]})))
  (testing "maps"
    (is (= (s/compile-form '[top] '{:_ [1]})
           {:body '{:_ [(+ (*))]}
            :helpers {}
            :scope '[top]}))))

(deftest convenience ;; or sanity
  (testing "inc and dec"
    (is (= (s/compile-form [] '[(inc (dec foo))])
           {:body '[(+ (- foo (*)) (*))]
            :helpers {}
            :scope []})))
  (testing "if"
    (is (= (with-redefs [s/unique #(symbol (str % "_17"))]
             (s/compile-form '[top] '(if [:test 0] [:then 1] [:else 2])))
           {:body (list :org.timmc.swearjure/call-helper
                        (list {s/ev-true 'top-ift_17}
                              (s/wrap-boolean '[:test (+)])
                              'top-iff_17))
            :helpers {'top-ift_17 '[:then (+ (*))]
                      'top-iff_17 '[:else (+ (*) (*))]}
            :scope '[top]}))))
