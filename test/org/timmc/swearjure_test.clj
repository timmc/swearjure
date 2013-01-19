(ns org.timmc.swearjure-test
  (:refer-clojure :exclude (compile))
  (:use clojure.test
        org.timmc.swearjure))

(deftest form-builders
  (is (= (eval ev-true) true))
  (is (= (eval ev-false) false))
  (is (= (eval ev-nil) nil))
  (is (= (map (comp eval wrap-boolean)
              [nil false true 1 [1 2]])
         [false false true true true])))

(deftest naming
  (is (= (scope-name '[foo bar baz]) "foo-bar-baz")))

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

(deftest convenience ;; or sanity
  (testing "inc and dec"
    (is (= (compile-form [] '[(inc (dec foo))])
           {:body '[(+ (- foo (*)) (*))]
            :helpers {}
            :scope []})))
  (testing "if"
    (is (= (with-redefs [unique #(symbol (str % "_17"))]
             (compile-form '[top] '(if [:test 0] [:then 1] [:else 2])))
           {:body (list :org.timmc.swearjure/call-helper
                        (list {ev-true 'top-ift_17}
                              (wrap-boolean '[:test (+)])
                              'top-iff_17))
            :helpers {'top-ift_17 '[:then (+ (*))]
                      'top-iff_17 '[:else (+ (*) (*))]}
            :scope '[top]}))))
