(ns org.timmc.swearjure
  (:refer-clojure :exclude (compile)))

;; Current approach:
;; - A Swearjure program is specified as a quoted letfn block that calls one
;;   of its functions. The block should be eval-able as regular Clojure.
;; - The compiler walks into each function and splits it into a main body
;;   along with zero or more helper functions. (Currently only used for
;;   delaying evaluation in if= blocks.
;; - Helper fns are identified by gensyms.
;; - A second pass will put all the original and helper fns into a vector
;;   and walk into the fn bodies to convert symbol-based calls such as
;;   (main 1 2) into vector calls such as ((% 5) % 1 2).
;; - Unsolved problem: Representing and outputting reader sugar.

;;;; Utilities

(defn transpose
  [xs]
  (if (empty? xs)
    []
    (apply map vector xs)))

;;;; Form-builders

(def ev-true '(= :*))
(def ev-false '(= :+ :-))
(def ev-nil '({} :*))
(defn wrap-boolean
  [form]
  (list {ev-false ev-false, ev-nil ev-false} form ev-true))

;;;; --

;; TODO: deref, var, quote
(def alpha-ops "Call-position symbols that we can translate."
  '{if ::if
    vec ::vec
    concat ::concat
    inc ::inc
    dec ::dec})

(defn unique
  [x]
  (gensym (symbol (str x \_))))

(defn scope-name
  [scope]
  (apply str (interpose \- (map name scope))))

(defn fn-name
  [scope]
  (unique (scope-name scope)))

(defn compile-number
  [n]
  (cond (zero? n) '(+)
        (pos? n) (cons '+ (repeat n '(*)))
        :else (list '- (compile-number (- n)))))

(defn form-dispatch
  [form]
  (cond (integer? form) :integer
        (keyword? form) :lit
        (symbol? form) :lit
        (vector? form) :vector
        (map? form) :map

        (sequential? form)
        (cond (empty? form) :lit
              (alpha-ops (first form)) (alpha-ops (first form))
              :else :lit)))

(defmulti ^:private c-form "Slightly more raw form of compile-form."
  (fn [_ form] (form-dispatch form)))

(defn compile-form
  "Compile a form in a descent-scope into something suitable for a function
body."
  [scope form]
  (->
   (c-form scope form)
   (assoc :scope scope)
   (update-in [:helpers] (fnil identity {}))))

(defmethod c-form :lit [scope form]
  {:scope scope, :body form})

(defmethod c-form :integer [scope form]
  {:scope scope, :body (compile-number form)})

(defmethod c-form :vector [scope form]
  (let [raw (map #(c-form (conj scope 'v) %) form)
        [bodies helpers] (transpose (map (juxt :body :helpers) raw))]
    {:scope scope
     :body (vec bodies)
     :helpers (apply merge helpers)}))

(defmethod c-form :map [scope form]
  (let [[ks vs] (transpose form)
        cks (c-form (conj scope 'mk) ks)
        cvs (c-form (conj scope 'mv) vs)]
    {:scope scope
     :body (into {} (map vector (:body cks) (:body cvs)))
     :helpers (merge (:helpers cks) (:helpers cvs))}))

(declare compile-fn)

(defmethod c-form ::if [scope form]
  (let [[_ test then else] form
        ctest (compile-form (conj scope 'ifg) test)
        then-name (fn-name (conj scope 'ift))
        else-name (fn-name (conj scope 'iff))
        cthen (compile-fn then-name then)
        celse (compile-fn else-name else)]
    {:body `(::call-helper ({~ev-true ~then-name}
                            ~(wrap-boolean (:body ctest))
                            ~else-name))
     :helpers (merge {then-name (:body cthen)
                      else-name (:body celse)}
                     (:helpers ctest)
                     (:helpers cthen)
                     (:helpers celse))}))

(defmethod c-form ::inc [scope form]
  (let [[_ arg] form
        {:keys [body helpers]} (compile-form scope arg)]
    {:body (list '+ body '(*))
     :helpers helpers}))

(defmethod c-form ::dec [scope form]
  (let [[_ arg] form
        {:keys [body helpers]} (compile-form scope arg)]
    {:body (list '- body '(*))
     :helpers helpers}))

;; TODO: lexicals will need to be conveyed somehow
(defn compile-fn
  [name body]
  ;; TODO
  (compile-form [name] body))

;; This is what an input should look like.
#_
(letfn [(main [in]
          (if (= in [])
            ;; empty input -> []
            []
            ;; recur with extra accumulator args
            (main-acc in 1 [])))
        (main-acc [in nexdex accum]
          (if (= in (vec (concat [(in 0)] accum)))
            ;; if first + rest = input, we're done
            accum
            ;; otherwise recur with new accumulator
            (main-acc in
                      (inc nexdex)
                      (vec (concat accum [(in nexdex)])))))]
  ;; call the main method with the input and fns
  (main [0 1 2 3 4 5]))
