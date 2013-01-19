(ns org.timmc.swearjure
  (:refer-clojure :exclude (compile)))

(defn transpose
  [xs]
  (if (empty? xs)
    []
    (apply map vector xs)))

;; TODO: deref, var, quote
(def alpha-ops "Call-position symbols that we can translate."
  '#{if vec concat inc dec})

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
              (symbol? (first form)) (keyword (first form)))))

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
