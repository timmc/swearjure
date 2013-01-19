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

(defn compile-form
  [scope form]
  (->
   (cond (integer? form)
         {:body (compile-number form)}

         (keyword? form)
         {:body form}

         (symbol? form)
         {:body form}

         (vector? form)
         (let [raw (map #(compile-form (conj scope 'v) %) form)
               [bodies helpers] (transpose (map (juxt :body :helpers) raw))]
           {:body (vec bodies)
            :helpers (apply merge helpers)})

         (map? form)
         (let [[ks vs] (transpose form)
               cks (compile-form (conj scope 'mk) ks)
               cvs (compile-form (conj scope 'mv) vs)]
           {:body (into {} (map vector (:body cks) (:body cvs)))
            :helpers (merge (:helpers cks) (:helpers cvs))}))
   (assoc :scope scope)
   (update-in [:helpers] (fnil identity {}))))

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
