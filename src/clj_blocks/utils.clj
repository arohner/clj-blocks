(ns clj-blocks.utils
  (:require [clojure.string :as str])
  (:require clojure.pprint)
  (:use clojure.test))

(defn map-keys [f m]
  "returns a new map with f applied to the keys of the map"
  (into {} (map (fn [[key val]]
                  [(f key) val]) m)))

(defn map-vals [f m]
  "returns a new map with f applied to the vals of the map"
  (into {} (map (fn [[key val]]
                  [key (f val)]) m)))

(defn deftype-fields [c]
  "returns a seq of the names of a field, in the order required by the deftype constructor"
  
  (map #(.getName %) (.getFields c)))

(defn make-defrecord-from-map [deftype-class map]
  (let [ctor (first (sort-by #(count (.getParameterTypes %)) (.getConstructors deftype-class)))
        fn-arity (count (.getParameterTypes ctor))
        inst (.newInstance ctor (into-array Object (take fn-arity (repeatedly (constantly nil)))))]
    (into inst map)))

(defn make-defrecord-map-constructor 
  "returns a fn that takes a map, and constructs a defrecord instance
  with the fields supplied. takes the class of the defrecord" 
  [deftype-class]
  (let [ctor (first (sort-by #(count (.getParameterTypes %)) (.getConstructors deftype-class)))
        fn-arity (count (.getParameterTypes ctor))
        inst (.newInstance ctor (into-array Object (take fn-arity (repeatedly (constantly nil)))))]
    (fn [keyval-map]
      (let [args (keys inst)
            vals (into-array Object (map (fn [arg] (get keyval-map arg)) args))]
        (.newInstance ctor vals)))))

(defn decompose-defn-args* [& args]
  (letfn [(parse-name [args]
                      (assert (symbol? (first args)))
                      [(first args) (rest args)])          
          (parse-doc-string [args]
                            (if (string? (first args))
                              [(first args) (rest args)]
                              [nil args]))
          (parse-attr-map [args]
                          (if (map? (first args))
                            [(first args) (rest args)]
                            [nil args]))
          (parse-params [args]
                        (if (vector? (first args))
                          [(first args) (rest args)]
                          [nil args]))
          (parse-body [args]
                      [args nil])]
    (let [[name args] (parse-name args)
          [doc-string args] (parse-doc-string args)
          [attr-map args] (parse-attr-map args)
          [params args] (parse-params args)
          [body args] (parse-body args)]
      {:name name
       :doc-string doc-string
       :attr-map (merge attr-map (meta name))
       :params params
       :body body})))

(defmacro  decompose-defn-args
  "interprets args the way defn would, returns a map that can be consumed by defn-map"
  [& args]
  `(apply decompose-defn-args* (quote ~args)))

(defn defn-map* [arg-map]
  (let [defn-args (filter identity ((juxt :name :doc-string :attr-map :params) arg-map))
        body (:body arg-map)]
    `(~@defn-args ~@body)))

(defn humanize [obj]
  (let [obj (if (or (symbol? obj)
                    (keyword? obj))
              (name obj)
              obj)]
    (str/capitalize obj)))

(defmacro defn-map
  "generates a defn expression, but arguments are a map, to make it
  easier on macro writers. Valid keys: name, doc-string, attr-map,
  params, body. If params is nil, then body is assumed to be multi-arity
  expression, i.e. ([params] body)+ "
  [arg-map]
  `(defn ~@(defn-map* arg-map)))

(defn apply-map
  "Like apply, but if the last argument is a map, will convert the map to work with fn signatures that ends with a map destructuring. i.e.

   (defn foo [ a b & {opt1 :opt1 opt2 :opt2}])
   (apply-map foo 1 2 {:opt1 42 :opt2 3.14})"
  [f & args]
  (let [args (#'clojure.core/spread args)
        args (if (map? (last args))
               (concat (butlast args) (interleave (keys (last args)) (vals (last args))))
               args)]
    (apply f args)))

(deftest test-apply-map
  (is (= :x (apply-map (fn [] :x) [])))
  (is (= 3 (apply-map + [1 2])))
  (is (= nil (apply-map (fn [x y & {:keys [opt]}]
                          opt) [1 2])))
  (is (= :magic (apply-map (fn [x y & {:keys [opt]}]
                             opt) [1 2 {:opt :magic}]))))

(defmacro inspect 
  "prints the expression '<name> is <value>', and returns the value"
  [value]
  `(let [result# ~value]
     (println '~value "is" (with-out-str (clojure.pprint/pprint result#)))
     result#))