(ns clj-blocks.utils)

(defn map-keys [f m]
  "returns a new map with f applied to the keys of the map"
  (into {} (map (fn [[key val]]
                  [(f key) val]) m)))

(defn map-vals [f m]
  "returns a new map with f applied to the vals of the map"
  (into {} (map (fn [[key val]]
                  [key (f val)]) m)))

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