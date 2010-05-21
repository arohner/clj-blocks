(ns clj-blocks.routes
  (:use clj-blocks.utils)
  (:use [clojure.contrib.except :only (throwf)])
  (:require [clojure.contrib.string :as str])
  ;(:require [clout.core :as clout])
  (:require [compojure.core :as compojure]))

(defmacro defroutefn
  "Defines a defn, and associates it with a URL route. The defined fn
  takes one argument, the request. The call to defroutefn must be in a
  namespace using restful-routes"

  [name [http-method http-path] & defn-args]
  (let [defn-args (cons name defn-args)
        arg-map (apply decompose-defn-args* defn-args)
        arg-map (update-in arg-map [:attr-map] merge {::http-method http-method
                                                      ::http-path http-path})]
    `(defn ~@(defn-map* arg-map))))

(defn find-route-fns
  "introspects the namespace, returns a seq of [fn-name http-method path] vecs"
  [ns]
  (for [[name var-fn] (ns-publics ns) :when (::http-method (meta var-fn))]
    [(keyword name) (::http-method (meta var-fn)) (::http-path (meta var-fn))]))

(defmacro ns-routes
  "Returns the set of routes from ns defined using defroutefn"
  [ns-symbol]
  (require ns-symbol)
  (let [ns (find-ns ns-symbol)
        routes (->>
                (for [[fn-name http-method path] (find-route-fns ns)]
                  (do
                    (assert fn-name)
                    (assert ns)
                    (let [f (ns-resolve ns (symbol (clojure.core/name fn-name)))
                          path (format path name)]
                      (when f
                        (println "adding route" http-method path "->" fn-name)
                        (@(var compojure/compile-route) http-method path {:as 'request} [`(~f ~'request)])))))
                (filter identity)
                (into []))]
    `(apply compojure/routes ~routes)))

(defn path-for* [routefn route-args]
  {:pre [routefn]}
  (let [path (-> routefn meta ::http-path)]
    (when (not path)
      (throwf "no route metadata on %s" routefn))
    (reduce (fn [path [key val]]
              (assert (keyword? key))
              (str/replace-str (str key) (str val) path)) path route-args)))

(defmacro path-for
  "Returns the URL path for a var defined using defroutefn. If the route contains any variables, i.e. /foo/:id, route-args is a map of keywords to values to replace.

  path-for resolves vars at runtime (not compile time)"
  [routefn & [route-args]]
  (let [curr-ns *ns*]
    `(path-for* (ns-resolve ~curr-ns (quote ~routefn)) ~route-args)))
