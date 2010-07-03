(ns clj-blocks.routes
  (:use [clj-blocks.core :only (read-view)])
  (:use clj-blocks.utils)
  (:use [clojure.contrib.except :only (throwf)])
  (:require ring.middleware.keyword-params)
  (:require [clojure.contrib.string :as str])
  (:require [compojure.core :as compojure]))

(defmacro defroutefn
  "Creates a function that can be called normally, or in response to an
   HTTP request.

  View is an optional argument, a view created with defview. 

  If a view is used, when the fn is called as an HTTP request, the
  arguments will be taken from the http params map, read using
  read-view, and then passed to the fn. When the fn is called in
  normal clojure code, read-view will not be called, so the the
  argument should be the correct type

  If the function has & args, the & arg will be a map containing
  params in the request map that weren't bound to a positional
  argument.

  The call to defroutefn must be in a namespace using ns-routes.

  Requires the wrap-read-view middleware"

  [name [http-method http-path view?] & defn-args]
  (let [defn-args (cons name defn-args)
        arg-map (apply decompose-defn-args* defn-args)
        params (:params arg-map)
        arg-map (update-in arg-map [:attr-map] merge
                           {::http-method http-method
                            ::http-path http-path
                            ::bindings (if (and (= 1 (count params)) (map? (first params)))
                                         `(quote ~(first params))
                                         `(quote ~(:params arg-map)))
                            ::view view?})]
    `(defn ~@(defn-map* arg-map))))

(defn find-route-fns
  "introspects the namespace, returns a seq of [fn-name http-method path] vecs"
  [ns]
  (for [[name var-fn] (ns-publics ns) :when (::http-method (meta var-fn))
        :let [metadata (meta var-fn)]]
    [(keyword name) (::bindings metadata) (::http-method metadata) (::http-path metadata)]))

(defn ns-routes* [ns-symbol]
  (require ns-symbol)
  (let [ns (find-ns ns-symbol)]
    (->>
     (for [[fn-name fn-binding http-method path] (find-route-fns ns)]
       (do
         (assert fn-name)
         (assert ns)
         (let [f (ns-resolve ns (symbol (clojure.core/name fn-name)))]
           (when f
             (println "adding route" http-method path "->" fn-name)
             (compojure/compile-route* http-method path fn-binding f)))))
     (filter identity))))
  
(defmacro ns-routes
  "Returns the set of routes in ns defined using defroutefn"
  [ns]
  `(ns-routes* (quote ~ns)))

(defn path-for* [routefn route-args]
  {:pre [routefn]}
  (let [path (-> routefn meta ::http-path)
        path (if (string? path)
               path
               (first path))]
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

(defn apply-map
  "Like apply, but if the last argument is a map, will convert the map to work with fn signatures that ends with a map destructuring. i.e.

   (defn foo [ a b & {opt1 :opt1 opt2 :opt2}])
   (apply foo 1 2 {:opt1 42 :opt2 3.14})"
  [f & args]
  (let [args (if (map? (last args))
                 (concat (butlast args) (interleave (keys (last args)) (vals (last args))))
                 args)]
    (apply f args)))

(defn wrap-read-view [handler]
  "middleware. If the request has a :request-fn key and the request-fn
has a :view metadata, will read-view the params. You probably want to
insert this between compojure's find-matching-route and
call-matching-route."
  (fn [request]
    (if (and (:route-fn request) (::view (meta (:route-fn request))))
      (let [view (::view (meta (:route-fn request)))
            request (update-in request [:params] #(read-view view %))]
        (handler request))
      (handler request))))

(defmacro def-webfn
  ""
  
  [name [http-method http-path] view signature & body]
  `(defroutefn ~name [~http-method ~http-path] {::view ~view} ~signature
     ~@body))

