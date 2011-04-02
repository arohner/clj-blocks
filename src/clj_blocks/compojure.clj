;; this is compojure.core v0.6.2, with some minor modifications. Copyright James Reeves, Licensed under EPL

(ns clj-blocks.compojure
  (:require [clojure.string :as str])
  (:use [clj-blocks.utils :only (inspect)])
  (:use clout.core
        compojure.response
        [clojure.contrib.core :only (-?>)]
        [clojure.contrib.def :only (name-with-attributes)]))


(defn method-matches?
  "True if this request matches the supplied request method."
  [method request]
  (let [request-method (request :request-method)
        form-method    (get-in request [:form-params "_method"])]
    (if (and form-method (= request-method :post))
      (= (str/upper-case (name method)) form-method)
      (= method request-method))))

(defn if-method
  "Evaluate the handler if the request method matches."
  [method handler]
  (fn [request]
    (cond
      (or (nil? method) (method-matches? method request))
        (handler request)
      (and (= :get method) (= :head (:request-method request)))
        (-?> (handler request)
             (assoc :body nil)))))

(defn assoc-route-params
  "Associate route parameters with the request map."
  [request params]
  (merge-with merge request {:route-params params, :params params}))

(defn if-route
  "Evaluate the handler if the route matches the request."
  [route handler]
  (fn [request]
    (if-let [params (route-matches route request)]
      (handler (assoc-route-params request params)))))

(defn- prepare-route
  "Pre-compile the route."
  [route]
  (cond
    (string? route)
      (route-compile route)
    (vector? route)
      (route-compile
        (first route)
        (apply hash-map (rest route)))
    :else
      (if (string? route)
         (route-compile route)
         route)))

(defn assoc-&-binding [binds req sym]
  (assoc binds sym `(dissoc (:params ~req)
                            ~@(map keyword (keys binds))
                            ~@(map str (keys binds)))))

(defn assoc-symbol-binding [binds req sym]
  (assoc binds sym `(get-in ~req [:params ~(keyword sym)]
                      (get-in ~req [:params ~(str sym)]))))

(defn vector-bindings
  "Create the bindings for a vector of parameters."
  [args req]
  (loop [args args, binds {}]
    (if-let [sym (first args)]
      (cond
        (= '& sym)
          (recur (nnext args) (assoc-&-binding binds req (second args)))
        (= :as sym)
          (recur (nnext args) (assoc binds (second args) req))
        (symbol? sym)
          (recur (next args) (assoc-symbol-binding binds req sym))
        :else
          (throw (Exception. (str "Unexpected binding: " sym))))
      (mapcat identity binds))))

(defmacro let-request [[bindings request] & body]
  (if (map? bindings)
    `(let [~bindings ~request] ~@body)
    `(let [~@(vector-bindings bindings request)] ~@body)))

(defn apply-request
  [f bindings request]
  (if (map? bindings)
    (f request)
    (let [f-args (for [arg bindings]
                   (get-in request [:params (keyword arg)]))]
      (apply f f-args))))

(defmacro compile-route
  "Compile a route in the form (method path f) into a route function. f should be a function that takes the arguments specified in bindings"
  [method route bindings f]
  `(let [prepared-route# (#'prepare-route ~route)]
     (#'if-method ~method
                  (#'if-route prepared-route#
                              (fn [request#]
                                (render (apply-request ~f ~bindings request#) request#))))))