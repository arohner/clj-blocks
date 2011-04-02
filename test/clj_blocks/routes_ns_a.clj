(ns clj-blocks.routes-ns-a
  (:use clj-blocks.routes))

(defroutefn no-args [:get "/no-args"] []
  (str :no-args))

(defroutefn one-vector-arg [:get "/one-arg"] [x]
  (str x))

(defroutefn map-args [:get "/map-args"] [{:as request}]
  (str (-> request :params :foo)))

(defroutefn url-arg [:get "/foo/:id"] [id]
  (str id))

(defroutefn route-w-regex [:get ["/regex/:id" :id #"\d+"]] [id]
  (str "regex " id))
