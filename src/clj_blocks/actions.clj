(ns clj-blocks.actions
  (:use [ring.util.response :only (redirect)])
  (:require [compojure.core :as compojure])
  (:require [clojure.contrib.string :as str])
  (:use [hiccup.core :only (html)])
  (:use [hiccup.page-helpers :only (url link-to)])
  (:require [ring.middleware.keyword-params])
  (use [sandbar.stateful-session :only (session-put! session-get)]))

(let [random-char-map (into {} (map vector (range) (concat (range (int \0) (int \9)) (range (int \A) (int \Z)) (range (int \a) (int \z)))))]
  (defn random-char []
    (char (get random-char-map (rand-int (count random-char-map))))))

(defn create-action-id []
  (apply str (for [i (range 30)]
                 (random-char))))

(defn- create-action! [fn]
  {:pre [fn]}
  (let [aid (create-action-id)]
    (session-put! :actions (assoc (session-get :actions) aid fn))
    aid))

(defn- get-action [aid]
  (when-let [actions (session-get :actions)]
    (get actions aid)))

(defn- valid-action? [aid]
  (boolean (get-action aid)))

(defn action-handler [request]
  (let [{{aid :action} :params} request
        action-fn (get-action aid)]
    (assert aid)
    (assert action-fn)
    (action-fn (:params request))
    (redirect (get-in request [:headers "referer"]))))

(def *action-path* nil)

(defn action-url!
  "returns a url that returns the result of calling fn"
  [fn]
  ;; (assert user-has-session)
  ;; (assert stateful-session)
  ;; (assert *action-path*)
  (let [aid (create-action! fn)]
    (assert *action-path*)
    (url *action-path* {:action aid})))

(defn action-link [thunk content]
  (html
   (link-to (action-url! thunk) content)))

(defn wrap-action-handler [handler path]
  "specifies path to be the route that will handle clj-blocks actions"
  (let [action-route (compojure/compile-route nil path {:as request} (action-handler request))
        handler (-> handler
                    (compojure.core/wrap-routes [action-route])
                    (ring.middleware.keyword-params/wrap-keyword-params))]
    (fn [request]
      (binding [*action-path* path]
        (handler request)))))