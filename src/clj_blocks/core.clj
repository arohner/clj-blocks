(ns clj-blocks.core
  (:use [hiccup.core :only (html)])
  (:use [clojure.contrib.java-utils :only (as-str)])
  (:use [clj-blocks.utils :only (map-vals make-defrecord-map-constructor)])
  (:use [clojure.contrib.except :only (throwf)])
  (:require [sandbar.forms :as forms]))

;;;; UI framework for building web pages quickly. Inspired by weblocks
;;;; and Ian Eslick.

(def h (ref (make-hierarchy)))

(defrecord block
  [type        ;; the dispatch value for the block. required
   name        ;; optional name for the data. Required in form contexts
   label       ;; user-visible description of the data
   value       ;; the value of the data. required
   
   dom-id      ;; optional dom id to attach
   css-class   ;; optional CSS class
   css-style   ;; optional CSS style
   ])

(let [block-constructor (make-defrecord-map-constructor block)]
  (defn make-block
  "makes a block. options are key value pairs that set fields in the block record"
  [type value & options]
  (let [options (map-vals as-str (apply hash-map options))]
    (block-constructor (merge {:type type :value value} options)))))

(defmulti render-block
  "renders an object/value, in a specific context"
  (fn [object context]
    ;(println "render-block dispatch:" object context)
    (assert (instance? block object))
    [(:type object) context])
  :hierarchy h)

(defn render [object context]
  (println "render: " (class object))
  (render-block (if (instance? block object)
                  object
                  (make-block (type object) object)) context))

(defn derive-ref [ref-h child parent]
  (dosync
   (alter ref-h derive child parent)))

(defmulti read-block
  (fn [val-str type]
    type)
  :hierarchy h)

(defmulti validate-block
  "returns truthy if object is valid. Optionally takes a type to dispatch on, in case it can't be inferred from the type of the value, i.e. :positive-int. Returns true by default."
  (fn
    ([val type]
       type)
    ([val]
       (type val))) 
  :hierarchy h)

(defn with-label [block body]
  (html
   (when (:label block)
     [:label {:for (:name block)}
      (:label block)": "])
   body))

(defn read-from-params
  "fields is [[param-name param-type]+] param-name is a key in the params map, param-type is a value in the blocks hierarchy that has implemented read-block. Returns a map of the parsed values"
  [params fields]
  (into {} (inspect (for [[name type] fields]
             (do
               (println name type)
               [name (read-block (get params (clojure.core/name name)) type)])))))

(load "builtins")