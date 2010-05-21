(ns clj-blocks.core
  (:use [hiccup.core :only (html)])
  (:use [clojure.contrib.java-utils :only (as-str)])
  (:require [clojure.contrib.string :as str])
  (:require [clojure.contrib.seq :as seq])
  (:use clj-blocks.utils)
  (:use [clojure.contrib.except :only (throwf)])
  (:require [sandbar.forms :as forms]))

;;;; UI framework for building web pages quickly. Inspired by weblocks
;;;; and Ian Eslick.

(def h (ref (make-hierarchy)))

(defrecord block
  [type        ;; the dispatch value for the block. required
   name        ;; optional name for the data. Required in form contexts
   value       ;; the value of the data. required

   label       ;; user-visible description of the data
   dom-id      ;; optional dom id to attach
   css-class   ;; optional CSS class
   css-style   ;; optional CSS style
   ])

(defn make-block
  "makes a block"
  [fields]
  (make-defrecord-from-map block fields))

(defmulti render-block
  "renders an object/value, in a specific context"
  (fn [object context]
    (assert (instance? block object))
    [(:type object) context])
  :hierarchy h)

(defn render [object context]
  (render-block (if (instance? block object)
                  object
                  (make-block {:type (type object) :value object})) context))

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

(defn read-from-params*
  "fields is [[param-name param-type]+] param-name is a key in the
  params map, param-type is a value in the blocks hierarchy that has
  implemented read-block. Returns a map of the parsed values"
  [params fields]
  (into {} (for [[name type] fields]
             (do
               (println name type)
               [name (read-block (get params (clojure.core/name name)) type)]))))

(defrecord model
  [fields
   to-block
   from-block
   validator])

(defmacro defmodel
  "a model is a 'schema' for defining how to render and read a block that is composed of multiple values (like a DB row, comprised of multiple columns). Fields is a seq of maps. Each map defines a component block.

If there are more or fewer fields in the block than the row, pass functions to-block from-block. To-block takes one argument, your domain object, and returns a seq of blocks. from-block does the reverse."
  [name & args]
  (let [argmap (apply hash-map args)]
    `(def ~name (make-defrecord-from-map model ~argmap))))

(defn get-model-blocks [model row]
  (let [row (if (:to-block model)
              ((:to-block model) row)
              row)]
    (for [field (:fields model)]
      (make-block (merge field {:value (get row (:name field))})))))

(defmulti render-model-blocks (fn [blocks context]
                                context))

(defn splice-seq
  "replaces the value at position idx with new-val. Returns the updated seq"
  [seq idx new-val]
  (concat (take idx seq) [new-val] (drop (inc idx) seq)))

(defn update-block
  "given a seq of blocks, finds the block with name, and applies f with any extra args. f should return an updated block"
  [blocks name f & args]
  (let [[index block] (seq/find-first (fn [[index block]]
                                        (= name (:name block))) (seq/indexed blocks))]
    (if block
      (splice-seq blocks index (apply f block args))
      blocks)))

(load "builtins")
(load "table")