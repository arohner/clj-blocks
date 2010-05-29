(ns clj-blocks.core
  (:use [hiccup.core :only (html)])
  (:require [hiccup.page-helpers :as page])
  (:require [hiccup.form-helpers :as form])
  (:use [clojure.contrib.java-utils :only (as-str)])
  (:require [clojure.contrib.string :as str])
  (:require [clojure.contrib.seq :as seq])
  (:use clj-blocks.utils)
  (:use [clojure.contrib.except :only (throwf)])
                                        ;(:require [sandbar.forms :as forms])
  )

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

(defn render-block-dispatch [object context]
  (assert (instance? block object))
  [(or (:display-as object) (:type object)) context])

(defmulti render-block
  "renders an object/value, in a specific context"
  render-block-dispatch
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

(defrecord view
  [fields
   reader
   writer])

(defn make-view [argmap]
  (make-defrecord-from-map view argmap))

(defmacro defview
  "a view is a customized definition of how to render and read a clojure map.

The view defines several functions. :fields, :reader, :writer. :fields is required, :reader and writer are optional.

  fields is a seq of fields. Each field is a vector. The first element in the vector is the name of the field. The name is used for form values, and is the default label for a field, if label is not specified.

   The rest of the vector is a map of arguments. The following keys are recognized:
   :label - This is an html label, used in data and form contexts. When not specified, inferred from name.
   :value - The value to display. May be a literal or a fn of one argument, the input map. If not specified, the value will be looked up in the map using name as the key. 
   :type - The data type of the field. This is used to read, and specifies the default rendering. When rendering and type is not specified, inferred to be (type value). When reading and not specified, type is assumed to be :string
   :display-as - Overrides the default rendering; must be a value that render-block understands. Many values will consume extra optional arguments in the field map, consult the documentation for those.

   :dom-id
   :css-class
   :css-style
 
   :writer - an optional fn. Takes the input map. Returns another map which will be used as the input to :fields. Do arbitrary pre-processing of the map here.
   :reader - optional fn. Called after reading the map in from an http post. Takes one argument, the read in map. do arbitrary post-processing here. "
  
  [name & args]
  (let [argmap (apply hash-map args)]
    `(def ~name (make-view ~argmap))))

(defn get-name [field]
  (first field))

(defn get-options [field]
  (second field))

(defn get-label [field]
  (let [name (get-name field)
        options (get-options field)]
    (or (:label options) (humanize name))))

(defn get-defaults [map field]
  (let [name (get-name field)
        options (second field)
        value (get map name)]
    {:name (clojure.core/name name)
     :value value
     :label (or (:label options) (humanize name))}))

(defn call-fns-of-map [field map]
  (reduce (fn [field [key value]]
            (if (fn? value)
              (assoc field key (value map))
              field)) field field))

(defn get-view-blocks [view map]
  (let [map (if-let [reader-fn (:reader view)]
              (reader-fn map)
              map)]
    (for [field (:fields view)
          :let [defaults (get-defaults map field)
                [name options] field
                options (merge defaults options)
                options (call-fns-of-map options map)
                options (if (:type options)
                          options
                          (merge options {:type (or (type (:value options)) :string)}))]]
      (make-block options))))

(defn read-view
  "Reads in the values from an HTTP params map, using a view created by defview. Returns a clojure map. "
  [view params]
  (let [obj (into {} (for [block (get-view-blocks view {})
                           :let [{name :name
                                  type :type} block]]
                       (do
                         (assert type)
                         [(keyword name) (read-block (get params (keyword name)) type)])))
        reader-fn (:reader view)]
    (if reader-fn
      (reader-fn obj)
      obj)))

(defmulti render-view-blocks (fn [blocks context]
                                context))

(defn render-view [view context map]
  (let [map (if-let [writer-fn (:writer view)]
              (writer-fn map)
              map)]
    (render-view-blocks (get-view-blocks view map) context)))

(defn splice-seq
  "replaces the value at position idx with new-val. Returns the updated seq"
  [seq idx new-val]
  (concat (take idx seq) [new-val] (drop (inc idx) seq)))

(load "builtins")
(load "table")
(load "form")