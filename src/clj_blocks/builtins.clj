(in-ns 'clj-blocks.core)

;;=========
;; Defaults
;;=========

(derive-ref h :data :any-context)
(derive-ref h :table :any-context)
(derive-ref h :form :any-context)

(defmethod render-block :default [block context]
  {:pre [(instance? clj-blocks.core.block block)]}
  (str (:value block)))

(defmethod validate-block :default [block & _]
  true)

;; The official rename-keys is currently broken with defrecords. Remove this after
;; https://www.assembla.com/spaces/clojure/tickets/393-fix-rename-keys-to-work-with-defrecords
;; is fixed
(defn rename-keys*
  "Returns the map with the keys in kmap renamed to the vals in kmap"
  {:added "1.0"}
  [map kmap]
    (reduce 
     (fn [m [old new]]
       (if (and (not= old new)
                (contains? m old))
         (-> m (assoc new (get m old)) (dissoc old))
         m)) 
     map kmap))

(defn standard-html-attrs
  "returns a map of the 'standard' elements in a block, in an appropriate form for html attributes"
  [block]
  (-> block
      (rename-keys* {:dom-id :id
                                :css-class :class
                                :css-style :style})
      (select-keys [:id :class :style])))

(defn standard-html-form-attrs
  [block]
  (-> block
      (rename-keys* {:dom-id :id
                                :css-class :class
                                :css-style :style})
      (select-keys [:id :class :style :name :value])))

(defn standard-form-field [block]
  {:pre [(instance? clj-blocks.core.block block)]}
  (html
   [:input (merge {:type "textfield"}
                  (standard-html-form-attrs block))]))
;;=========
;; Bools
;;=========

(derive-ref h Boolean :bool)

(defmethod render-block [:bool :form] [block _]
  (html
   (when (:label block)
     [:label {:for (:name block)} (:prompt block)])
   [:input (merge {:type "checkbox"}
                  {:checked (boolean (:value block))}
                  (standard-html-form-attrs block))]))

(defmethod render-block [:checkbox :form] [block _]
  (form/check-box (:name block) (boolean (:value block))))

(defmethod read-block :bool [val-str _]
  (if (= val-str "true")
    true
    false))

;;=========
;; Integers
;;=========

(derive-ref h Integer :int)

(defmethod render-block [:int :any-context] [block _]
  (str (:value block)))

(defmethod read-block :int [val-str _]
  (if val-str
    (Integer/parseInt val-str)
    nil))

(defmethod validate-block :int [val & _]
  (integer? val))

(defmethod render-block [:int :form] [block _]
  (standard-form-field block))

;; example of a derived type on top of int

(defmethod validate-block :positive-int [val & _]
  (and (integer? val) (> val 0)))

(derive-ref h Double :floating-point)
(derive-ref h Float :floating-point)

(defmethod render-block [:floating-point :any-context] [block _]
  (str (format "%2.2f" (:value block))))

;;========
;; Strings
;;========

(derive-ref h String :string)

(defmethod render-block [:string :data] [block _]
  (html
   (:value block)))

(defmethod render-block [:string :table] [block _]
  (:value block))

(defmethod render-block [:string :form] [block _]
  (standard-form-field block))

(defmethod read-block :string [s _]
  s)

(defmethod read-block :keyword [s _]
  (assert (string? s))
  (keyword s))

;;========
;; Dates
;;========

(defn date-formatter []
  (java.text.SimpleDateFormat. "EEE MMM dd HH:mm:ss yyyy")) ;; SimpleDateFormat is not re-entrant

(derive-ref h Long :date)

(defn- date-as-string [dateInMillis]
 (let [cal (java.util.Calendar/getInstance)]
   (.setTimeInMillis cal dateInMillis)
   (.format (date-formatter) (.getTime cal))))

(defmethod render-block [:date :data] [block _]
 (date-as-string (:value block)))

(defmethod render-block [:date :table] [block _]
 (date-as-string (:value block)))

(defmethod render-block [:date :form] [block _]
 (standard-form-field block (date-as-string (:value block))))

(defmethod read-block :date [string _]
 (if string
   (java.util.Date/parse string)
   nil))

;;========================
;; Default Object handling
;;========================

(derive-ref h clojure.lang.IPersistentMap :map)

(defn render-nested-blocks [object context]
  ;{:pre [(instance? block object)]}
  (for [[name val] object
        :let [block (make-block {:type (type val) :value val :name name :label name})]]
    [:div {:class "clj-block-value"}
     (with-label block
       (render-block block context))]))

(defmethod render-block [:map :data] [block _]
  {:pre (instance? clj-blocks.core.block block)}
  [:div {:class "clj-block-object"}
   (render-nested-blocks (:value block) :data)])

(defmethod render-block [:map :table] [block _]
  [:tr {:class "clj-block-row"}
   (for [[name val] (:value block)]
     [:td (render-block (make-block {:type (type val) :value val :name name :label name}) :table)])])

(defmethod render-block [:map :form] [block _]
  [:div {:class "clj-block-form"}
   (render-nested-blocks (:value block) :form)])

(derive-ref h clojure.lang.IPersistentVector :vector)

(defmethod render-block [:vector :data] [block context]
  (str "[" (str/join " " (map #(render % context) (:value block))) "]"))

(defmethod render-block [:link :any-context] [block context]
  [:a (merge
       {:href (:link-to block)}
       (standard-html-attrs block)) (:value block)])

;;===========
;; Select / Dropdown / Multiselect
;;===========

(defn render-select [block]
  (let [id (or (:dom-id block) (gensym "select"))
        block (assoc block :dom-id id)]
    (list [:select
           (standard-html-form-attrs block)
           (for [[label value] (:allowed-values block)]
             [:option {:id (str id "_" value)
                       :label label
                       :selected (= value (:value block))
                       :value value}])]
          (when (:on-change-js block)
            (js/script
             (js/on-ready
              (js/on-change (js/id id)
               (js* (.getScript jQuery (str (clj (actions/action-url! (:on-change-js block))) "&value=" (. (jQuery (clj (str (js/id id) " option:selected"))) attr "value")))))))))))

(defmethod render-block [:dropdown :any-context] [block context]
  (render-select block))

(defmethod render-block [:multiselect :any-context] [block context]
  (render-select (assoc block :multiple true)))

(defmethod render-block [:hidden :form] [block _]
  [:input {:type :hidden :name (:name block) :value (:value block)}])

(defmethod render-block [:post-action :table] [block _]
  (form/form-to [:post (:url block)]
                [:input {:type "submit" :value (:name block)
                         :disabled (or (:disabled block) nil)}]))