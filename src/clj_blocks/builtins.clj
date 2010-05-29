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

(defn standard-form-field [block]
  {:pre [(instance? clj-blocks.core.block block)]}
  (html
   [:input (merge {:type "textfield"}
                  (map-vals str (select-keys block [:id :class :style :name :value])))]))
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
                  (map-vals str (select-keys block [:id :class :style :name :value])))]))

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
  [:a {:href (:link-to block)} (:value block)])

;;===========
;; Select / Dropdown / Multiselect
;;===========

(defn render-select [block]
  [:select
   (->
    block
    (select-keys [:dom-id :css-class :css-style :multiple :disabled :name])
    (clojure.set/rename-keys {:dom-id :id :css-class :class :css-style :style}))
   (for [[label value] (:allowed-values block)]
     [:option {:label label
               :selected (= value (:value block))
               :value value}])])

(defmethod render-block [:dropdown :any-context] [block context]
  (render-select block))

(defmethod render-block [:multiselect :any-context] [block context]
  (render-select (assoc block :multiple true)))

(defmethod render-block [:hidden :form] [block _]
  [:input {:type :hidden :name (:name block) :value (:value block)} ])