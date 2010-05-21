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
                  (map-vals str (select-keys block [:id :class :style :name :value])))]))

(defmethod read-block :bool [val-str _]
  (if (= "true")
    true
    false))

;;=========
;; Integers
;;=========

(derive-ref h Integer :int)

(defmethod render-block [:int :any-context] [block _]
  (str (:value block)))

(defmethod read-block :int [val-str _]
  (Integer/parseInt val-str))

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
  ; (forms/form-textfield (:prompt block) (:name block) (map-vals str (select-keys block [:id :class :style :name :value]))))

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
  (println "render-block vector:" block)
  (str "[" (str/join " " (map #(render % context) (:value block))) "]"))

(defmethod render-block [:link :any-context] [block context]
  (println "render-block link:" block)
  [:a {:href (:link-to block)} (:value block)])