(in-ns 'clj-blocks.core)

(defmethod render-model-blocks :table [blocks _]
  (println "render-model-blocks :table:" blocks)
  [:tr {:class "clj-block-row"}
   (for [block blocks]
     [:td (render-block block :table)])])

(defn table-header [model]
  [:thead
   [:tr
    (for [field (:fields model)]
      [:th (:label field)])]])

(defn with-table
  "produces an HTML table containing rows, all rendered as type model"
  [model rows & {:keys [id]}]
  [:table {:id id}
   (table-header model)
   [:tbody
    (for [row rows]
      (render (with-meta row {:type model}) :table))]])