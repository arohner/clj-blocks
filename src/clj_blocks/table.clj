(in-ns 'clj-blocks.core)

(defmethod render-view-blocks :table [blocks _]
  [:tr {:class "clj-block-row"}
   (for [block blocks]
     [:td (render-block block :table)])])

(defn table-header [view]
  [:thead
   [:tr
    (for [field (:fields view)]
      [:th (get-label field)])]])

(defn with-table
  "produces an HTML table containing rows, all rendered as type model"
  [view rows & {:keys [id]}]
  [:table {:id id}
   (table-header view)
   [:tbody
    (when (seq rows)
      (for [row rows]
        (render-view view :table row)))]])