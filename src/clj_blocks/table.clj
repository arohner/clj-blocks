(in-ns 'clj-blocks.core)

(defmethod render-view-blocks :table [blocks _]
  [:tr {:class "clj-block-row"}
   (for [block blocks]
     [:td (render-block block :table)])])

(defn table-header [fields]
  [:thead
   [:tr
    (for [field fields]
      [:th (get-label field)])]])

(defn with-table
  "produces an HTML table containing rows, all rendered as type model"
  [view rows & {:keys [id options]}]
  (list
   [:table {:id id}
   (table-header (get-fields view (first rows)))
   [:tbody
    (when (seq rows)
      (for [row rows]
        (render-view view :table row)))]]
   (when id
     (js/script (js/on-ready (js/data-table id options))))))