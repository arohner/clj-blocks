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
  "produces an HTML table containing rows, all rendered as type model. Uses the keys on the first row for columns."
  [view rows & {:keys [id options]}]
  (list
   [:table {:id id
            :class "display"}
    (table-header (get-fields view (first rows)))
    [:tbody
     (when (seq rows)
       (for [row rows]
         (render-view view :table row)))]]
   (when id
     (js/script (js/on-ready (js/data-table id options))))))

(defn to-data-table
  "given a seq of rows from the database, orders them appropriately"
  [view rows]
  (for [row rows
        :let [blocks (get-view-blocks view row)]]
    (map #(html (render-block % :table)) blocks)))

(defn colnum-to-name [view col-num]
  "returns the :name for the column with index col-num"
  (:name (nth (get-view-blocks view {}) col-num)))

(def sortdir-to-int {"asc" 1
                     "desc" -1})