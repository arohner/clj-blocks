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
  "produces an HTML table containing rows, all rendered as type model. Uses the keys on the first row for columns.

Recognized keys:

id - the id for the table element
options - a map of options for datatables. Anything recognized by the datatables constructor is valid
table-var-name - a string or symbol specifying a javascript variable name for the datatables object"
  [view rows & {:keys [id datatables options table-var-name] :or {datatables true}}]
  (list
   [:table {:id id
            :class "display"}
    (table-header (get-fields view (first rows)))
    [:tbody
     (when (seq rows)
       (for [row rows]
         (render-view view :table row)))]]
   (let [table-var (or (when table-var-name (symbol table-var-name)) (gensym "table"))]
     (when (and id datatables)
       (list
        (js/script
         (js/on-ready
          (js* (var (clj table-var) (clj (js/data-table id options)))))))))))

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