(in-ns 'clj-blocks.core)

(defmethod render-view-blocks :data [blocks _]
  [:div {:class "clj-block-data"}
   (for [block blocks
         :let [body (render-block block :data)]]
     [:div {:class "clj-block-value"}
      (with-label block
        body)])])