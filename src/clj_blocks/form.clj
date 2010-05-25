(in-ns 'clj-blocks.core)

(defmethod render-view-blocks :form [blocks _]
  [:div {:class "clj-block-form"}
   (for [block blocks]
     [:div {:class "clj-block-value"}
      (with-label block
        (render-block block :form))])])