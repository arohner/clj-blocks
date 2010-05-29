(in-ns 'clj-blocks.core)

(defmethod render-view-blocks :form [blocks _]
  [:div {:class "clj-block-form"}
   (for [block blocks
         :let [dispatch-value (or (:display-as block) (:type block))
               body (render-block block :form)]]
     [:div {:class "clj-block-value"}
      (if (not= :hidden dispatch-value)
        (with-label block
          body)
        body)])])