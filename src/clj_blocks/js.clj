(ns clj-blocks.js
  (:use [com.reasonr.scriptjure :only (js js* cljs)])
  (:use (hiccup core page-helpers form-helpers)))

(defn link
  "makes a link that calls javascript when clicked"
  [attrs text js-code]
  (html [:a (merge attrs {:href "#" :onclick (js (clj js-code)
                                                 (return false))}) text]))

(defn button
  [attrs text js-code]
  [:form {:method "POST" :action "#"}
   [:input (merge 
                  {:type "submit"
                   :onClick (js (clj js-code)
                                (return false))
                   :value text}
                  attrs)]])

(defn script
  "makes an HTML tag that contains javascript. js should either be a string literal or a fragment that can be compiled with scriptjure"
  [js-code]
  (html [:script {:type "text/javascript"} (if (seq? js-code)
                                             (cljs js-code)
                                             js-code)]))

(defn id
  "converts the name of a dom id to a jquery selector. i.e. foo -> #foo"
  [id]
  (str "#" id))

(defn highlight-div
  "returns the javascript to temporarily highlight a div"
  [selector]
  (js* (. (jQuery (clj selector)) effect "highlight" {} 1500)))     

(defn show-div
  [selector]
  (js* (. (jQuery (clj selector)) show "fast")))

(defn hide-div
  [selector]
  (js* (. (jQuery (clj selector)) hide "fast")))

(defn hidden-div
  "returns a link that when clicked, shows body"
  [{:keys [div-id link-id link-text]} & body]
  (let [div-id (or div-id (gensym))]
    (html
     [:div {:id div-id :style "display: none;"}
      (list body)]
     (link {:id link-id} (or link-text "Show") (show-div (id div-id))))))

(defn redirect
  [url]
  (format "window.location.replace(\"%s\")" url))

(defn on-ready
  "executes js-block when the page is ready"
  [js-block]
  (js* (.ready (jQuery document) (fn []
                               (clj js-block)))))

(defn on-click
  "returns a script that adds fn-action as an on click handler for the element with id. fn-action is a javascript function that takes one argument, the click event"
  [selector fn-action]
  (js* (.click (jQuery (clj selector))
               (fn [e]
                 (clj fn-action)))))

(defn on-change
  [selector action]
  (js* (.change (jQuery (clj selector))
                (fn [e]
                  (clj action)))))

;; (defn ajax-js
;;   "returns a javascript function that when called, will make an ajax request to call fn, clojure code, on the server. fn should return javascript that will be eval'd by the client.  
;;  Can be used in jQuery event handlers, i.e.

;; (on-change \"#foo\" (ajax-js #(some-clojure-code)) "
;;   [fn])

(defn progress-spinner [selector]
  "replaces selector with an progress spinner"
  (let [html
        (html [:p {:class "clj-blocks-progress-spinner"} [:img {:src "/gif/progress-spinner.gif"}]])]
    ;; hide the original element, then insert the spinner
    ;; afterwards. If we replace the original element, we would break
    ;; e.g. forms that are about to be submitted.
    (js* (. (jQuery (clj selector)) hide)
         (. (jQuery (clj selector)) after (clj html)))))

(defn cancel-progress-spinner [selector]
  "undos a progress spinner. Pass in the same selector used to create the progress spinner"
  (let [progress-selector (format "%s + \\.%s" selector "clj-blocks-progress-spinner")]
    (js* (do
         (. (jQuery (clj progress-selector)) remove)
         (. (jQuery (clj selector)) show)))))

(defn ajax-post-form [url form-selector on-success]
  "does an AJAX POST to url. form-selector is a jquery selector that identifies the form to serialize. on-success is a js function that gets called with the body on success"
  (js*
   (clj (progress-spinner form-selector))
   (. jQuery post (clj url) (.serialize (jQuery (clj form-selector))) (clj on-success))))

(defn ajax-update [selector url]
  "JS snippet calls jquery .load()"
  (js* (. (jQuery (clj selector)) load (clj url))))

(defn ajax-replace [selector url]
  "JS snippet replaces selector with the content from URL. differs from ajax-update in that it will replace the entire node, rather than the content of the node"
  (js* (. jQuery get (clj url)
          (fn [data, status, request]
            (. (jQuery (clj selector)) replaceWith data)))))

(defn data-table
  "enables jQuery dataTable plugin on table with id id"
  [id & [options]]
  (js* (.dataTable (jQuery (clj (clj-blocks.js/id id)))
                   (clj options))))
