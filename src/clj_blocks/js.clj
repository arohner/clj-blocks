(ns clj-blocks.js
  (:use [com.reasonr.scriptjure :only (js js* cljs)])
  (:require [hiccup.core])
  (:use (hiccup page-helpers form-helpers)))

(defn script
  "makes an HTML tag that contains javascript. js should either be a string literal or a fragment that can be compiled with scriptjure"
  [js-code]
  (hiccup.core/html [:script {:type "text/javascript"} (if (seq? js-code)
                                             (cljs js-code)
                                             js-code)]))



(defn post-link
  [link-attrs {form-name :form-name, url :url, text :text, :as args}]
  "makes an <a href> that performs a POST. Creates a hidden form, and a link with javascript that calls form.submit()
   
   required keys: form-name, url, text
   optional key: hidden-input, a map. Each pair will become an input type=hidden on the form"
  (assert form-name)
  (hiccup.core/html 
     [:form {:name form-name :method "POST" :action url :style "display:none"}
      (map (fn [[key val]]
	     (hidden-field key val)) (args :hidden-input))
      (submit-button "submit")]
     [:a (merge link-attrs {:href (format "javascript:document.%s.submit()" form-name)}) text]))

(defn button
  [attrs text js-code]
  [:form {:method "POST" :action "#"}
   [:input (merge 
                  {:type "submit"
                   :onClick (js (clj js-code)
                                (return false))
                   :value text}
                  attrs)]])

(defn html
  "returns the javascript to replace selector with html"
  [selector hiccup-html]
  (js* (.html (jQuery (clj selector)) (clj (hiccup.core/html hiccup-html)))))

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

(defn link
  "makes a link that calls javascript when clicked"
  [attrs text js-code]
  (let [link-id (or (:id attrs) (gensym))
        attrs (assoc attrs :id link-id)
        code (js* (do
                    (clj js-code)
                    (return false)))]
    (hiccup.core/html
     [:a (merge attrs {:href "#"}) text]
     (script (on-ready (on-click (id link-id) code))))))

(defn hidden-div
  "returns a link that when clicked, shows body"
  [{:keys [div-id link-id link-text]} & body]
  (let [div-id (or div-id (gensym "div"))
        link-id (or link-id (gensym "link"))
        link-text (or link-text "Show")
        show-fn-name (gensym "show_fn")
        hide-fn-name (gensym "hide_fn")]
    (hiccup.core/html
     [:div {:id div-id :style "display: none;"}
      (list body)]
     (script
      (js* (fn (clj show-fn-name) [e]
             (clj (show-div (id div-id)))
             (clj (html (id link-id) "Hide"))
             (.unbind (jQuery (clj (id link-id))) "click")
             (.click (jQuery (clj (id link-id))) (clj hide-fn-name)))
           (fn (clj hide-fn-name) [e]
             (clj (hide-div (id div-id)))
             (clj (html (id link-id) link-text))
             (.unbind (jQuery (clj (id link-id))) "click")
             (.click (jQuery (clj (id link-id))) (clj show-fn-name)))))
     [:a {:href "#"
          :id link-id} link-text]
     (script (on-ready (js* (.click (jQuery (clj (id link-id))) (clj show-fn-name))))))))

;; (defn ajax-js
;;   "returns a javascript function that when called, will make an ajax request to call fn, clojure code, on the server. fn should return javascript that will be eval'd by the client.  
;;  Can be used in jQuery event handlers, i.e.

;; (on-change \"#foo\" (ajax-js #(some-clojure-code)) "
;;   [fn])

(defn progress-spinner [selector]
  "replaces selector with an progress spinner"
  (let [html
        (hiccup.core/html [:div {:class "clj-blocks-progress-spinner"} [:img {:src "/gif/progress-spinner.gif"}]])]
    ;; hide the original element, then insert the spinner
    ;; afterwards. If we replace the original element, we would break
    ;; e.g. forms that are about to be submitted.
    (js* (. (jQuery (clj selector)) hide)
         (. (jQuery (clj selector)) after (clj html)))))

(defn cancel-progress-spinner [selector]
  "undos a progress spinner. Pass in the same selector used to create the progress spinner"
  (let [progress-selector (format "%s + \\.%s" selector "clj-blocks-progress-spinner")]
    (js* (do
           (. (jQuery (clj progress-selector)) hide)
           (. (jQuery (clj selector)) show)))))

(defn ajax-post-form [url form-selector on-success]
  "does an AJAX POST to url. form-selector is a jquery selector that identifies the form to serialize. on-success is a js function that gets called with the body on success"
  (js*
   (clj (progress-spinner form-selector))
   (. jQuery post (clj url) (.serialize (jQuery (clj form-selector))) (clj on-success))))

(defn post-data
  "does a post of data to url."
  [url data on-success]
  (js* (.post jQuery (clj url) (clj data) (clj on-success))))

(defn ajax-update [selector url]
  "JS snippet calls jquery .load()"
  (js* (do
         (. (jQuery (clj selector)) load (clj url) (fn []
                                                     (clj (cancel-progress-spinner selector)))
            ))))

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
