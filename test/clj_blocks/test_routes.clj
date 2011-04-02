(ns clj-blocks.test-routes
  (:use clojure.test)
  (:use clj-blocks.routes)
  (:use ring.mock.request))

(deftest ns-routes-returns-fn
  (is (fn? (ns-routes clj-blocks.routes-ns-a))))

(deftest no-args-works
  (let [handler (ns-routes clj-blocks.routes-ns-a)
        response (handler (request :get "/no-args"))]
    (is (= ":no-args" (:body response)))))

(deftest vector-args-works
  (let [handler (ns-routes clj-blocks.routes-ns-a)
        response (handler (-> (request :get "/one-arg") (assoc :params {:x 42})))]
    (is (= "42" (:body response)))))

(deftest url-arg-works
  (let [handler (ns-routes clj-blocks.routes-ns-a)
        response (handler (-> (request :get "/foo/10")))]
    (is (= "10" (:body response)))))

(deftest map-arg-works
  (let [handler (ns-routes clj-blocks.routes-ns-a)
        response (handler (-> (request :get "/map-args") (assoc :params {:foo 42})))]
    (is (= "42" (:body response)))))

(deftest regex-route-doesnt-match-invalid-regex
  (let [handler (ns-routes clj-blocks.routes-ns-a)
        response (handler (request :get "/regex/invalid"))]
    (is (nil? response))))

(deftest regex-route-matches-valid-regex
  (let [handler (ns-routes clj-blocks.routes-ns-a)
        response (handler (request :get "/regex/100"))]
    (is (= "regex 100") (:body response))))
