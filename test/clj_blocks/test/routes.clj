(ns clj-blocks.test.routes
  (:use clojure.test)
  (:use clj-blocks.routes))

(deftest path-for-works
  (is (= "/foo/42" (path-for clj-blocks.test-namespace/foo {:id 42}))))