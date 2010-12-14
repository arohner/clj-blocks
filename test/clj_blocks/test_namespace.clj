(ns clj-blocks.test-namespace
  (:use clj-blocks.routes))

(defroutefn foo [:get "/foo/:id"] [request])