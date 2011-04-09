(defproject clj-blocks
  "0.0.23"
  :description "a web framework inspired by Weblocks"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [clout "0.4.1"]                 
                 [ring/ring "0.3.7"]
                 [hiccup/hiccup "0.3.4"]
                 [compojure "0.6.2"]
                 [scriptjure "0.1.22"]]
  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]
                     [jline "0.9.94"]
                     [ring-mock "0.1.1"]])