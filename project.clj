(defproject clj-blocks
  "0.0.5"
  :description "a web framework inspired by Weblocks"
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
                 [sandbar "0.2.3"]
                 [org.clojars.arohner/hiccup "44cbc6e1a1534d1ad32b8bbdf6424d3ecd788192"]
                 [clout "0.2.0"]
                 [org.clojars.arohner/compojure "41a61347414ac54eadc496fb5bb7f4b25955cd4c"]
		 [ring/ring-core "0.2.0"]
		 [ring/ring-devel "0.2.0"]
		 [ring/ring-jetty-adapter "0.2.0"]
                 [scriptjure "0.1.9"]]
  :dev-dependencies [[leiningen/lein-swank "1.1.0"]
		     [swank-clojure "1.2.0-SNAPSHOT"]
		     [jline "0.9.94"]])