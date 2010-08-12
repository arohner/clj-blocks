(defproject clj-blocks
  "0.0.9"
  :description "a web framework inspired by Weblocks"
  :dependencies [[sandbar "0.2.3" :exclusions [hiccup ring/ring-core sandbar-session compojure]]
                 [sandbar/sandbar-core "0.3.0" :exclusions [hiccup]]
                 [org.clojars.arohner/sandbar-session "cd7bb1a8ad844dbef623a76353a6529ed3ce1f67"]
                 [org.clojars.arohner/hiccup "9bab3bd8bedbfcbd13e2ba278798615dd2895346"]
                 [clout "0.2.0"]
                 [org.clojars.arohner/compojure "41a61347414ac54eadc496fb5bb7f4b25955cd4c"]
		 [org.clojars.arohner/ring-core "f47973382a035d67bd8f9d2890de40aabd65b45f" :exclusions [hiccup compojure]]
		 [ring/ring-devel "0.2.3" :exclusions [ring/ring-core hiccup compojure]]
		 [ring/ring-jetty-adapter "0.2.3" :exclusions [ring/ring-core]]
                 [scriptjure "0.1.9"]]
  :dev-dependencies [[leiningen/lein-swank "1.1.0"]
		     [swank-clojure "1.2.0-SNAPSHOT"]
		     [jline "0.9.94"]])