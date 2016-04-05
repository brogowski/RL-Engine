(defproject rl-engine "0.0.0-SNAPSHOT"
  :description "Rougelike game engine"
  :url "https://github.com/brogowski/RL-Engine"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot rl-engine.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
