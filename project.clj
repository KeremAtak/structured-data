(defproject structured-data "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [iloveponies.tests/structured-data "0.1.0-SNAPSHOT"]]
  :profiles {:dev {:dependencies [[midje "1.7.0"]]}}
  :plugins [[lein-midje "3.2-RC4"]])
              