(defproject tursas "0.2"
  :description "Chess AI engine supporting UCI and XBoard protocol."
  :license {:name "ISC license"
            :url "http://opensource.org/licenses/isc-license.txt"
            :distribution :repo}
  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]]
  :warn-on-reflection false
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [matchure "0.10.1"]]
  :main tursas.core)
