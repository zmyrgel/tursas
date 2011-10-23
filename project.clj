(defproject tursas "0.2"
  :description "Simple chess engine which uses XBoard protocol."
  :license {:name "ISC license"
            :url "http://opensource.org/licenses/isc-license.txt"
            :distribution :repo}
  :dev-dependencies [[swank-clojure "1.3.4-SNAPSHOT"]]
  :warn-on-reflection false
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [matchure "0.10.1"]]
  :main tursas.core)
