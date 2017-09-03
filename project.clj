(defproject tursas "0.2"
  :description "Simple chess engine which uses XBoard protocol."
  :license {:name "ISC license"
            :url "http://opensource.org/licenses/isc-license.txt"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [matchure "0.10.1"]]
  :main tursas.tursas
  :aot [tursas.tursas])
