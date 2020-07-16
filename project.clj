(defproject clojure-saxon "0.9.4"
  :description "Clojure wrapper for the Saxon XSLT and XQuery processor."
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :resource-paths ["resources/saxon9ee.jar" "resources/saxon-license.lic"])
