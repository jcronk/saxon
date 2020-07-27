(defproject clojure-saxon "0.11.0"
  :description "Clojure wrapper for the Saxon XSLT and XQuery processor."
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [net.sf.saxon/SaxonEE "9.9.1.7"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :repl-options {:init-ns saxon.xslt}
  :resource-paths ["resources" "test/resources"])
