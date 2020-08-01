(require 'cemerick.pomegranate.aether)
(cemerick.pomegranate.aether/register-wagon-factory!
  "http" #(org.apache.maven.wagon.providers.http.HttpWagon.))
(defproject clojure-saxon "0.12.1-SNAPSHOT"
  :description "Clojure wrapper for the Saxon XSLT and XQuery processor."
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [net.sf.saxon/SaxonEE "9.9.1.7"]]
  :target-path "target/%s"
  :deploy-repositories [["releases" {:url "http://cin-build-1:8081/artifactory/libs-release-local/"
                                     :sign-releases false
                                     :username :env/ARTIFACTORY_USER
                                     :password :env/ARTIFACTORY_PASS}]
                        ["snapshots" {:url "http://cin-build-1:8081/artifactory/libs-snapshot-local/"
                                      :sign-releases false
                                      :username :env/ARTIFACTORY_USER
                                      :password :env/ARTIFACTORY_PASS}]]
  :aliases {"release-prepare!" ["do"
                                ["vcs" "assert-committed"]
                                ["change" "version" "leiningen.release/bump-version" "release"]
                                ["vcs" "commit"]
                                ["vcs" "tag" "--no-sign"]
                                ["uberjar"]]
            "release-deploy!" ["deploy" "releases" "clojure-saxon/saxon-clj" :project/version "./target/saxon-clj.jar"]
            "release-push!" ["do"
                             ["change" "version" "leiningen.release/bump-version"]
                             ["vcs" "commit"]
                             ["vcs" "push"]]}
  :profiles {:uberjar {:aot :all}}
  :repl-options {:init-ns saxon.xslt}
  :resource-paths ["resources" "test/resources"])
