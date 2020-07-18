(ns saxon-test
  (:require [clojure.java.io :as io]
            [clojure.string :as st]
            [clojure.test :refer :all]
            [saxon :as s])
  (:import (java.io StringReader ByteArrayInputStream)
           (javax.xml.transform Source ErrorListener TransformerException)
           (net.sf.saxon.s9api XdmNode XsltCompiler XsltExecutable SaxonApiException)))

(def xml "<root><a><b/></a></root>")
(defn sample-error-listener
  []
  (binding [*out* *err*])
  (reify ErrorListener
    (^void error [_ ^TransformerException e]
      (println (str "Error: " (.getMessageAndLocation e))))
    (^void warning [_ ^TransformerException e]
      (println (str "Warning: " (.getMessageAndLocation e))))
    (^void fatalError [_ ^TransformerException e]
      (println (str "Fatal: " (.getMessageAndLocation e)))
      (throw e))))

(deftest test-compile-xml
  (is (instance? XdmNode (s/compile-xml xml)))
  (is (instance? XdmNode (s/compile-xml (java.io.StringReader. xml))))
  (is (instance? XdmNode (s/compile-xml (java.io.ByteArrayInputStream. (.getBytes xml "UTF-8")))))
  (is (thrown? SaxonApiException (s/compile-xml (format "%s bad stuff" xml)))))

(deftest test-query
  (is (= 3 (count (s/query "//element()" (s/compile-xml xml))))))

(deftest test-node-path
  (is (= "/root/a[1]/b[1]"
         (->> (s/compile-xml xml)
              (s/query "(//element())[3]")
              s/node-path))))

(deftest test-object-creation
  (testing "Compiler function"
    (is (instance? XsltCompiler (s/compiler)) "It can create a compiler with no special settings")
    (is (instance? XsltCompiler (s/compiler {:params {:test true}}))))
  (testing "Create exe"
    (let [c (s/compiler)
          ss (io/resource "stylesheet-xml-input.xsl")
          ssrc (s/xml-source ss)]
      (is (instance? XsltExecutable (.compile c ssrc)) "It creates an executable")))
  (testing "Load packages"
    (let [pkg-list (mapv io/resource ["counter.sef" "array.sef"])]
      (is (instance? XsltCompiler (s/compiler {:package-list pkg-list})))))
  (testing "set static parameters"
    (let [params {:test true :string "A string"}
          c (s/compiler {:params params})
          ss (-> "stylesheet-xml-input.xsl" io/resource s/xml-source)
          ex (.compile c ss)
          xf (.load30 ex)
          inp (-> "xml-input.xml" io/resource s/compile-xml)]

      (let [outp (.applyTemplates xf inp)
            outp-str (.toString outp)]
        (is (instance? XdmNode outp) "it returns an XdmNode (i.e., it works)")
        (is (st/includes? outp-str "String contents") "first static param worked")
        (is (st/includes? outp-str "A string") "second param worked")))))
