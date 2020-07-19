(ns saxon-test
  (:require [clojure.java.io :as io]
            [clojure.string :as st]
            [clojure.test :refer :all]
            [saxon :as s])
  (:import (java.io StringReader ByteArrayInputStream)
           (javax.xml.transform Source ErrorListener TransformerException SourceLocator)
           (net.sf.saxon.s9api XdmNode XsltCompiler MessageListener XsltExecutable SaxonApiException)))

(def xml "<root><a><b/></a></root>")
(def sample-error-listener
  (binding [*out* *err*]
    (reify ErrorListener
      (^void error [_ ^TransformerException e]
        (println (str "Error: " (.getMessageAndLocation e))))
      (^void warning [_ ^TransformerException e]
        (println (str "Warning: " (.getMessageAndLocation e))))
      (^void fatalError [_ ^TransformerException e]
        (println (str "Fatal: " (.getMessageAndLocation e)))
        (throw e)))))

(def sample-message-listener
  (reify MessageListener
    (^void message [_ ^XdmNode content ^boolean terminate ^SourceLocator locator]
      (let [rown (.getLineNumber locator)
            coln (.getColumnNumber locator)
            id (.getPublicId locator)
            sys-id (.getSystemId locator)]
        (println (str "Message received at row: " rown ", col: " coln "from " id ": "
                      content))))))

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

(deftest test-compiler-and-settings
  (testing "Compiler function"
    (is (instance? XsltCompiler (s/compiler)) "It can create a compiler with no special settings")
    (is (instance? XsltCompiler (s/compiler {:params {:test true}}))))
  (testing "Create exe"
    (let [c (s/compiler)
          ss (io/resource "stylesheet-xml-input.xsl")
          ssrc (s/xml-source ss)]
      (is (instance? XsltExecutable (.compile c ssrc)) "It creates an executable")))
  (testing "Load packages"
    (let [pkg-list (mapv io/resource ["counter.sef" "array.sef"])
          compiler (s/compiler {:package-list pkg-list})]
      (is (instance? XsltCompiler compiler))
      (let [ss (-> "test-counter.xsl" io/resource s/xml-source)
            ex (.compile compiler ss)
            xf (.load30 ex)
            inp (-> "xml-input.xml" io/resource s/compile-xml)]
        (is (st/includes? (.toString (.applyTemplates xf inp)) "<Result>1</Result>") "successfully used the package"))))
  (testing "set static parameters"
    ; there's no way to get at the params on XsltCompiler once they're set,
    ; so using the full stylesheet execution to show that they 1. are visible
    ; to the stylesheet, and 2. picked up as static params.
    (let [params {:test true :string "A string"}
          c (s/compiler {:params params})
          ss (-> "stylesheet-xml-input.xsl" io/resource s/xml-source)
          ex (.compile c ss)
          xf (.load30 ex)
          inp (-> "xml-input.xml" io/resource s/compile-xml)]
      (.setMessageListener xf sample-message-listener)
      (let [outp (.applyTemplates xf inp)
            outp-str (.toString outp)]
        (is (instance? XdmNode outp) "it returns an XdmNode (i.e., it works)")
        ; the stylesheet has a use-when attribute on the "test" param, so this
        ; shows it was compiled as expected
        (is (st/includes? outp-str "String contents") "first static param worked")
        (is (st/includes? outp-str "A string") "second param worked")))))
