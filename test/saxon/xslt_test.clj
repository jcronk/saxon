(ns saxon.xslt-test
  (:require [clojure.java.io :as io]
            [clojure.string :as st]
            [clojure.test :refer :all]
            [saxon.core :as s]
            [saxon.xslt :as xsl])
  (:import (java.io StringReader ByteArrayInputStream)
           (javax.xml.transform Source ErrorListener TransformerException SourceLocator)
           (net.sf.saxon.s9api XdmNode XsltCompiler MessageListener XsltExecutable SaxonApiException)))

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

(deftest test-compiler-and-settings
  (testing "Compiler function"
    (is (instance? XsltCompiler (xsl/compiler)) "It can create a compiler with no special settings")
    (is (instance? XsltCompiler (xsl/compiler {:params {:test true}}))))

  (testing "Create exe"
    (let [c (xsl/compiler)
          ss (io/resource "stylesheet-xml-input.xsl")
          ssrc (s/xml-source ss)]
      (is (instance? XsltExecutable (.compile c ssrc)) "It creates an executable")))

  (testing "Load packages"
    (let [pkg-list (mapv io/resource ["counter.sef" "array.sef"])
          compiler (xsl/compiler {:package-list pkg-list})]
      (is (instance? XsltCompiler compiler))
      (let [ss (io/resource "test-counter.xsl")
            xf (xsl/transformer compiler ss)
            inp (io/resource "xml-input.xml")]
        (is (st/includes? (.toString (xsl/apply-templates xf inp)) "<Result>1</Result>") "successfully used the package"))))

  (testing "set static parameters"
    ; there's no way to get at the params on XsltCompiler once they're set,
    ; so using the full stylesheet execution to show that they 1. are visible
    ; to the stylesheet, and 2. picked up as static params.
    (let [params {:test true :string "A string"}
          c (xsl/compiler {:params params})
          ss (io/resource "stylesheet-xml-input.xsl")
          xf (xsl/transformer c ss)
          inp (io/resource "xml-input.xml")]
      (let [outp (xsl/apply-templates xf inp)
            outp-str (.toString outp)]
        (is (instance? XdmNode outp) "it returns an XdmNode (i.e., it works)")
        ; the stylesheet has a use-when attribute on the "test" param, so this
        ; shows it was compiled as expected
        (is (st/includes? outp-str "String contents") "first static param worked")
        (is (st/includes? outp-str "A string") "second param worked")))))

(deftest test-transformer-and-settings
  (let [compiler (xsl/compiler)]
    (testing "Set listeners"
      (let [params {:msg-listener sample-message-listener
                    :err-listener sample-error-listener}
            ss (io/resource "stylesheet-init-template.xsl")
            xfrm (xsl/transformer compiler ss)]
        (xsl/set-transformer-properties! xfrm params)
        (is (identical? (.getMessageListener xfrm) sample-message-listener) "it set the message listener")
        (is (identical? (.getErrorListener xfrm) sample-error-listener) "it set the error listener")))

    (testing "Set stylesheet params"
      (let [params {:ssheet-params {:ss-param "global param"}}
            ss (io/resource "stylesheet-init-template.xsl")
            xfrm (xsl/transformer compiler ss)]
        (xsl/set-transformer-properties! xfrm params)
        (is (st/includes? (.callTemplate xfrm (s/qname "main")) "global param"))))

    (testing "Set template params"
      (let [params {:init-template-params {:input "changed" :tunnel false}}
            ss (io/resource "stylesheet-init-template.xsl")
            xfrm (xsl/transformer compiler ss)]
        (xsl/set-transformer-properties! xfrm params)
        (is (st/includes? (.callTemplate xfrm (s/qname "main")) "changed"))))))