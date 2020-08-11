(ns saxon.xslt-test
  (:require [clojure.java.io :as io]
            [clojure.string :as st]
            [clojure.test :refer :all]
            [saxon.core :as s]
            [saxon.xslt :as xsl])
  (:import (javax.xml.transform
             ErrorListener
             TransformerException
             SourceLocator)
           (net.sf.saxon.s9api
             XdmNode
             XdmDestination
             XsltCompiler
             MessageListener
             XsltExecutable)))

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
    (^void message [_
                    ^XdmNode content
                    ^boolean _terminate
                    ^SourceLocator locator]
      (let [rown (.getLineNumber locator)
            coln (.getColumnNumber locator)
            id (.getPublicId locator)
            _sys-id (.getSystemId locator)]
        (println (str "Message received at row: " rown ", col: "
                      coln "from " id ": " content))))))

(deftest test-compiler-and-settings
  (testing "Compiler function"
    (is (instance? XsltCompiler (xsl/compiler))
        "It can create a compiler with no special settings")
    (is (instance? XsltCompiler (xsl/compiler {:params {:test true}}))))

  (testing "Create exe"
    (let [c (xsl/compiler)
          ss (io/resource "stylesheet-xml-input.xsl")
          ssrc (s/xml-source ss)]
      (is (instance? XsltExecutable (.compile c ssrc))
          "It creates an executable")))

  (testing "Load packages"
    (let [pkg-list (mapv io/resource ["counter.sef"])
          compiler (xsl/compiler {:package-list pkg-list})]
      (is (instance? XsltCompiler compiler))
      (let [ss (io/resource "test-counter.xsl")
            xf (xsl/transformer compiler ss)
            inp (io/resource "xml-input.xml")]
        (is (st/includes?
              (.toString (xsl/apply-templates xf inp))
              "<Result>1</Result>")
            "successfully used the package"))))

  (testing "set static parameters"
    ; there's no way to get at the params on XsltCompiler once they're set,
    ; so using the full stylesheet execution to show that they 1. are visible
    ; to the stylesheet, and 2. picked up as static params.
    (let [params {:test true :string "A string"}
          c (xsl/compiler {:params params})
          ss (io/resource "stylesheet-xml-input.xsl")
          xf (xsl/transformer c ss)
          inp (io/resource "xml-input.xml")
          outp (xsl/apply-templates xf inp)
          outp-str (.toString outp)]
      (is (instance? XdmNode outp)
          "it returns an XdmNode (i.e., it works)")
      ; the stylesheet has a use-when attribute on the "test" param, so this
      ; shows it was compiled as expected
      (is (st/includes? outp-str "String contents")
          "first static param worked")
      (is (st/includes? outp-str "A string")
          "second param worked"))))

(deftest test-transformer-and-settings
  (let [compiler (xsl/compiler)]
    (testing "Set listeners"
      (let [params {:msg-listener sample-message-listener
                    :err-listener sample-error-listener}
            ss (io/resource "stylesheet-init-template.xsl")
            xfrm (xsl/transformer compiler ss)]
        (xsl/set-transformer-properties! xfrm params)
        (is (identical? (.getMessageListener xfrm) sample-message-listener)
            "it set the message listener")
        (is (identical? (.getErrorListener xfrm) sample-error-listener)
            "it set the error listener")))

    (testing "Set stylesheet params"
      (let [params {:ssheet-params {:ss-param "global param"}}
            ss (io/resource "stylesheet-init-template.xsl")
            xfrm (xsl/transformer compiler ss)]
        (xsl/set-transformer-properties! xfrm params)
        (is (st/includes?
              (.callTemplate xfrm (s/qname "main"))
              "global param"))))

    (testing "Set template params"
      (let [params {:init-template-params {:input "changed" :tunnel false}}
            ss (io/resource "stylesheet-init-template.xsl")
            xfrm (xsl/transformer compiler ss)]
        (xsl/set-transformer-properties! xfrm params)
        (is (st/includes? (.callTemplate xfrm (s/qname "main")) "changed"))))))

(deftest test-stylesheet-chaining
  (let [compiler (xsl/compiler)
        ss-list (->> ["A.xsl" "B.xsl" "C.xsl" "D.xsl" "E.xsl"]
                     (map #(io/resource %))
                     (map #(xsl/transformer compiler %)))
        inp (s/as-source (io/resource "xml-input.xml"))
        dest (XdmDestination.)
        chain (xsl/chain ss-list dest)]
    (testing "invocation by apply-templates"
      (.applyTemplates (first ss-list) inp chain)
      (is (= (.toString (.getXdmNode dest))
             "That was a successful test! Hooray!")
          "All five transformations were applied"))
    (testing "invocation by call-template"
      (let [ss (first ss-list)
            params {:init-template-params {:test "I know"}}]
        (xsl/set-transformer-properties! ss params)
        (.callTemplate ss (s/qname "main") chain)
        (is (= (.toString (.getXdmNode dest))
               "I know that it was a successful test! Hooray!")
            (str "It called the template, used the param, then applied the "
                 "remaining four stylesheets"))))))

(deftest test-stylesheet-chaining-with-results
  (let [compiler (xsl/compiler)
        ss-list (->> ["A.xsl" "B.xsl" "C.xsl" "D.xsl"]
                     (map #(io/resource %))
                     (map #(xsl/transformer compiler %)))
        inp (s/as-source (io/resource "xml-input.xml"))
        dest (XdmDestination.)
        {initial :initial-dest
         results :results} (xsl/chain-with-results ss-list dest)]
    (.applyTemplates (first ss-list) inp initial)
    (let [[a b c d] (map #(.toString (.getXdmNode %)) (conj (vec others) dest))]
      (is (st/includes? a "That is a test"))
      (is (st/includes? b "That was a test"))
      (is (st/includes? c "a successful test"))
      (is (st/includes? d "Hooray!")))))
