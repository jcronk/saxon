(ns saxon.core-test
  (:require [clojure.java.io :as io]
            [clojure.string :as st]
            [clojure.test :refer :all]
            [saxon.core :as s])
  (:import java.io.StringReader
           (java.net
             URL
             URI)
           javax.xml.transform.Source
           javax.xml.transform.stream.StreamSource
           (net.sf.saxon.s9api
             XdmNode
             XdmAtomicValue
             XdmValue
             SaxonApiException)))

(def xml "<root><a><b/></a></root>")
(deftest test-compile-xml
  (is (instance? XdmNode (s/compile-xml xml)))
  (is (instance? XdmNode (s/compile-xml (java.io.StringReader. xml))))
  (is (instance? XdmNode (s/compile-xml (java.io.ByteArrayInputStream.
                                          (.getBytes xml "UTF-8")))))
  (is (thrown? SaxonApiException (s/compile-xml (format "%s bad stuff" xml)))))

(deftest test-query
  (is (= 3 (count (s/query "//element()" (s/compile-xml xml))))))

(deftest test-node-path
  (is (= "/root/a[1]/b[1]"
         (->> (s/compile-xml xml)
              (s/query "(//element())[3]")
              s/node-path))))

(deftest test-coercions
  (are [it] (instance? Source (s/as-source it))
       (io/file "a-file")
       "an input string"
       (URL. "file:///dev/null")
       (URI. "file:///dev/null")
       (StringReader. "an input string")
       (StreamSource. (io/file "a-file")))
  (are [it] (instance? XdmValue (s/as-xdmval it))
       "a string"
       (io/file "a-file")
       (XdmAtomicValue. "a string")
       {:key "value" :key2 "value"}))
