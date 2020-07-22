(ns saxon.xslt
  "Clojure Saxon wrapper"
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as st]
            [saxon.core :refer :all])
  (:import (java.io File InputStream OutputStream Reader StringReader Writer)
           (java.net URL URI)
           (javax.xml.transform Source ErrorListener TransformerException)
           (javax.xml.transform.stream StreamSource)
           (net.sf.saxon.lib FeatureKeys Feature)
           (net.sf.saxon.om NodeInfo)
           (net.sf.saxon.s9api XsltCompiler XsltTransformer Axis Destination Processor Serializer
                               Serializer$Property XPathCompiler MessageListener XPathSelector
                               XdmDestination XdmValue XdmItem XdmNode XdmNodeKind
                               XdmAtomicValue XdmMap XQueryCompiler XQueryEvaluator QName)
           (net.sf.saxon.tree.util Navigator)))

; XsltCompiler manipulation functions
(defn- set-compiler-params
  [compiler params]
  (let [pconv (to-params params)]
    (doseq [[qn av] pconv]
      (.setParameter compiler qn av))))

(defn- import-packages
  [^XsltCompiler compiler package-list]
  (doseq [file (map str package-list)]
    (let [pkg-uri (URI. file)
          package (.loadLibraryPackage compiler pkg-uri)]
      (.importPackage compiler package))))

(defn compiler
  "Return an XsltCompiler. Options:
    package-list: list of compiled packages to import, by filename or URL
    params: map of static parameters to set"
  [& opts]
  (let [^XsltCompiler compiler (.newXsltCompiler proc)
        [{package-list :package-list
          params :params}] opts]
    (cond-> compiler
      package-list (import-packages package-list)
      params (set-compiler-params params))
    compiler))

(defn- convert-template-params
  [params]
  (let [{tunnel :tunnel} params
        tparams (dissoc params :tunnel)]
    {:tunnel (true? tunnel)
     :tparams (to-params params)}))

(defn- set-init-template-params
  [xfrmr params]
  (let [{params :tparams
         tunnel :tunnel}
          (convert-template-params params)]
    (.setInitialTemplateParameters xfrmr params tunnel)))

(defn set-transformer-properties!
  "Set properties on the compiled and loaded XsltTransfomer30 object
    msg-listener, err-listener, and uri-resolver: MessageListener, ErrorListener,
      and URIResolvers, if not using the defaults
    ssheet-params: Global stylesheet params as a map (currently this will only
      work with string values)
    init-template-params: Parameters to be passed into the initial template (as
      a map); this includes an optional boolean :tunnel param"
  [xfrmr props]
  (let [{msg-listener :msg-listener
         err-listener :err-listener
         uri-resolver :uri-resolver
         ssheet-params :ssheet-params
         init-tmpl-params :init-template-params} props]
    ; I don't have a clue why this works and the below doesn't, but okay
    (when (instance? MessageListener msg-listener) (.setMessageListener xfrmr msg-listener))
    (cond-> xfrmr
      ; msg-listener (.setMessageListener msg-listener)
      err-listener (.setErrorListener err-listener)
      uri-resolver (.setURIResolver uri-resolver)
      ssheet-params (.setStylesheetParameters (to-params ssheet-params))
      init-tmpl-params (set-init-template-params init-tmpl-params)) xfrmr))

(defn transformer
  "Compile and load a stylesheet, returning an Xslt30Transformer"
  [compiler ss]
  (.load30 (.compile compiler (xml-source ss))))

(defn apply-templates
  ([xform input]
   (->> input
        compile-xml
        (.applyTemplates xform)
        unwrap-xdm-items))
  ([xform input output]
   (let [result (apply-templates xform input)]
     (spit output result))))

(defn call-template
  "Call a named template.  tmpl-name must be the output of the qname function"
  [xform tmpl-name]
  (unwrap-xdm-items (.callTemplate xform tmpl-name)))

(defn compile-ss
  "Compiles stylesheet (from anything convertible to javax.
  xml.transform.Source), returns function that applies it to
  compiled doc or node."
  [f]
  (let [cmplr (.newXsltCompiler proc)
        exe (.compile cmplr (xml-source f))]

    (fn [^XdmNode xml & params]
      (let [xdm-dest (XdmDestination.)
            transformer (.load exe)] ; created anew, is thread-safe
        (when params
          (let [prms (first params)
                ks (keys prms)]
            (doseq [k ks]
              (.setParameter transformer
                             (QName. ^String (name k))
                             (XdmAtomicValue. (k prms))))))
        (doto transformer
          (.setInitialContextNode xml)
          (.setDestination xdm-dest)
          (.transform))
        (.getXdmNode xdm-dest)))))
