(ns saxon.xslt
  "Clojure Saxon wrapper"
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as st]
            [saxon.core :as sx])
  (:import java.net.URI
           (net.sf.saxon.s9api
             XsltCompiler
             MessageListener
             XdmDestination
             RawDestination
             TeeDestination
             NullDestination)))

; XsltCompiler manipulation functions
(defn set-compiler-params!
  "Set params on the compiler. Setting them here allows them to be used by the
  stylesheet as static params"
  [^XsltCompiler compiler params]
  (let [pconv (sx/to-params params)]
    (doseq [[qn av] pconv]
      (.setParameter compiler qn av))))

(defn import-packages!
  "Import `package-list` of .sef files into compiler's package library"
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
  (let [^XsltCompiler compiler (.newXsltCompiler sx/proc)
        [{package-list :package-list
          params :params}] opts]
    (cond-> compiler
      package-list (import-packages! package-list)
      params (set-compiler-params! params))
    compiler))

(defn- convert-template-params
  [params]
  (let [{tunnel :tunnel} params
        tparams (dissoc params :tunnel)]
    {:tunnel (true? tunnel)
     :tparams (sx/to-params params)}))

(defn set-init-template-params!
  "Set params on initial template for xfrmr. For tunnel params,
  include `:tunnel true` in the params map"
  [xfrmr params]
  (let [{params :tparams
         tunnel :tunnel}
          (convert-template-params params)]
    (.setInitialTemplateParameters xfrmr params tunnel)))

(defn set-transformer-properties!
  "Set properties and params on an Xslt30Transformer instance.
  Properties are :msg-listener, :err-listener, and :uri-resolver
  Params are :ssheet-params and :init-template-params - these are both
  maps."
  [xfrmr props]
  (let [{msg-listener :msg-listener
         err-listener :err-listener
         uri-resolver :uri-resolver
         ssheet-params :ssheet-params
         init-tmpl-params :init-template-params} props]
    ; I don't have a clue why this works and the below doesn't, but okay
    (when
      (instance? MessageListener msg-listener)
      (.setMessageListener xfrmr msg-listener))
    (cond-> xfrmr
      ; msg-listener (.setMessageListener msg-listener)
      err-listener (.setErrorListener err-listener)
      uri-resolver (.setURIResolver uri-resolver)
      ssheet-params (.setStylesheetParameters (sx/to-params ssheet-params))
      init-tmpl-params (set-init-template-params! init-tmpl-params)) xfrmr))

(defn transformer
  "Compile and load a stylesheet, returning an Xslt30Transformer"
  ([ss]
   (transformer (compiler) ss))
  ([compiler ss]
   (.load30 (.compile compiler (sx/as-source ss)))))

(defn apply-templates
  "Use xform to apply templates to XML input."
  ([xform input]
   (->> input
        sx/as-source
        (.applyTemplates xform)
        sx/unwrap-xdm-items))
  ([xform input dest]
   (->> input
        sx/as-source
        (.applyTemplates xform dest))
   (sx/unwrap-xdm-items (.getXdmNode dest))))

(defn call-template
  "Call a named template.  tmpl-name must be the output of the qname function"
  ([xform tmpl-name]
   (sx/unwrap-xdm-items (.callTemplate xform tmpl-name)))
  ([xform tmpl-name dest]
   (.callTemplate xform tmpl-name dest)
   (sx/unwrap-xdm-items (.getXdmNode dest))))

(defn chain
  "Combine a collection of XsltTransformer objects into a destination.

  This is for scenarios where there is a series of stylesheets, and the output of
  the first stylesheet needs to go into the output of the second stylesheet, whose
  output needs to go into the third stylesheet, etc., until you have the final output.

  Pass in the full set of stylesheets as compiled and loaded transformers, and create
  a Destination object that will hold the final output.  This function creates the
  Destination to be passed to the first transformer.  Example:

  (let [col (map transformer ss-file-list)
        dest (XdmDestination.)
        chained (chain col dest)]
    (.applyTemplates (first col) input-file chained)
    (.getXdmNode dest))
"
  [ss-coll dest]
  (let [[head & tail] ss-coll]
    (reduce #(.asDocumentDestination %2 %1) dest (reverse tail))))

(defn- tee-destination
  [ss dest1 dest2]
  (.asDocumentDestination ss (TeeDestination. dest1 dest2)))

(defn chain-with-results
  "As chain, but returns a map containing the various Destination objects that are needed to
  get the intermediate results.

    :first-dest
    :results

  (let [col (map transformer ss-file-list)
        dest (XdmDestination.)
        { initial-dest :initial-dest
          results :results } (chain-with-results col dest)]
    (.applyTemplates (first col) input-file initial-dest)
    (.getXdmNode dest) ; final result
    (map #(.getXdmNode) results)) ; seq of intermediate results, in order
"
  [ss-coll dest]
  (let [dests (repeatedly (-> ss-coll count dec) #(XdmDestination.))
        paired (map vector (butlast ss-coll) dests)
        [xfm-first dest-first] (first paired)
        to-reduce (rest paired)
        final-dest (.asDocumentDestination (last ss-coll) dest)
        reduction (reduce #(tee-destination (first %2) (last %2) %1) final-dest to-reduce)]
    {:initial-dest (TeeDestination. dest-first reduction)
     :results dests}))
