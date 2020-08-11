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

(defprotocol S9Destinations
  (unwrap-dest [_dest] "return the underlying value of the Destination"))
(extend-protocol S9Destinations
  XdmDestination
  (unwrap-dest [dest] (.getXdmItem dest))
  RawDestination
  (unwrap-dest [dest] (.getXdmValue dest))
  NullDestination
  (unwrap-dest [dest] nil))

(defn as-dest
  "Create a Destination that applies xf before ending at final"
  [final xf]
  (.asDocumentDestination xf final))

(defn dest-reduce
  "Reduce a collection of transformers into a destination"
  [xf-coll dest]
  (reduce as-dest dest (reverse xf-coll)))

(defn- as-tee-dest
  [xf final addl]
  (.asDocumentDestination xf (TeeDestination. final addl)))

(defn- tee-dest-reducer
  [reduced-dest pair]
  (let [[xf new-dest] pair]
    (as-tee-dest xf reduced-dest new-dest)))

(defn dest-reductions
  "Reduce a collection of transformers.
  Returns a sequence where the first item is the reduced destination and the
  remaining items are destinations holding the intermediate output of each
  transformer.  The final transformation output will be in `dest`"
  [xf-coll dest]
  (let [dests (repeatedly (count xf-coll) #(XdmDestination.))
        final (as-dest dest (last xf-coll))
        pairs (reverse (map vector (butlast xf-coll) (next dests)))
        xform-dest (reduce tee-dest-reducer final pairs)]
    (cons (TeeDestination. xform-dest (first dests)) (vec dests))))

(defn xform-piped
  "Apply templates to a collection of transformers, getting the final output.
  3-arity version takes a function to be applied to each intermediate
  destination."
  ([xf-coll in]
   (let [[primary & others] (map transformer xf-coll)
         dest (XdmDestination.)
         xform-dest (dest-reduce others dest)]
     (.applyTemplates primary in xform-dest)
     (str (.getXdmValue dest))))
  ([xf-coll in f]
   (let [[primary & others] xf-coll
         dest (XdmDestination.)
         {xform-dest :initial
          results :results} (dest-reductions others dest)]
     (.applyTemplates primary in xform-dest)
     (f results)
     (str (.getXdmValue dest)))))
