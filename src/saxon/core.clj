; Copyright © March 2009, Perry Trolard, Washington University in Saint Louis
;
; The use and distribution terms for this software are covered by the MIT
; Licence (http://opensource.org/licenses/mit-license.php), which can be found
; in the file MIT.txt at the root of this distribution. Use of the software
; counts as agreeing to be bound by the terms of this license. You must not
; remove this notice from this software.

(ns saxon.core
  (:require [clojure.string :as st])
  (:import (java.io File InputStream OutputStream Reader StringReader Writer)
           (java.net URL URI)
           (java.util Map)
           (javax.xml.transform Source)
           (javax.xml.transform.stream StreamSource)
           (net.sf.saxon.lib FeatureKeys Feature)
           (net.sf.saxon.s9api Destination Processor Serializer Serializer$Property XPathCompiler XdmValue XdmItem XdmNode XdmNodeKind XdmAtomicValue XdmMap XQueryCompiler QName)
           (net.sf.saxon.tree.util Navigator)))
;;
;; Utilities
;;

(def ^{:tag Processor}
  proc
  "Returns the Saxon Processor object, the thread-safe generator class for documents,
  stylesheets, & XPaths. Creates & defs the Processor if not already created."
  (Processor. true))

(defn- upper-snake-case
  "Extracted function for converting kebab case to SCREAMING_SNAKE_CASE"
  [s]
  (-> s name st/upper-case (st/replace "-" "_")))

(defn qname
  "Easily create a QName - you can pass in just the name, the name + ns-uri, or
  the name + ns-uri + ns-prefix"
  ([^String nm] (QName. nm))
  ([^String uri ^String nm] (QName. uri nm))
  ([^String pfx ^String uri ^String nm] (QName. pfx uri nm)))

(defn set-config-property!
  "Sets a configuration property on the Saxon Processor object. Takes keyword
  representing a net.sf.saxon.FeatureKeys field and the value to be set, e.g.
    (set-config-property! :line-numbering true)
  Lets errors bubble up."
  [prop value]
  (let [prop (upper-snake-case prop)
        ^Feature field (.get (.getField FeatureKeys prop) nil)]
    (.setConfigurationProperty proc field value)))

(defprotocol Coercions
  "Coerce between 'resource-namish' things"
  (^javax.xml.transform.Source as-source [x] "Coerce argument to a StreamSource")
  (^net.sf.saxon.s9api.XdmValue as-xdmval [x] "Coerce argument to an XdmValue"))

(extend-protocol Coercions
  nil
  (as-source [_] nil)
  (as-xdmval [_] nil)

  String
  (as-source [s] (StreamSource. (StringReader. s)))
  (as-xdmval [s] (XdmAtomicValue. s))

  File
  (as-source [f] (StreamSource. f))
  (as-xdmval [f] (XdmAtomicValue. (str f)))

  Boolean
  (as-xdmval [b] (XdmAtomicValue. b))

  InputStream
  (as-source [i] (StreamSource. i))

  URL
  (as-source [u] (StreamSource. (.openStream u)))

  URI
  (as-source [u] (StreamSource. (.openStream (.toURL u))))

  Reader
  (as-source [r] (StreamSource. r))

  Source
  (as-source [s] s)

  XdmValue
  (as-xdmval [x] x)

  Map
  (as-xdmval [m] (letfn [(reducer [r k v]
                           (assoc r (-> k name as-xdmval) (as-xdmval v)))]
                   (XdmMap. ^java.util.Map (reduce-kv reducer {} m)))))

(defn to-params
  "Convert a map of parameters into the QName/XdmValue map that Saxon is expecting"
  [params]
  (letfn [(conv-pair
            [[k v]]
            (vector (QName. ^String (name k)) (as-xdmval v)))]
    (into {} (map conv-pair params))))

(defmulti ^Source xml-source class)
(defmethod xml-source File
  [f]
  (StreamSource. ^File f))
(defmethod xml-source InputStream
  [i]
  (StreamSource. ^InputStream i))
(defmethod xml-source URL
  [u]
  (StreamSource. ^InputStream (.openStream ^URL u)))
(defmethod xml-source Reader
  [r]
  (StreamSource. ^Reader r))
(defmethod xml-source String
  [s]
  (StreamSource. (StringReader. ^String s)))
(defmethod xml-source XdmNode
  [nd]
  (.asSource ^XdmNode nd))

;; Well, except this is public -- maybe doesn't need to be

(defn atomic?
  "Returns true if XdmItem or a subclass (XdmAtomicValue, XdmNode) is an atomic value."
  [^XdmItem val]
  (.isAtomicValue val))

(defn unwrap-xdm-items
  "Makes XdmItems Clojure-friendly. A Saxon XdmItem is either an atomic value
  (number, string, URI) or a node.

  This function returns an unwrapped item or a sequence of them, turning XdmAtomicValues
  into their corresponding Java datatypes (Strings, the numeric types), leaving XdmNodes
  as nodes."
  [sel]
  (let [result
          (map #(if (atomic? %) (.getValue ^XdmAtomicValue %) %)
               sel)]
    (if (next result)
      result
      (first result))))

;;
;; Public functions
;;

(defn compile-xml
  "Compiles XML into an XdmNode, the Saxon
  currency for in-memory tree representation. Takes
  File, URL, InputStream, Reader, or String."
  {:tag XdmNode}
  [x]
  (.. proc (newDocumentBuilder)
      (build (xml-source x))))

(defn as-xpath-map
  [mp]
  (letfn [(reducer [r [k v]]
            (assoc r (-> k name XdmAtomicValue.) (XdmAtomicValue. v)))]
    (XdmMap. ^java.util.Map (reduce-kv reducer {} mp))))

(defn compile-xpath
  "Compiles XPath expression (given as string), returns
  function that applies it to compiled doc or node. Takes
  optional map of prefixes (as keywords) and namespace URIs."
  [^String xpath & ns-map]
  (let [cmplr (doto (.newXPathCompiler proc)
                (#(doseq [[pre uri] (first ns-map)]
                    (.declareNamespace ^XPathCompiler % (name pre) uri))))
        exe (.compile cmplr xpath)]

    (fn [^XdmNode xml]
      (unwrap-xdm-items
        (doto (.load exe)
          (.setContextItem xml))))))

(defn compile-xquery
  "Compiles XQuery expression (given as string), returns
  function that applies it to compiled doc or node. Takes
  optional map of prefixes (as keywords) and namespace URIs."
  [^String xquery & ns-map]
  (let [cmplr (doto (.newXQueryCompiler proc)
                (#(doseq [[pre uri] (first ns-map)]
                    (.declareNamespace ^XQueryCompiler % (name pre) uri))))
        exe (.compile cmplr xquery)]

    (fn [^XdmNode xml]
      ; TODO add variable support
      ;(.setExternalVariable ^Qname name ^XdmValue val)
      (unwrap-xdm-items
        (doto (.load exe)
          (.setContextItem xml))))))

;; memoize compile-query funcs, create top-level query func

;; redef, decorate-with are copyright (c) James Reeves. All rights reserved.
;; Taken from compojure.control; Compojure: http://github.com/weavejester/compojure
; (defmacro redef
;   "Redefine an existing value, keeping the metadata intact."
;   {:private true}
;   [name value]
;   `(let ['m (meta '~name)
;          'v (def ~name ~value)]
;      (alter-meta! 'v merge 'm)
;      'v))

; (defmacro decorate-with
;   "Wrap multiple functions in a decorator."
;   {:private true}
;   [decorator & funcs]
;   `(do ~@(for ['f funcs]
;            `(redef '~f (~decorator '~f)))))

; (decorate-with memoize compile-xpath compile-xquery)

(defn query
  "Run query on node. Arity of two accepts (1) string or compiled query fn & (2) node;
  arity of three accepts (1) string query, (2) namespace map, & (3) node."
  ([q nd] ((if (fn? q) q (compile-xquery q)) nd))
  ([q nses nd] ((compile-xquery q nses) nd)))

; (definline with-default-ns
;   "Returns XQuery string with nmspce declared as default element namespace."
;   [nmspce q] `(format "declare default element namespace '%s'; %s" ~nmspce ~q))

;; Serializing

(defn- write-value
  [^XdmValue node ^Destination serializer]
  (.writeXdmValue proc node serializer))

(defn- set-props
  [^Serializer s props]
  (doseq [[prop value] props]
    (let [prop (Serializer$Property/valueOf (upper-snake-case prop))]
      (.setOutputProperty s prop value))))

(defmulti serialize (fn [_node dest & _props] (class dest)))
(defmethod serialize File
  [node ^File dest & props]
  (let [s (.newSerializer proc)]
    (set-props s (first props))
    (write-value node (doto s (.setOutputFile dest)))
    dest))
(defmethod serialize OutputStream
  [node ^OutputStream dest & props]
  (let [s (.newSerializer proc)]
    (set-props s (first props))
    (write-value node (doto s (.setOutputStream dest)))
    dest))
(defmethod serialize Writer
  [node ^Writer dest & props]
  (let [s (.newSerializer proc)]
    (set-props s (first props))
    (write-value node (doto s (.setOutputWriter dest)))
    dest))

;; Node functions

(defn parent-node
  "Returns parent node of passed node."
  [^XdmNode nd]
  (.getParent nd))

(defn node-name
  "Returns the name of the node (as QName)."
  [^XdmNode nd]
  (.getNodeName nd))

(defn node-ns
  "Returns the namespace of the node or node name."
  [q]
  (if (= (class q) QName)
    (.getNamespaceURI ^QName q)
    (node-ns (node-name q))))

(def ^:private
  node-kind-map
  {XdmNodeKind/DOCUMENT :document
   XdmNodeKind/ELEMENT :element
   XdmNodeKind/ATTRIBUTE :attribute
   XdmNodeKind/TEXT :text
   XdmNodeKind/COMMENT :comment
   XdmNodeKind/NAMESPACE :namespace
   XdmNodeKind/PROCESSING_INSTRUCTION :processing-instruction})

(defn node-kind
  "Returns keyword corresponding to node's kind."
  [^XdmNode nd]
  (node-kind-map (.getNodeKind nd)))

(defn node-path
  "Returns XPath to node."
  [^XdmNode nd]
  (Navigator/getPath (.getUnderlyingNode nd)))

;(def ^{:private true}
;    axis-map
;        {:ancestor            Axis/ANCESTOR
;         :ancestor-or-self    Axis/ANCESTOR_OR_SELF
;         :attribute           Axis/ATTRIBUTE
;         :child               Axis/CHILD
;         :descendant          Axis/DESCENDANT
;         :descendant-or-self  Axis/DESCENDANT_OR_SELF
;         :following           Axis/FOLLOWING
;         :following-sibling   Axis/FOLLOWING_SIBLING
;         :parent              Axis/PARENT
;         :preceding           Axis/PRECEDING
;         :preceding-sibling   Axis/PRECEDING_SIBLING
;         :self                Axis/SELF
;         :namespace           Axis/NAMESPACE})
;
;(defn axis-seq
;   "Returns sequences of nodes on given axis."
;   ([^XdmNode nd axis]
;    (.axisIterator nd ^Axis (axis-map axis)))
;   ([^XdmNode nd axis name]
;    (.axisIterator nd ^Axis (axis-map axis) (QName. ^String name))))

; Node-kind predicates

(defn document?
  "Returns true if node is document."
  [^XdmNode nd]
  (.equals (.getNodeKind nd) XdmNodeKind/DOCUMENT))

(defn element?
  "Returns true if node is element."
  [^XdmNode nd]
  (.equals (.getNodeKind nd) XdmNodeKind/ELEMENT))

(defn attribute?
  "Returns true if node is attribute."
  [^XdmNode nd]
  (.equals (.getNodeKind nd) XdmNodeKind/ATTRIBUTE))

(defn text?
  "Returns true if node is text."
  [^XdmNode nd]
  (.equals (.getNodeKind nd) XdmNodeKind/TEXT))

(defn comment?
  "Returns true if node is comment."
  [^XdmNode nd]
  (.equals (.getNodeKind nd) XdmNodeKind/COMMENT))

(defn namespace?
  "Returns true if node is namespace."
  [^XdmNode nd]
  (.equals (.getNodeKind nd) XdmNodeKind/NAMESPACE))

(defn processing-instruction?
  "Returns true if node is processing instruction."
  [^XdmNode nd]
  (.equals (.getNodeKind nd) XdmNodeKind/PROCESSING_INSTRUCTION))
