(ns shakhov.units.core
  (:use [clojure.algo.generic :only [root-type]])
  (:require [clojure.algo.generic.arithmetic :as ga]
            [clojure.algo.generic.comparison :as gc]
            [clojure.algo.generic.math-functions :as gm]))

;;
;;   Protocol for quantity-like objects: dimensions, units and quantities
;;

(defprotocol PQuantity
  "Properties of quantity-like objects"
  (dimension [this]
    "Return the dimension of quantity")
  (unit [this]
    "Return the unit of quantity")
  (magnitude [this]
    "Return the magnitude of quantity expressed in its units")
  (magnitude-in-base-units [this]
    "Return the magnitude of quantity expressed in base units"))


;;
;;  Dimension System
;;

(defrecord DimensionSystem
    [name basic-dimensions dimensions])

(defn- new-dimension-system
  ([basic-dimensions]
     (new-dimension-system basic-dimensions nil))
  ([basic-dimensions name]
     (DimensionSystem. name
                       basic-dimensions
                       (ref {}))))

;;  Print-method

(defmethod print-method DimensionSystem
  [^DimensionSystem ds ^java.io.Writer w]
  (.write w "#DS{")
  (.write w (apply str (interpose \, (:basic-dimensions ds))))
  (.write w "}"))

;;
;;  Dimension
;;

(defrecord Dimension
    [^DimensionSystem dimension-system
     exponents
     name]
  PQuantity
  (dimension [this] this))

(defn- new-dimension
  ([dimension-system exponents]
     (new-dimension dimension-system exponents nil))
  ([dimension-system exponents name]
     (let [exponents (into {} (remove (comp zero? second)
                                      exponents))]
       (Dimension. dimension-system
                   exponents
                   name))))

;;  Print-method

(defmethod print-method Dimension
  [^Dimension dim ^java.io.Writer w]
  (let [ds (:dimension-system dim)]
    (.write w "#dimension:{")
    (print-method (or (:name ds) ds) w)
    (.write w ":")
    (when-let [name (:name dim)]
      (print-method name w)
      (.write w "="))
    (print-method 
     (:exponents dim) w)
    (.write w "}")))
