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

(defn new-dimension-system
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

(defn new-dimension
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


;;
;;  Adding dimensions to dimension systems
;;

(declare unnamed?)

(defn add-dimension
  "Add new dimension to the dimension system."
  ([^DimensionSystem ds exponents]
     (add-dimension ds exponents nil))
  ([^DimensionSystem ds exponents name]
     (let [dim (new-dimension ds exponents name)]
       (dosync
        (alter (:dimensions ds)
               (fn [dim-map]
                 (let [old-dim (get dim-map exponents)]
                   (assoc dim-map exponents
                          (cond (nil? old-dim) dim
                                (or (unnamed? old-dim)
                                    (= name (:name old-dim))) old-dim
                                :else (new-dimension ds exponents nil))))))
        (when name
          (alter (:dimensions ds)
                 (fn [dim-map]
                   (assoc dim-map name dim)))))
       dim)))


(defn get-dimension
  "Return the dimension corresponding to the given set of dimension exponents
   in dimension system, adding new dimension to the dimension system if necessary."
  [^DimensionSystem ds exponents]
  (if-let [dim (get @(:dimensions ds) exponents)]
    dim
    (add-dimension ds exponents)))


;;
;;  Asserts for dimensions
;;

(defn assert-same-dimension-system
  [ds1 ds2]
  (when-not (identical? ds1 ds2)
    (throw (Exception. (str "Can't combine dimensions from " (:name ds1) " and " (:name ds2))))))

;;
;;  Predicates
;;

(defn- unnamed?
  [o]
  (nil? (:name o)))

(defn- compatible-dimensions?
  [d1 d2]
  (assert-same-dimension-system (:dimension-system d1)
                                (:dimension-system d2))
  (or (identical? d1 d2)
      (and (= (:exponents d1) (:exponents d2))
           (or (unnamed? d1) (unnamed? d2)))))

(defn dimension?
  "Return true if dim is a dimension compatible with quantity."
  [dim o]
  (compatible-dimensions? dim (dimension o)))


;;
;;  Dimension arithmetic
;;

(derive Dimension root-type)

(defmethod ga/* [Dimension Dimension]
  [d1 d2]
  (assert-same-dimension-system (:dimension-system d1)
                                (:dimension-system d2))
  (get-dimension (:dimension-system d1)
                 (merge-with + (:exponents d1) (:exponents d2))))

(ga/defmethod* ga / Dimension
  [d]
  (let [exponents (:exponents d)]
    (get-dimension (:dimension-system d)
                   (zipmap (keys exponents)
                           (map - (vals exponents))))))

;;
;;  Define dimensions systems and dimensions
;;

(defmacro def-dimension-system
  "Define a dimension system specified by a set of basic dimension symbols.
   Add all dimensions to the dimension system and define corresponding symbols in current namespace."
  [ds-name & basic-dimensions]
  (let [basic-dimensions (set basic-dimensions)
        dimension-defs  (map (fn [d]
                               `(def ~d (add-dimension ~ds-name {'~d 1} '~d)))
                             basic-dimensions)]
    `(do (def ~ds-name
           (new-dimension-system '~basic-dimensions
                                 '~ds-name))
         (def dimensionless (add-dimension ~ds-name {} 1))
         ~@dimension-defs)))

(defmacro def-dimension*
  [name spec]
  (let [spec (partition 2 spec)
        dims (vec (map first spec))
        pows (vec (map second spec))
        ds `(reduce (fn [ds1# ds2#]
                     (assert-same-dimension-system ds1# ds2#)
                     ds1#)
                    (map :dimension-system ~dims))
        exps `(apply merge-with +
                     (map (fn [{e# :exponents} p#]
                            (zipmap (keys e#)
                                    (map (partial * p#)
                                         (vals e#))))
                          ~dims ~pows))]
    `(def ~name
       (add-dimension ~ds ~exps '~name))))

(defmacro def-dimension
  "Define new dimension names."
  [& specs]
  (let [specs (partition 2 specs)]
    `(do ~@(map (fn [[name spec]]
                  `(def-dimension* ~name ~(vec spec)))
                specs))))
