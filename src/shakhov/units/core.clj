(ns shakhov.units.core
  (:use [clojure.algo.generic :only [root-type]])
  (:require [clojure.algo.generic.arithmetic :as ga]
            [clojure.algo.generic.comparison :as gc]
            [clojure.algo.generic.math-functions :as gm]))

(set! *warn-on-reflection* true)

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
  (.write w "#DS:{")
  (.write w ^String (apply str (interpose \, (:basic-dimensions ds))))
  (.write w "}"))

;;
;;  Dimension
;;

(declare basic-dimensions-with-exponents)

(defrecord Dimension
    [^DimensionSystem dimension-system
     exponents
     name]
  PQuantity
  (dimension [this] this)
  Object
  (toString [this]
    (if name
      (str name)
      (basic-dimensions-with-exponents this))))

(defn- filter-exponents
  [exponents]
  (into {} (remove (comp zero? second) exponents)))

(defn new-dimension
  ([dimension-system exponents]
     (new-dimension dimension-system exponents nil))
  ([dimension-system exponents name]
     (Dimension. dimension-system
                 (filter-exponents exponents)
                 name)))

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
    (.write w ^String (basic-dimensions-with-exponents dim))
    (.write w "}")))

;;
;;  Unit System
;;
(defrecord UnitSystem
    [name basic-dimensions-and-units units])

(defn new-unit-system
  ([basic-dimensions-and-units]
     (new-unit-system basic-dimensions-and-units nil))
  ([basic-dimensions-and-units name]
     (UnitSystem. name
                  basic-dimensions-and-units
                  (ref {}))))

;;  Print-method

(defmethod print-method UnitSystem
  [^UnitSystem us ^java.io.Writer w]
  (.write w "#US:{")
  (.write w ^String (apply str (interpose \, (vals (:basic-dimensions-and-units us)))))
  (.write w "}"))

;;
;;  Unit
;;

(declare basic-units-with-exponents)

(defmulti in-units-of
  "Return quantity converted to given units."
  (fn [u o] [(type u) (type o)]))

(defrecord Unit
  [^UnitSystem unit-system ^Number factor ^Dimension dimension name]
  PQuantity
  (dimension [this] dimension)
  (unit [this] this)
  (magnitude [this] 1.0)
  (magnitude-in-base-units [this] factor)
  clojure.lang.IFn
  (invoke [this o] (in-units-of this o))
  Object
  (toString [this]
    (if name
      (str name)
      (str (format "%g" factor) " " (basic-units-with-exponents unit-system dimension)))))

(defn new-unit
  ([^UnitSystem unit-system ^Number factor ^Dimension dimension]
     (new-unit unit-system factor dimension nil))
  ([^UnitSystem unit-system ^Number factor ^Dimension dimension name]
     (Unit. unit-system factor dimension name)))

;;  Print-Method

(defmethod print-method Unit
  [^Unit u ^java.io.Writer w]
  (let [us (:unit-system u)
        basic? (contains? (set (vals (:basic-dimensions-and-units us))) (:name u))]
    (.write w "#unit:{")
    (print-method (or (:name us) us) w)
    (.write w ":")
    (when-let [name (:name u)]
      (print-method name w)
      (when-not basic?
        (.write w "=")))
    (when-not basic?
      (.write w (format "%g" (:factor u)))
      (.write w " ")
      (.write w ^String (basic-units-with-exponents us (dimension u))))
    (.write w "}")))

;;
;;  Quantity
;;

(defrecord Quantity
    [^Number magnitude ^Unit unit]
  PQuantity
  (dimension [this] (dimension unit))
  (unit [this] unit)
  (magnitude [this] magnitude)
  (magnitude-in-base-units [this] (* magnitude (:factor unit)))
  clojure.lang.IFn
  (invoke [this o] (in-units-of this o))
  Object
  (toString [this]
    (str (format "%g" magnitude) " (" unit ")")))

(defn new-quantity
  ([^Number magnitude ^Unit unit]
     (Quantity. magnitude unit)))

;;  Print-Method

(defmethod print-method Quantity
  [^Quantity q ^java.io.Writer w]
  (let [u (unit q)
        d (dimension q)]
    (.write w "#")
    (.write w (if (:name d) (str (:name d)) "quantity"))
    (.write w ":{")
    (.write w (format "%g" (:magnitude q)))
    (.write w " (")
    (if (:name u)
      (print-method (:name u) w)
      (do (when-not (== 1.0 (:factor u))
            (.write w (str (format "%g" (:factor u)) "*")))
          (.write w ^String (basic-units-with-exponents (:unit-system u) d))))
    (.write w ")")
    (.write w "}")))

;;
;;  Printing symbols with exponents
;;

(declare dimensionless?)

(defn- symbols-with-exponents
  [symbols]
  (let [{pos true neg false} (group-by (comp pos? second) symbols)
        symbols (concat (sort pos) (sort neg))]
    (apply str (interpose \* (for [[s e] symbols]
                               (str s (when-not (= 1 e)
                                        (str "^" e))))))))

(defn- ^String basic-dimensions-with-exponents
  [dim]
  (if-not (dimensionless? dim)
    (symbols-with-exponents (vec (:exponents dim)))
    "1"))

(defn- ^String basic-units-with-exponents
  [us dim]
  (if-not (dimensionless? dim)
    (symbols-with-exponents (map (fn [[d e]]
                                   [(get (:basic-dimensions-and-units us) d) e])
                                 (:exponents dim)))
    "1"))

;;
;;  Hierarchies
;;

(derive Dimension root-type)
(derive ::quantity root-type)
(derive Unit ::quantity)
(derive Quantity ::quantity)

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
  (let [exponents exponents]
    (let [exponents (filter-exponents exponents)]
      (if-let [dim (get @(:dimensions ds) exponents)]
        dim
        (add-dimension ds exponents)))))

;;
;;  Predicates and asserts
;;

(defn assert-same-dimension-system
  [ds1 ds2]
  (when-not (identical? ds1 ds2)
    (throw (Exception. (str "Can't combine dimensions from " (:name ds1) " and " (:name ds2))))))

(defn- unnamed?
  [o]
  (nil? (:name o)))

(defn dimensionless?
  [q]
  (every? zero? (vals (:exponents (dimension q)))))

(defn- assert-dimensionless
  [q]
  (when-not (dimensionless? q)
    (throw (Exception. "Argument must be dimensionless: " q))))

(defn- compatible-dimensions?
  [d1 d2]
  (assert-same-dimension-system (:dimension-system d1)
                                (:dimension-system d2))
  (or (identical? d1 d2)
      (and (= (:exponents d1) (:exponents d2))
           (or (unnamed? d1) (unnamed? d2)))))

(defn- assert-compatible-dimensions
  [q1 q2]
  (let [d1 (dimension q1)
        d2 (dimension q2)]
    (when-not (compatible-dimensions? d1 d2)
      (throw (Exception. (str "Cannot convert " d1 " to " d2))))))

(defn dimension?
  "Return true if dim is a dimension compatible with quantity."
  [dim o]
  (compatible-dimensions? dim (dimension o)))

;;
;;  Dimension arithmetic
;;

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
                               `(do (def ~(symbol (str *ns*) (str d))
                                      (add-dimension ~ds-name {'~d 1} '~d))
                                    (def ~(symbol (str *ns*) (str d "?"))
                                      (partial dimension? ~d))))
                             basic-dimensions)]
    `(do (def ~(symbol (str *ns*) (str ds-name))
           (new-dimension-system '~basic-dimensions
                                 '~ds-name))
         ~@dimension-defs))) 

(defmacro def-dimension*
  [ds name spec]
  (let [spec (partition 2 spec)
        dims (vec (map first spec))
        pows (vec (map second spec))
        ds `(reduce (fn [ds1# ds2#]
                      (assert-same-dimension-system ds1# ds2#)
                      ds1#)
                    ~ds (map :dimension-system ~dims))
        exps `(apply merge-with +
                     (map (fn [{e# :exponents} p#]
                            (zipmap (keys e#)
                                    (map (partial * p#)
                                         (vals e#))))
                          ~dims ~pows))]
    `(do (def ~(symbol (str *ns*) (str name))
           (add-dimension ~ds ~exps '~name))
         (def ~(symbol (str *ns*) (str name "?"))
           (partial dimension? ~name)))))

(defmacro def-dimension
  "Define new dimension names in the dimension system."
  [ds & specs]
  (let [specs (partition 2 specs)]
    `(do ~@(map (fn [[name spec]]
                  `(def-dimension* ~ds ~name ~(vec spec)))
                specs))))

;;
;;  Adding units to unit systems
;;

(defn add-unit
  "Add new unit to the unit system."
  ([^UnitSystem us ^Number factor ^Dimension dim]
     (add-unit us factor dim nil))
  ([^UnitSystem us ^Number factor ^Dimension dim name]
     (let [unit (new-unit us factor dim name)]
       (dosync
        (alter (:units us)
               (fn [unit-map]
                 (assoc-in unit-map [dim factor] unit)))
        (when name
          (alter (:units us)
                 (fn [unit-map]
                   (assoc unit-map name unit)))))
       unit)))


(defn get-unit
  "Return unit corresponding to the given factor and dimension in the unit system,
   adding new unit to the unit system if necessary."
  [^UnitSystem us ^Number factor ^Dimension dim]
  (if-let [unit (get-in @(:units us) [dim factor])]
    unit
    (add-unit us factor dim)))

;;
;; Define unit systems and units
;;

(defmacro def-unit-system
  "Define a unit system in terms of basic dimensions and associated basic units."
  [us-name & basic-dimensions-and-units]
  (let [basic-dimensions-and-units (apply hash-map basic-dimensions-and-units)
        unit-defs (map (fn [[d u]]
                         `(def ~(symbol (str *ns*) (str u))
                            (add-unit ~us-name 1.0 ~d '~u)))
                       basic-dimensions-and-units)]
    `(do (when-not (= ~@(map (fn [d] `(:dimension-system ~d)) (keys basic-dimensions-and-units)))
           (throw (Exception. (str "Cannot define unit system based on dimensions from different dimenson systems."))))
         (def ~(symbol (str *ns*) (str us-name))
           (new-unit-system '~basic-dimensions-and-units
                            '~us-name))
         ~@unit-defs)))

(defmacro def-unit*
  [us name q]
  `(let [unit# (as-unit ~us ~q)]
     (def ~(symbol (str *ns*) (str name))
       (add-unit ~us
                 (:factor unit#)
                 (:dimension unit#)
                 '~name))))

(defmacro def-unit
  "Define a new unit name with magntude and dimension derived from the quantity."
  [us & specs]
  (let [specs (partition 2 specs)]
    `(do ~@(map (fn [[name q]]
                  `(def-unit* ~us ~name ~q))
                specs))))

(defmacro def-prefixed-units
  ""
  [us base-units & spec]
  (let [unit-defs (for [unit base-units
                        [prefix factor] (partition 2 spec)]
                    `(def-unit
                       ~us
                       ~(symbol (str prefix (name unit)))
                       (ga/* ~factor ~unit)))]
    `(do ~@unit-defs)))

;;
;;  Converting quantities and units
;;

(defn to-unit-system
  "Convert quantity to the unit system."
  [to-us q]
  (let [from-us (:unit-system (unit q))]
    (if (identical? to-us from-us)
      q
      (throw (Exception. (str "Cannot convert unit system from " (:name from-us) " to " (:name to-us)))))))

(defmulti as-unit 
  "Return new unit with factor = (magnitude of quantity) x (unit factor)"
  (fn [us o] (type o)))

(defmethod as-unit Unit [us u]
  (to-unit-system us u))

(defmethod as-unit Dimension
  [us dim]
  ;; No unit name!!!
  ;; FIXME: assert dimension systems
  (add-unit us 1.0 dim))

(defmethod as-unit Quantity
  [us q]
  (let [u (to-unit-system us (unit q))]
    (get-unit us
              (magnitude-in-base-units q)
              (dimension u))))

(defmethod in-units-of [Unit Number]
  [^Unit u ^Number m]
  (new-quantity m u))

(defmethod in-units-of [Unit ::quantity]
  [^Unit u q]
  (assert-compatible-dimensions u q)
  (let [q (to-unit-system (:unit-system u) q)]
    (new-quantity (/ (magnitude-in-base-units q)
                     (:factor u)) u)))

(defmethod in-units-of [Quantity root-type]
  [q o]
  (in-units-of (as-unit (:unit-system (unit q)) q) o))

;;
;;  Quantities arithmetic
;;

(defmethod ga/+ [::quantity ::quantity]
  [q1 q2]
  (assert-compatible-dimensions q1 q2)
  (let [u1 (unit q1)
        q2 (u1 q2)]
    (u1 (+ (magnitude q1)
           (magnitude q2)))))

(defmethod ga/- ::quantity
  [q]
  ((unit q) (- (magnitude q))))


(defmethod ga/* [::quantity ::quantity]
  [q1 q2]
  (let [q2 (to-unit-system (:unit-system (unit q1))  q2)
        u1 (unit q1)
        u2 (unit q2)
        dim (ga/* (dimension u1) (dimension u2))]
    (if (dimensionless? dim)
      (* (magnitude-in-base-units q1)
         (magnitude-in-base-units q2))
      ((get-unit (:unit-system u1) 1.0 dim)
       (* 1.0 (magnitude-in-base-units q1) (magnitude-in-base-units q2))))))

(defmethod ga/* [root-type ::quantity]
  [a q]
  ((unit q) (* a (magnitude q))))

(defmethod ga/* [::quantity root-type]
  [q a]
  (ga/* a q))

(ga/defmethod* ga / ::quantity
  [q]
  (let [u (unit q)
        uinv (get-unit (:unit-system u) (/ (:factor u)) ((ga/qsym ga /) (:dimension u)))]
    (uinv ((ga/qsym ga /) (magnitude q)))))

;;
;;  Generic comparison
;;

(defmethod gc/zero? ::quantity
  [q]
  (gc/zero? (magnitude q)))

(defmethod gc/pos? ::quantity
  [q]
  (gc/pos? (magnitude q)))

(defmethod gc/neg? ::quantity
  [q]
  (gc/neg? (magnitude q)))

(defmethod gc/> [::quantity ::quantity]
  [q1 q2]
  (gc/pos? (magnitude (ga/- q1 q2))))

(defmethod gc/< [::quantity ::quantity]
  [q1 q2]
  (gc/neg? (magnitude (ga/- q1 q2))))

(defmethod gc/= [::quantity ::quantity]
  [q1 q2]
  (gc/zero? (magnitude (ga/- q1 q2))))

;
; Generic math functions
;

(defmethod gm/abs ::quantity
  [q]
  ((unit q) (gm/abs (magnitude q))))

(defmethod gm/sgn ::quantity
  [q]
  (gm/sgn (magnitude q)))

(doseq [f [gm/sin gm/cos gm/tan gm/asin gm/acos gm/atan gm/exp gm/log]]
  (defmethod f ::quantity
    [q]
    (assert-dimensionless q)
    (f (magnitude-in-base-units q))))

(defmethod gm/atan2 [::quantity ::quantity]
  [q1 q2]
  (let [q2 ((unit q1) q2)]
    (gm/atan2 (magnitude q1) (magnitude q2))))

(defn- int-pow
  [q ^Integer p]
  (apply ga/* (repeat p q)))

(defn- ratio-pow
  [q ^clojure.lang.Ratio r]
  (let [dim       (dimension q)
        exponents (:exponents dim)
        exponents (zipmap (keys exponents)
                          (map (partial * r) (vals exponents)))]
    (when-not (every? integer? (vals exponents))
      (throw (Exception. (str "Cannot take " dim " to power " r))))
    (let [dim (get-dimension (:dimension-system dim) exponents)
          u   (get-unit (:unit-system (unit q)) 1.0 dim)]
      (u (gm/pow (magnitude-in-base-units q) r)))))

(defmethod gm/pow [::quantity Number]
  [q ^Number p]
  (cond (dimensionless? q)  (gm/pow (magnitude-in-base-units q) p)
        (zero? p)           1.0
        (= p 1)             q
        (integer? p)        (int-pow q p)
        (ratio? p)          (ratio-pow q p)
        :else               (throw (Exception. (str "Cannot take quantity to "
                                                    (type p) " power")))))

(defmethod gm/sqrt ::quantity
  [q]
  (ratio-pow q 1/2))
