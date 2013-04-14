(ns shakhov.units.core-test
  (:refer-clojure :exclude [time force * / + - zero? pos? neg? < > =])
  (:use clojure.test
        shakhov.units.core
        [clojure.algo.generic.arithmetic :only [* / + -]]
        [clojure.algo.generic.comparison :only [zero? pos? neg? < > =]]))

(def-dimension-system structural
  length mass time)

(def-dimension
  area            [length 2]
  volume          [length 3]
  speed           [length 1 time -1]
  acceleration    [speed 1 time -1]
  force           [mass 1 acceleration 1]
  moment          [force 1 length 1]
  pressure        [force 1 area -1]
  stress          [force 1 area -1]
  density         [mass 1 volume -1]
  surface-density [density 1 length 1])

(deftest dimension-exponents
  (is (= {} (:exponents dimensionless)))
  (is (= '{length 2}
         (:exponents area)))
  (is (= '{length 3}
         (:exponents volume)))
  (is (= '{length 1 time -1}
         (:exponents speed)))
  (is (= '{length 1 time -2}
         (:exponents acceleration)))
  (is (= '{length 1 mass 1 time -2}
         (:exponents force)))
  (is (= '{length 2 mass 1 time -2}
         (:exponents moment)))
  (is (= '{mass 1 time -2 length -1}
         (:exponents pressure)))
  (is (= '{mass 1 time -2 length -1}
         (:exponents stress)))
  (is (= '{mass 1 length -3}
         (:exponents density)))
  (is (= '{mass 1 length -2}
         (:exponents surface-density))))

(deftest dimension-arithmetic
  (is (area? (* length length)))
  (is (volume? (* length area)))
  (is (speed? (/ length time)))
  (is (acceleration? (/ speed time)))
  (is (force? (* mass acceleration)))
  (is (moment? (* force length)))
  (is (pressure? (/ force area)))
  (is (stress? (/ force length length)))
  (is (density? (/ mass length length length)))
  (is (surface-density? (/ mass area))))

  
(deftest dimension-exceptions
  (let [ds1 (new-dimension-system #{'foo 'bar})
        d1  (add-dimension ds1 {'foo 1 'bar 1})]
    (is (thrown? Exception (* 1 length)))
    (is (thrown? Exception (* length 1)))
    (is (thrown? Exception (* length d1)))
    (is (thrown? Exception (def-dimension foobar [d1 2 length -2])))))

(def-unit-system si-structural
  length m
  mass kg
  time s)

(deftest basic-unit-dimensions
  (is (= length (dimension m)))
  (is (= mass (dimension kg)))
  (is (= time (dimension s))))

(deftest basic-unit-magnitude
  (is (= 1 (magnitude m)))
  (is (= 1 (magnitude-in-base-units m)))
  (is (= 1 (magnitude kg)))
  (is (= 1 (magnitude-in-base-units kg)))
  (is (= 1 (magnitude s)))
  (is (= 1 (magnitude-in-base-units s))))

(def-unit mm (* 0.001 m))
(def-unit cm (* 0.01 m))
(def-unit dm (* 0.1 m))
(def-unit km (* 1000 m))

(def-unit g (* 0.001 kg))
(def-unit tonne (* 1000.0 kg))

(def-unit N (/ (* kg m) s s))
(def-unit kN (* 1e3 N))
(def-unit MN (* 1e6 N))

(let [g-accel (/ (* 9.81 m) s s)]
  (def-unit kgf (* kg g-accel))
  (def-unit tonf (* tonne g-accel)))

(deftest derived-units
  (is (length? mm))
  (is (= 0.10 (:factor dm)))
  (is (= 10.0 (magnitude (km 10.0))))
  (is (= 0.10 (magnitude-in-base-units (cm 10.0))))

  (is (mass? tonne))
  (is (= 1.0 (magnitude-in-base-units (tonne 0.001))))
  (is (= 1000.0 (:factor tonne)))

  (is (force? tonf))
  (is (= 9810.0 (magnitude-in-base-units tonf))))

(deftest quantity-arithmetic
  (is (area? (* m m)))
  (is (area? (* m cm)))
  (is (pressure? (/ N m m)))
  (is (stress? (/ MN cm cm)))
  (is (density? (/ tonne m m m)))

  (is (= 200.0 (magnitude (cm (+ m m)))))
  (is (= 1020.0 (magnitude (kg (+ tonne (kg 20.0))))))
  (is (= 2021620.0 (magnitude (N (* 2 (+ MN kN tonf))))))
  (is (= 1.0 (magnitude ((* m m) (* 100 cm 100 cm)))))

  (is (= 0.0 (magnitude (- m (cm 100))))))

(deftest quantity-comparison
  (is (pos? (+ (cm 20) m)))
  (is (neg? (- (kg 100) tonne)))
  (is (zero? (- m (* 100 cm))))
  (is (< mm (* 10 cm) (/ m 2)))
  (is (> (/ tonne 3) (* 100 kg) (/ kg 10)))
  (is (= (* 1000 kg) (* 1 tonne))))