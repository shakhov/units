(ns shakhov.units.core-test
  (:refer-clojure :exclude [time force * /])
  (:use clojure.test
        shakhov.units.core
        [clojure.algo.generic.arithmetic :only [* /]]))

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