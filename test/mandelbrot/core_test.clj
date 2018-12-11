(ns mandelbrot.core-test
  (:require [clojure.test :refer :all]
            [mandelbrot.core :refer :all]
            [mandelbrot.complex :refer :all]
            [quil.core :as q]
            [quil.middleware :as m]))


(deftest scale-px-test
  (testing "Scale pixel fail."
    (is (= (scale-px 1 2 (complex 1.0) 2.0) (complex 1.5 1.0)))
    ))

(deftest int-to-color-test
  (testing "Int to color fail."
    (is (= (int-to-color 0) (list 255 0 0)))
    (is (= (int-to-color 1) (list 0 255 0)))
    (is (= (int-to-color 2) (list 0 0 255)))
    ))
