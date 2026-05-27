(ns limabean.core.inventory-test
  (:require [java-time.api :as jt]
            [limabean.core.inventory :as sut]
            [clojure.test :refer [deftest is]]))

;; we're testing a private function, so:
(def compare-positions #'sut/compare-positions)

(deftest compare-position-test
  (is (= (compare-positions {} {}) 0))
  (is (< (compare-positions {:units 10, :cur "NZD"}
                            {:units 10, :cur "NZD", :cost {:cur "IBM"}})
         0))
  (is (> (compare-positions {:units 10, :cur "NZD", :cost {:cur "IBM"}}
                            {:units 10, :cur "NZD"})
         0))
  (is (> (compare-positions {:units 10, :cur "NZD", :cost {:cur "IBM"}}
                            {:units 10, :cur "NZD", :cost {:cur "AAPL"}})
         0))
  (is (> (compare-positions
           {:units 10, :cur "NZD", :cost {:date (jt/local-date 2024 1 31)}}
           {:units 10, :cur "NZD", :cost {:date (jt/local-date 2024 1 1)}})
         0))
  ; and so on
)
