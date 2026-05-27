(ns limabean.core.inventory-test
  (:require [java-time.api :as jt]
            [limabean.core.inventory :as sut]
            [clojure.test :refer [deftest is]]))

;; we're testing private functions, so:
(def compare-positions-for-merge #'sut/compare-positions-for-merge)
(def compare-positions-for-append #'sut/compare-positions-for-append)

(deftest compare-position-for-merge-test
  (is (= (compare-positions-for-merge {} {}) 0))
  (is (= (compare-positions-for-merge {:units 10, :cur "IBM"}
                                      {:units 10, :cur "IBM"})
         0))
  (is (> (compare-positions-for-merge {:units 10, :cur "IBM"}
                                      {:units 10, :cur "AAPL"})
         0))
  (is (< (compare-positions-for-merge
           {:units 10, :cur "IBM"}
           {:units 10, :cur "IBM", :cost {:cur "GBP"}})
         0))
  (is (> (compare-positions-for-merge
           {:units 10, :cur "IBM", :cost {:cur "GBP"}}
           {:units 10, :cur "IBM"})
         0))
  (is (< (compare-positions-for-merge
           {:units 10, :cur "IBM", :cost {:cur "GBP"}}
           {:units 10, :cur "IBM", :cost {:cur "NZD"}})
         0))
  (is (< (compare-positions-for-merge
           {:units 10, :cur "IBM"}
           {:units 10, :cur "IBM", :cost {:date (jt/local-date 2024 1 1)}})
         0))
  (is (> (compare-positions-for-merge
           {:units 10, :cur "IBM", :cost {:date (jt/local-date 2024 1 31)}}
           {:units 10, :cur "IBM", :cost {:date (jt/local-date 2024 1 1)}})
         0))
  (is (= (compare-positions-for-merge
           {:units 10, :cur "IBM", :cost {:date (jt/local-date 2024 1 1)}}
           {:units 10, :cur "IBM", :cost {:date (jt/local-date 2024 1 1)}})
         0))
  ; and so on
)

(deftest compare-position-for-append-test
  (is (= (compare-positions-for-append {} {}) 0))
  (is (= (compare-positions-for-append {:units 10, :cur "IBM"}
                                       {:units 10, :cur "IBM"})
         0))
  (is (> (compare-positions-for-append {:units 10, :cur "IBM"}
                                       {:units 10, :cur "AAPL"})
         0))
  (is (< (compare-positions-for-append
           {:units 10, :cur "IBM"}
           {:units 10, :cur "IBM", :cost {:cur "GBP"}})
         0))
  (is (> (compare-positions-for-append
           {:units 10, :cur "IBM", :cost {:cur "GBP"}}
           {:units 10, :cur "IBM"})
         0))
  ;; cost-cur doesn't matter, we always append
  (is (> (compare-positions-for-append
           {:units 10, :cur "IBM", :cost {:cur "GBP"}}
           {:units 10, :cur "IBM", :cost {:cur "NZD"}})
         0))
  (is (< (compare-positions-for-append
           {:units 10, :cur "IBM"}
           {:units 10, :cur "IBM", :cost {:date (jt/local-date 2024 1 1)}})
         0))
  ;; date doesn't matter, we always append
  (is (> (compare-positions-for-append
           {:units 10, :cur "IBM", :cost {:date (jt/local-date 2024 1 31)}}
           {:units 10, :cur "IBM", :cost {:date (jt/local-date 2024 1 1)}})
         0))
  (is (> (compare-positions-for-append
           {:units 10, :cur "IBM", :cost {:date (jt/local-date 2024 1 1)}}
           {:units 10, :cur "IBM", :cost {:date (jt/local-date 2024 1 1)}})
         0))
  ; and so on
)
