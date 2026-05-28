(ns limabean.core.inventory-test
  (:require [java-time.api :as jt]
            [limabean.core.cell :as cell]
            [limabean.core.inventory :as sut]
            [clojure.test :refer [deftest is]]))

;; private functions:
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

(defn- mkp "Mark position" [p] (cell/mark p :position))
(defn- mkps "Mark positions" [ps] (mapv mkp ps))

(deftest merge-position-test
  (is (= (sut/merge-position [] {:units 10, :cur "IBM"} :strict)
         (mkps [{:units 10, :cur "IBM"}])))
  (is (= (sut/merge-position (mkps [{:units 10, :cur "IBM"}])
                             {:units 3, :cur "IBM"}
                             :strict)
         (mkps [{:units 13, :cur "IBM"}])))
  (is (= (sut/merge-position (mkps [{:units 10,
                                     :cur "IBM",
                                     :cost {:per-unit 150.00M,
                                            :total 1500.00M,
                                            :cur "NZD",
                                            :date (jt/local-date 2024 2 1)}}])
                             {:units 2, :cur "IBM"}
                             :strict)
         (mkps [{:units 2, :cur "IBM"}
                {:units 10,
                 :cur "IBM",
                 :cost {:per-unit 150.00M,
                        :total 1500.00M,
                        :cur "NZD",
                        :date (jt/local-date 2024 2 1)}}])))
  (is
    (= (sut/merge-position (mkps [{:units 10,
                                   :cur "IBM",
                                   :cost {:per-unit 150.00M,
                                          :total 1500.00M,
                                          :cur "NZD",
                                          :date (jt/local-date 2024 2 1)}}])
                           {:units 2,
                            :cur "IBM",
                            :cost {:per-unit 150.00M,
                                   :total 300.00M,
                                   :cur "NZD",
                                   :date (jt/local-date 2024 2 1)}}
                           :strict)
       (mkps [{:units 12,
               :cur "IBM",
               :cost {:per-unit 150.00M,
                      :total 1800.00M,
                      :cur "NZD",
                      :date (jt/local-date 2024 2 1)}}]))))

(deftest merge-position-append-test
  (is (= (sut/merge-position [] {:units 10, :cur "IBM"} :none)
         (mkps [{:units 10, :cur "IBM"}])))
  (is (= (sut/merge-position (mkps [{:units 10, :cur "IBM"}])
                             {:units 3, :cur "IBM"}
                             :none)
         (mkps [{:units 13, :cur "IBM"}])))
  (is (= (sut/merge-position (mkps [{:units 10,
                                     :cur "IBM",
                                     :cost {:per-unit 150.00M,
                                            :total 1500.00M,
                                            :cur "NZD",
                                            :date (jt/local-date 2024 2 1)}}])
                             {:units 2, :cur "IBM"}
                             :none)
         (mkps [{:units 2, :cur "IBM"}
                {:units 10,
                 :cur "IBM",
                 :cost {:per-unit 150.00M,
                        :total 1500.00M,
                        :cur "NZD",
                        :date (jt/local-date 2024 2 1)}}])))
  (is
    (= (sut/merge-position (mkps [{:units 10,
                                   :cur "IBM",
                                   :cost {:per-unit 150.00M,
                                          :total 1500.00M,
                                          :cur "NZD",
                                          :date (jt/local-date 2024 2 1)}}])
                           {:units 2,
                            :cur "IBM",
                            :cost {:per-unit 150.00M,
                                   :total 300.00M,
                                   :cur "NZD",
                                   :date (jt/local-date 2024 2 1)}}
                           :none)
       (mkps [{:units 10,
               :cur "IBM",
               :cost {:per-unit 150.00M,
                      :total 1500.00M,
                      :cur "NZD",
                      :date (jt/local-date 2024 2 1)}}
              {:units 2,
               :cur "IBM",
               :cost {:per-unit 150.00M,
                      :total 300.00M,
                      :cur "NZD",
                      :date (jt/local-date 2024 2 1)}}]))))

;; private function:
(def negate-position #'sut/negate-position)

(deftest negate-position-test
  (is (= (negate-position {:units 10, :cur "NZD"}) {:units -10, :cur "NZD"}))
  (is (= (negate-position {:units -3.50M, :cur "USD"})
         {:units 3.50M, :cur "USD"}))
  (is (= (negate-position {:units 10,
                           :cur "IBM",
                           :cost
                             {:per-unit 150.00, :total 1500.00, :cur "USD"}})
         {:units -10,
          :cur "IBM",
          :cost {:per-unit 150.00, :total -1500.00, :cur "USD"}})))

;; private function:
(def positions-diff #'sut/positions-diff)

(deftest positions-diff-test
  (is (= (positions-diff [{:units 10, :cur "NZD"}]
                         [{:units 15, :cur "NZD"}]
                         :strict)
         (mkps [{:units 5, :cur "NZD"}])))
  (is (= (positions-diff [{:units 10,
                           :cur "IBM",
                           :cost {:per-unit 100, :total 1000, :cur "NZD"}}]
                         [{:units 15,
                           :cur "IBM",
                           :cost {:per-unit 100, :total 1500, :cur "NZD"}}]
                         :strict)
         (mkps [{:units 5,
                 :cur "IBM",
                 :cost {:per-unit 100, :total 500, :cur "NZD"}}])))
  (is (= (positions-diff [{:units 10,
                           :cur "IBM",
                           :cost {:per-unit 100, :total 1000, :cur "NZD"}}]
                         [{:units 15,
                           :cur "IBM",
                           :cost {:per-unit 100, :total 1500, :cur "NZD"}}]
                         :none)
         (mkps [{:units -10,
                 :cur "IBM",
                 :cost {:per-unit 100, :total -1000, :cur "NZD"}}
                {:units 15,
                 :cur "IBM",
                 :cost {:per-unit 100, :total 1500, :cur "NZD"}}]))))
