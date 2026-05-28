(ns limabean.core.inventory-test
  (:require [java-time.api :as jt]
            [limabean.core.cell :as cell]
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

(defn- mkp "Mark positions" [ps] (mapv #(cell/mark % :position) ps))

(deftest merge-position-test
  (is (= (sut/merge-position [] {:units 10, :cur "IBM"} :strict)
         (mkp [{:units 10, :cur "IBM"}])))
  (is (= (sut/merge-position (mkp [{:units 10, :cur "IBM"}])
                             {:units 3, :cur "IBM"}
                             :strict)
         (mkp [{:units 13, :cur "IBM"}])))
  (is (= (sut/merge-position (mkp [{:units 10,
                                    :cur "IBM",
                                    :cost {:per-unit 150.00M,
                                           :total 1500.00M,
                                           :cur "NZD",
                                           :date (jt/local-date 2024 2 1)}}])
                             {:units 2, :cur "IBM"}
                             :strict)
         (mkp [{:units 2, :cur "IBM"}
               {:units 10,
                :cur "IBM",
                :cost {:per-unit 150.00M,
                       :total 1500.00M,
                       :cur "NZD",
                       :date (jt/local-date 2024 2 1)}}])))
  (is
    (= (sut/merge-position (mkp [{:units 10,
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
       (mkp [{:units 12,
              :cur "IBM",
              :cost {:per-unit 150.00M,
                     :total 1800.00M,
                     :cur "NZD",
                     :date (jt/local-date 2024 2 1)}}]))))

(deftest merge-position-append-test
  (is (= (sut/merge-position [] {:units 10, :cur "IBM"} :none)
         (mkp [{:units 10, :cur "IBM"}])))
  (is (= (sut/merge-position (mkp [{:units 10, :cur "IBM"}])
                             {:units 3, :cur "IBM"}
                             :none)
         (mkp [{:units 13, :cur "IBM"}])))
  (is (= (sut/merge-position (mkp [{:units 10,
                                    :cur "IBM",
                                    :cost {:per-unit 150.00M,
                                           :total 1500.00M,
                                           :cur "NZD",
                                           :date (jt/local-date 2024 2 1)}}])
                             {:units 2, :cur "IBM"}
                             :none)
         (mkp [{:units 2, :cur "IBM"}
               {:units 10,
                :cur "IBM",
                :cost {:per-unit 150.00M,
                       :total 1500.00M,
                       :cur "NZD",
                       :date (jt/local-date 2024 2 1)}}])))
  (is
    (= (sut/merge-position (mkp [{:units 10,
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
       (mkp [{:units 10,
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
