(ns limabean.core.ledger-test
  (:require [java-time.api :as jt]
            [limabean.core.ledger :as sut]
            [clojure.test :refer [deftest is]]))

;; we're testing a private function, so:
(def merge-position #'sut/merge-position)

(deftest merge-position-test
  (is (= (merge-position [] {:units 10, :cur "IBM"}) [{:units 10, :cur "IBM"}]))
  (is (= (merge-position [{:units 10, :cur "IBM"}] {:units 3, :cur "IBM"})
         [{:units 13, :cur "IBM"}]))
  (is (= (merge-position [{:units 10,
                           :cur "IBM",
                           :cost {:per-unit 150.00M,
                                  :total 1500.00M,
                                  :cur "NZD",
                                  :date (jt/local-date 2024 2 1)}}]
                         {:units 2, :cur "IBM"})
         [{:units 2, :cur "IBM"}
          {:units 10,
           :cur "IBM",
           :cost {:per-unit 150.00M,
                  :total 1500.00M,
                  :cur "NZD",
                  :date (jt/local-date 2024 2 1)}}]))
  (is
    (= (merge-position [{:units 10,
                         :cur "IBM",
                         :cost {:per-unit 150.00M,
                                :total 1500.00M,
                                :cur "NZD",
                                :date (jt/local-date 2024 2 1)}}]
                       {:units 2,
                        :cur "IBM",
                        :cost {:per-unit 150.00M,
                               :total 300.00M,
                               :cur "NZD",
                               :date (jt/local-date 2024 2 1)}})
       [{:units 12,
         :cur "IBM",
         :cost {:per-unit 150.00M,
                :total 1800.00M,
                :cur "NZD",
                :date (jt/local-date 2024 2 1)}}])))
