(ns limabean.core.queries
  "Queries from the ledger"
  (:require [java-time.api :as jt]
            [limabean.core.inventory :as inventory]
            [limabean.core.account :as account]))

(defn bom
  "Return beginning of month containing `date`"
  [date]
  (.minusDays date (dec (.getDayOfMonth date))))

(defn periods
  "Return bom fenceposts within the date range"
  [begin end]
  (loop [result (if (= (.getDayOfMonth end) 1) '() (list end))
         prev (bom end)]
    (if (.isAfter prev begin)
      (recur (cons prev result) (.minusMonths prev 1))
      (vec (cons begin result)))))

(defn sum-units
  "Sum the inventory for the given accounts, returning a map"
  [inv cur accs]
  (into {}
        (map (fn [acc] [acc
                        (apply +
                          (keep (fn [[inv-acc positions]]
                                  (when (account/sub-acc? acc inv-acc)
                                    (inventory/positions->units-of positions
                                                                   cur)))
                                inv))])
          accs)))

(defn diff [m1 m2] (into {} (map (fn [[k v]] [k (- (get m2 k) v)]) m1)))

(defn monthly-income-statement
  [inventory-history cur {:keys [name-income name-expenses]} [begin end]]
  (let [totals (map #(-> (inventory/history-at inventory-history %)
                         (sum-units cur [name-income name-expenses])
                         (as-> t [% t]))
                 (periods begin end))
        diffs (map (fn [[date m1] [_ m2]] [date (diff m1 m2)])
                totals
                (rest totals))]
    diffs))
