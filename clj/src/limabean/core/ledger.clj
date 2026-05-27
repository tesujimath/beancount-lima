(ns limabean.core.ledger
  (:require [limabean.core.cell :as cell :refer [cell]]
            [limabean.core.inventory :as inventory]))

(defn- combine-positions
  "Combine two matching positions"
  [p1 p2]
  (let [units (+ (:units p1) (:units p2))
        cost-total (and (:cost p1)
                        (+ (get-in p1 [:cost :total])
                           (get-in p2 [:cost :total])))]
    (if (zero? units)
      nil
      (cond-> (assoc p1 :units units)
        cost-total (assoc-in [:cost :total] cost-total)))))

(defn- merge-position
  "Merge position in currency order, then by cost attributes."
  [positions p1 compare-fn]
  (loop [merged []
         remaining positions]
    (let [[p & remaining] remaining]
      (if (nil? p)
        (conj merged p1)
        (let [cmp (compare-fn p1 p)]
          (cond (> cmp 0) (recur (conj merged p) remaining)
                (< cmp 0) (into (conj merged p1 p) remaining)
                :else (let [p' (combine-positions p p1)]
                        (if p'
                          (into (conj merged p') remaining)
                          (into merged remaining)))))))))

(defn- compare-function-for-booking-method
  "Return the compare function appropriate for the booking method."
  [booking-method]
  (cond (booking-method #{:strict :strict-with-size :fifo :lifo :hifo})
          inventory/compare-positions-for-merge
        (= booking-method :none) inventory/compare-positions-for-append
        :else (throw (Exception. (str "unsupported booking method "
                                      booking-method)))))

(defn build-with-history
  "Cumulate postings into inventory and inventory history indexed by date.

  `acc-booking-fn` is a function which returns the booking method for an
  account."
  [postings acc-booking-fn]
  (let [[invs invs-by-date]
          ;; invs         =          {acc -> [position]}
          ;; invs-by-date = {date -> {acc -> [position]}}
          (reduce (fn [[invs invs-by-date] p]
                    (let [acc (:acc p)
                          ;; lose any extraneous attributes, such as might
                          ;; be in a posting, and mark as position
                          p' (cell/mark (select-keys p [:units :cur :cost])
                                        :position)
                          compare-fn (compare-function-for-booking-method
                                       (acc-booking-fn acc))
                          merged-positions
                            (merge-position (get invs acc []) p' compare-fn)
                          invs' (if (seq merged-positions)
                                  (assoc invs acc merged-positions)
                                  (dissoc invs acc))
                          invs-by-date' (assoc invs-by-date (:date p) invs')]
                      [invs' invs-by-date']))
            [{} (sorted-map)]
            postings)]
    {:inventory invs, :history invs-by-date}))

(defn build
  "Cumulate postings into inventory indexed by date.

  `acc-booking-fn` is a function which returns the booking method for an
             account."
  [postings acc-booking-fn]
  (:inventory (build-with-history postings acc-booking-fn)))
