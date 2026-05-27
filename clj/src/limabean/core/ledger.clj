(ns limabean.core.ledger
  (:require [limabean.core.inventory :as inventory]))

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
  "For a single currency, so only cost is the differentiator,
   and costs respect the inventory sort order for positions."
  [positions p1]
  (loop [merged []
         remaining positions]
    (let [[p & remaining] remaining]
      (if (nil? p)
        (conj merged p1)
        (let [cmp (inventory/compare-positions p p1)]
          (cond (< cmp 0) (recur (conj merged p) remaining)
                (> cmp 0) (into (conj merged p1 p) remaining)
                :else (let [p' (combine-positions p p1)]
                        (if p'
                          (into (conj merged p') remaining)
                          (into merged remaining)))))))))

(defn- merge-or-append-position-at-cost
  "Positions held at cost are appended, otherwise combined."
  [positions p1]
  (let [[p & remaining] positions]
    (cond (nil? p) [p1]
          (:cost p1) (conj positions p1)
          (:cost p) (into [p1] positions)
          :else (into [(combine-positions p p1)] remaining))))

(defn- merge-function-for-booking-method
  "Return the merge function appropriate for the booking method."
  [booking-method]
  (cond (booking-method #{:strict :strict-with-size :fifo :lifo :hifo})
          merge-position
        (= booking-method :none) merge-or-append-position-at-cost
        :else (throw (Exception. (str "unsupported booking method "
                                      booking-method)))))

(defn- accumulate
  "Accumulate a position into an inventory according to its booking method.

   Position attributes are `:units`, `:cur`, and `:cost`.

   Empty positions are removed, as are empty currencies.
  "
  [inv p merge-f]
  (let [;; lose any extraneous attributes, such as might be in a posting
        p (select-keys p [:units :cur :cost])
        cur (:cur p)
        positions (merge-f (get inv cur []) p)
        inv' (if (seq positions) (assoc inv cur positions) (dissoc inv cur))]
    inv'))

(defn build
  "Cumulate postings into ledger according to booking method.

  `acc-booking-fn` is a function which returns the booking method for an
  account."
  [postings acc-booking-fn]
  (let [[invs invs-by-date]
          ;; invs         =          {acc -> {cur -> [position]}}
          ;; invs-by-date = {date -> {acc -> {cur -> [position]}}}
          (reduce (fn [[invs invs-by-date] p]
                    (let [acc (:acc p)
                          merge-f (merge-function-for-booking-method
                                    (acc-booking-fn acc))
                          merged-currency-positions
                            (accumulate (get invs acc {}) p merge-f)
                          invs' (if (seq merged-currency-positions)
                                  (assoc invs acc merged-currency-positions)
                                  (dissoc invs acc))
                          invs-by-date' (assoc invs-by-date (:date p) invs')]
                      [invs' invs-by-date']))
            [{} (sorted-map)]
            postings)]
    {:current invs, :history invs-by-date}))
