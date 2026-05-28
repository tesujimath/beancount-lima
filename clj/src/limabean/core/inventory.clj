(ns limabean.core.inventory
  "Functions to build and query an inventory."
  (:require [limabean.core.cell :as cell :refer [cell]]))

;;;
;;; Position comparators for merge/append
;;;

;; TODO instead of explicit delay/force these functions should be macros,
;; except that gave me errors from spec, which may be the CIDER integration

(defn- compare-nil-first-or*
  "If either x or y is nil, that compares first, otherwise else."
  [x y else]
  (cond (and (nil? x) (nil? y)) 0
        (nil? x) -1
        (nil? y) 1
        :else (force else)))

(defn- compare-nil-first
  "If either x or y is nil, that compares first, otherwise standard compare."
  [x y]
  (compare-nil-first-or* x y (delay (compare x y))))

(defn- compare-different-or*
  "If the values compare different return that, else return the else."
  [x y else]
  (let [cmp (compare x y)] (if (not= 0 cmp) cmp (force else))))

(defn- compare-nil-first-different-or*
  "If the values compare different return that, else return the else."
  [x y else]
  (let [cmp (compare-nil-first x y)] (if (not= 0 cmp) cmp (force else))))

(defn- compare-positions-for-merge
  "Compare positions for sort/merge order.

   First by currency then by cost attributes."
  [p1 p2]
  (compare-different-or*
    (:cur p1)
    (:cur p2)
    (let [c1 (:cost p1)
          c2 (:cost p2)]
      (compare-nil-first-or*
        c1
        c2
        (delay (compare-different-or*
                 (:date c1)
                 (:date c2)
                 (delay (compare-different-or*
                          (:cur c1)
                          (:cur c2)
                          (delay (compare-different-or*
                                   (:per-unit c1)
                                   (:per-unit c2)
                                   (compare-nil-first-different-or*
                                     (:label c1)
                                     (:label c2)
                                     (delay (compare-nil-first
                                              (:merge c1)
                                              (:merge c2))))))))))))))

(defn- compare-positions-for-append
  "Compare positions for sort/append order.

   First by currency then simply p1 after p2 if there are costs."
  [p1 p2]
  (compare-different-or*
    (:cur p1)
    (:cur p2)
    (let [c1 (:cost p1) c2 (:cost p2)] (compare-nil-first-or* c1 c2 1))))


;;;
;;; Inventory building
;;;

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

(defn- compare-function-for-booking-method
  "Return the compare function appropriate for the booking method."
  [booking-method]
  (cond (booking-method #{:strict :strict-with-size :fifo :lifo :hifo})
          compare-positions-for-merge
        (= booking-method :none) compare-positions-for-append
        :else (throw (Exception. (str "unsupported booking method "
                                      booking-method)))))
(defn merge-position
  "Merge position in currency order, then by cost attributes."
  [positions pst booking-method]
  (let [;; lose any extraneous attributes, such as might
        ;; be in a posting, and mark as position
        pos (cell/mark (select-keys pst [:units :cur :cost]) :position)
        compare-fn (compare-function-for-booking-method booking-method)]
    (loop [merged []
           remaining positions]
      (let [[p & remaining] remaining]
        (if (nil? p)
          (conj merged pos)
          (let [cmp (compare-fn pos p)]
            (cond (> cmp 0) (recur (conj merged p) remaining)
                  (< cmp 0) (into (conj merged pos p) remaining)
                  :else (let [p' (combine-positions p pos)]
                          (if p'
                            (into (conj merged p') remaining)
                            (into merged remaining))))))))))

(defn build-with-history
  "Cumulate postings into inventory and inventory history indexed by date.

  `acc-booking-fn` is a function which returns the booking method for an
  account."
  [postings acc-booking-fn]
  (let [[invs invs-by-date]
          ;; invs         =          {acc -> [position]}
          ;; invs-by-date = {date -> {acc -> [position]}}
          (reduce (fn [[invs invs-by-date] pst]
                    (let [acc (:acc pst)
                          merged-positions (merge-position (get invs acc [])
                                                           pst
                                                           (acc-booking-fn acc))
                          invs' (if (seq merged-positions)
                                  (assoc invs acc merged-positions)
                                  (dissoc invs acc))
                          invs-by-date' (assoc invs-by-date (:date pst) invs')]
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


;;;
;;; Queries
;;;

(defn- positions->units-by-currency
  [ps]
  (reduce (fn [result p]
            (let [units (get result (:cur p) 0M)]
              (assoc result (:cur p) (+ units (:units p)))))
    {}
    ps))

(defn- positions->currencies
  [ps]
  (let [by-cur (positions->units-by-currency ps)
        curs (sort (keys by-cur))]
    curs))

(defn cur-freq
  "Return map of frequency of currency use by currency."
  [inv]
  (reduce (fn [curs acc]
            (reduce (fn [curs cur] (assoc curs cur (inc (get curs cur 0))))
              curs
              (positions->currencies (get inv acc))))
    {}
    (cell/real-keys inv)))

(defn positions->units
  "Return positions collapsed down to units only with no costs."
  [ps]
  (let [by-cur (positions->units-by-currency ps)
        curs (sort (keys by-cur))]
    (mapv (fn [cur] {:units (get by-cur cur), :cur cur}) curs)))

(defn positions->units-of
  "Return positions collapsed down to units only of the specified currency with no costs, or zero if none for that currency."
  [ps cur]
  (let [by-cur (positions->units-by-currency ps)] (get by-cur cur 0M)))


;;;
;;; Cells
;;;

(defn- cost->cell
  "Format a cost into a cell, avoiding the clutter of cell/type tagging"
  [cost]
  (cell/row [(cell (:date cost)) (cell (:cur cost)) (cell (:per-unit cost))
             (cell (:label cost)) (cell (if (:merge cost) "*" nil))]
            cell/SPACE-MINOR))

(defmethod cell :position
  [pos]
  (let [units (cell/row [(cell (:units pos)) (cell (:cur pos))]
                        cell/SPACE-MINOR)]
    (if-let [cost (:cost pos)]
      (cell/row [units (cost->cell cost)] cell/SPACE-MEDIUM)
      (cell/row [units] cell/SPACE-MEDIUM))))
