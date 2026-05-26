(ns limabean.core.inventory
  "Functions to build and query an inventory."
  (:require [limabean.core.cell :as cell :refer [cell]]))

;; TODO instead of explicit delay/force these functions should be macros,
;; except that gave me errors from spec, which may be the CIDER integration

(defn- compare-empty-first-or*
  "If either x or y is empty, that compares first, otherwise else."
  [x y else]
  (cond (and (empty? x) (empty? y)) 0
        (empty? x) -1
        (empty? y) 1
        :else (force else)))

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

(defn- compare-cost-keys
  "Compare cost keys"
  [x y]
  (compare-empty-first-or*
    x
    y
    (let [[date-x cur-x per-unit-x label-x merge-x] x
          [date-y cur-y per-unit-y label-y merge-y] y]
      (delay (compare-different-or*
               date-x
               date-y
               (delay (compare-different-or*
                        cur-x
                        cur-y
                        (delay (compare-different-or*
                                 per-unit-x
                                 per-unit-y
                                 (compare-nil-first-different-or*
                                   label-x
                                   label-y
                                   (delay (compare-nil-first merge-x
                                                             merge-y))))))))))))

(defn- booking-rule
  "Map a booking method to the rule for combining positions, :merge or :append."
  [method]
  (cond (method #{:strict :strict-with-size :fifo :lifo :hifo}) :merge
        (= method :none) :append
        :else (throw (Exception. (str "unsupported booking method " method)))))
(defn- position-key
  "Return a key for a position which separates out by cost."
  [pos]
  (let [cost (:cost pos)]
    (if cost
      [(:date cost) (:cur cost) (:per-unit cost) (:label cost) (:merge cost)]
      [])))

(defn- update-or-set
  [m k f v1]
  (let [v0 (get m k)] (if v0 (assoc m k (f v0)) (assoc m k v1))))

(defn- single-currency-accumulator
  "Position accumulator for a single currency"
  [rule]
  (case rule
    :merge {:accumulate-f (fn [positions p1]
                            (let [k (position-key p1)]
                              (update-or-set
                                positions
                                k
                                (fn [p0]
                                  (assoc p0 :units (+ (:units p0) (:units p1))))
                                p1))),
            :reduce-f (fn [rf result positions]
                        (let [cost-keys (sort compare-cost-keys
                                              (keys positions))]
                          (reduce (fn [result k] (rf result (get positions k)))
                            result
                            cost-keys))),
            :positions {}}
    :append {:accumulate-f
               (fn [positions p1]
                 (if (contains? p1 :cost)
                   (assoc positions :at-cost (conj (:at-cost positions) p1))
                   (assoc positions
                     :simple (if-let [p0 (:simple positions)]
                               (assoc p0 :units (+ (:units p0) (:units p1)))
                               p1)))),
             :reduce-f (fn [rf result positions]
                         (let [result1 (if-let [simple (:simple positions)]
                                         (rf result simple)
                                         result)]
                           (reduce rf result1 (:at-cost positions)))),
             :positions {:simple nil, :at-cost []}}))

(defn- sca-accumulate
  [sca pos]
  (let [{:keys [accumulate-f positions]} sca]
    (assoc sca :positions (accumulate-f positions pos))))

(defn- sca-reduce
  [rf result sca]
  (let [{:keys [reduce-f positions]} sca] (reduce-f rf result positions)))

(defn accumulator
  "Create an inventory accumulator with given booking method."
  ([booking] (let [rule (booking-rule booking)] {:rule rule, :scas {}})))

(defn accumulate
  "Accumulate a position into an inventory according to its booking method.

  Position attributes are `:units`, `:cur`, and `:cost`."
  [inv p]
  (let [{:keys [rule scas]} inv
        ;; lose any extraneous attributes, such as might be in a posting
        p (select-keys p [:units :cur :cost])
        cur (:cur p)
        ;; lookup the sca for this currency, or create a new one
        sca (get scas cur (single-currency-accumulator rule))]
    (assoc inv :scas (assoc scas cur (sca-accumulate sca p)))))

(defn positions
  "Return the current balance of an inventory accumulator as a list of positions."
  [inv]
  (let [{:keys [scas]} inv
        currencies (sort (keys scas))]
    (reduce (fn [result cur]
              (sca-reduce (fn [result p]
                            ;; only keep the non-zero positions
                            (if (zero? (:units p))
                              result
                              (conj result (cell/mark p :position))))
                          result
                          (get scas cur)))
      []
      currencies)))

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

(defn build
  "Cumulate postings into inventory according to booking method.

  `acc-booking-fn` is a function which returns the booking method for an account."
  [postings acc-booking-fn]
  (let [init {}
        cumulated (reduce (fn [result p]
                            (let [acc (:acc p)
                                  inv (if-let [inv (get result acc)]
                                        inv
                                        (accumulator (acc-booking-fn acc)))]
                              (assoc result acc (accumulate inv p))))
                    init
                    postings)
        accounts (sort (keys cumulated))
        inv (reduce (fn [result account]
                      (let [account-positions (positions (get cumulated
                                                              account))]
                        (if (seq account-positions)
                          ;; only keep the non-empty positions
                          (assoc result account account-positions)
                          result)))
              {}
              accounts)]
    inv))

(defn cur-freq
  "Return map of frequency of currency use by currency."
  [inv]
  (reduce (fn [curs acc]
            (reduce (fn [curs cur] (assoc curs cur (inc (get curs cur 0))))
              curs
              (positions->currencies (get inv acc))))
    {}
    (cell/real-keys inv)))

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
