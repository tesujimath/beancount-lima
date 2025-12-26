(ns lima.inventory)

;; TODO instead of explicit delay/force these functions should be macros,
;; except that gave me errors from spec, which may be the CIDER integration

(defn compare-empty-first-or*
  "If either x or y is empty, that compares first, otherwise else."
  [x y else]
  (cond (and (empty? x) (empty? y)) 0
        (empty? x) -1
        (empty? y) 1
        :else (force else)))

(defn compare-nil-first-or*
  "If either x or y is nil, that compares first, otherwise else."
  [x y else]
  (cond (and (nil? x) (nil? y)) 0
        (nil? x) -1
        (nil? y) 1
        :else (force else)))

(defn compare-nil-first
  "If either x or y is nil, that compares first, otherwise standard compare."
  [x y]
  (compare-nil-first-or* x y (delay (compare x y))))

(defn compare-different-or*
  "If the values compare different return that, else return the else."
  [x y else]
  (let [cmp (compare x y)] (if (not= 0 cmp) cmp (force else))))

(defn compare-nil-first-different-or*
  "If the values compare different return that, else return the else."
  [x y else]
  (let [cmp (compare-nil-first x y)] (if (not= 0 cmp) cmp (force else))))

(defn compare-cost-keys
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

(defn booking-rule
  "Map a booking method to the rule for combining positions, :merge or :append."
  [method]
  (cond (method #{:strict :strict-with-size :fifo :lifo :hifo}) :merge
        (= method :none) :append
        :else (throw (Exception. (format "unsupported booking method"
                                         method)))))
(defn position-key
  "Return a key for a position which separates out by cost."
  [pos]
  (let [cost (:cost pos)]
    (if cost
      [(:date cost) (:cur cost) (:per-unit cost) (:label cost) (:merge cost)]
      [])))

(defn update-or-set
  [m k f v1]
  (let [v0 (get m k)] (if v0 (assoc m k (f v0)) (assoc m k v1))))

(defn single-currency-accumulator
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

(defn sca-accumulate
  [sca pos]
  (let [{:keys [accumulate-f positions]} sca]
    (assoc sca :positions (accumulate-f positions pos))))

(defn sca-reduce
  [rf result sca]
  (let [{:keys [reduce-f positions]} sca] (reduce-f rf result positions)))

(defn accumulator
  "Create an inventory accumulator, which must be finalized after accumulation is complete, using finalize-inventory."
  ([method] (let [rule (booking-rule method)] {:rule rule, :scas {}})))

(defn accumulate
  "Accumulate a position into an inventory"
  [inv p]
  (let [{:keys [rule scas]} inv
        ;; lose any extraneous attributes, such as might be in a posting
        p (select-keys p [:units :cur :cost])
        cur (:cur p)
        ;; lookup the sca for this currency, or create a new one
        sca (if-let [sca (get scas cur)]
              sca
              (single-currency-accumulator rule))]
    (assoc inv :scas (assoc scas cur (sca-accumulate sca p)))))

(defn finalize
  "Finalize an inventory accumulator into a list of positions"
  [inv]
  (let [{:keys [scas]} inv
        currencies (sort (keys scas))]
    (reduce (fn [result cur]
              (sca-reduce (fn [result p]
                            ;; only keep the non-zero positions
                            (if (zero? (:units p)) result (conj result p)))
                          result
                          (get scas cur)))
      []
      currencies)))
