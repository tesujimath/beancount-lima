(ns lima.inventory)

;; TODO instead of explicit delay/force these functions should be macros,
;; except that gave me errors from spec, which may be the CIDER integration

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

(defn compare-cost-or-nil
  "Compare costs or nils"
  [x y]
  (compare-nil-first-or*
    x
    y
    (delay (compare-different-or*
             (:date x)
             (:date y)
             (delay (compare-different-or*
                      (:cur x)
                      (:cur y)
                      (delay (compare-different-or*
                               (:per-unit x)
                               (:per-unit y)
                               (compare-nil-first-different-or*
                                 (:label x)
                                 (:label y)
                                 (delay (compare-nil-first (:merge x)
                                                           (:merge y))))))))))))

(defn inventory
  "Create an inventory, being a sorted set which accumulates according to the booking method."
  ([] (inventory :strict))
  ([booking]
   (sorted-set-by
     (cond (booking #{:strict :strict-with-size :fifo :lifo :hifo})
             (fn [x y] (compare-cost-or-nil (:cost x) (:cost y)))
           (= booking :none) (fn [x y]
                               (compare-nil-first-or* (:cost x) (:cost y) 1))
           :else (throw (Exception. (format "unsupported booking method"
                                            booking)))))))

(defn accumulate
  "Accumulate a value into an inventory"
  [inv x]
  ;; lose any extraneous attributes, such as might be in a posting:
  (let [x (select-keys x [:units :cur :cost])]
    (let [pos (inv x)]
      (if pos
        ;; TODO confirm this naive accumulation is OK, at least for now
        (let [pos (assoc pos :units (+ (:units pos) (:units x)))]
          (conj (disj inv pos) pos))
        (conj inv x)))))
