(ns lima.cumulate)

(require '[lima.inventory :refer [accumulator accumulate finalize]])

(defn cumulate
  "Cumulate directives into inventory"
  [booked]
  (let [{:keys [directives options]} booked
        default-method (or (:booking options) :strict)
        init {:methods {}, :invs {}}
        cumulated
          (reduce
            (fn [result d]
              (case (:dct d)
                :open (if-let [method (:booking d)]
                        (assoc result
                          :methods (assoc (:methods result) (:acc d) method))
                        result)
                :txn (reduce (fn [result p]
                               (let [invs (:invs result)
                                     acc (:acc p)
                                     inv (if-let [inv (get invs acc)]
                                           inv
                                           (let [method (or (get (:methods
                                                                   result)
                                                                 acc)
                                                            default-method)]
                                             (accumulator method)))]
                                 (assoc result
                                   :invs (assoc invs acc (accumulate inv p)))))
                       result
                       (:postings d))
                result))
            init
            directives)
        invs (:invs cumulated)
        accounts (sort (keys invs))]
    (reduce (fn [result acc] (assoc result acc (finalize (get invs acc))))
      {}
      accounts)))
