(ns lima.core.count
  (:require [lima.core.inventory :as inv]))

(defn inventory
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
                :txn
                  (reduce (fn [result p]
                            (let [invs (:invs result)
                                  acc (:acc p)
                                  inv (if-let [inv (get invs acc)]
                                        inv
                                        (let [method (or (get (:methods result)
                                                              acc)
                                                         default-method)]
                                          (inv/accumulator method)))]
                              (assoc result
                                :invs (assoc invs acc (inv/accumulate inv p)))))
                    result
                    (:postings d))
                result))
            init
            directives)
        invs (:invs cumulated)
        accounts (sort (keys invs))]
    (reduce (fn [result account]
              (let [account-positions (inv/finalize (get invs account))]
                (if (seq account-positions)
                  ;; only keep the non-empty positions
                  (assoc result account account-positions)
                  result)))
      {}
      accounts)))
