(ns limabean.core.registry
  "Functions to build and query registry.

  The registry is build from directives and options, and contains for example, booking method for each account, and currencies in order of frequency of usage.")

(defn build
  "Build the registry for given `directives` and `options`."
  [directives options]
  (let [default-booking (get options :booking :strict)
        init {:acc-booking {}, :cur-freq {}}
        result (reduce (fn [result dct]
                         (case (:dct dct)
                           :open (if-let [booking (get dct :booking)]
                                   (assoc result
                                     :acc-booking (assoc (:acc-booking result)
                                                    (:acc dct) booking))
                                   result)
                           :txn
                             ;; bump currency frequency for each posting
                             (reduce (fn [result pst]
                                       (let [cur-freq (:cur-freq result)
                                             cur (:cur pst)
                                             freq (get cur-freq cur 0)]
                                         (assoc result
                                           :cur-freq (assoc cur-freq
                                                       cur (inc freq)))))
                               result
                               (:postings dct))
                           result))
                 init
                 directives)
        acc-booking (:acc-booking result)
        cur-freq (:cur-freq result)
        curs (mapv first (sort-by (comp - second) (into [] cur-freq)))
        accs (into {}
                   (map (fn [acc] [acc
                                   (cond-> nil
                                     (contains? acc-booking acc)
                                       (assoc :booking (get acc-booking acc)))])
                     (keys acc-booking)))]
    {:default-booking default-booking, :accs accs, :curs curs}))

(defn acc-booking
  "Lookup the booking method for an account in the registry."
  [reg acc]
  (get-in reg [:accs acc :booking] (:default-booking reg)))
