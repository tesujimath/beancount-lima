(ns limabean.core.journal
  "Functions to build and query journal.

  A journal is a date-ordered list of postings with running balance (across all accounts).
  To restrict the balance to fewer accounts, pre-filter by account."
  (:require [limabean.core.inventory :as inventory]
            [limabean.core.cell :as cell :refer [cell]]))

(defn- with-bal
  "Return a (stateful) transducer to add a running total of units to postings.
  Only one running total is maintained, unseparated by account."
  []
  (fn [rf]
    (let [state (volatile! [])]
      (fn
        ;; init
        ([] (rf))
        ;; completion
        ([result] (rf result))
        ;; step
        ([result p]
         (let [p (dissoc p :cost) ;; journal excludes cost
               positions (inventory/merge-position @state p :none)]
           (vreset! state positions)
           (rf result (cell/mark (assoc p :bal positions) :journal/entry))))))))

(defn build
  "Build journal from given `postings`."
  [postings]
  (into [] (with-bal) postings))

(defmethod cell :journal/entry
  [ent]
  (cell/row [(cell (:date ent)) (cell (:acc ent)) (cell (:payee ent))
             (cell (:narration ent)) (cell (:flag ent)) (cell (:units ent))
             (cell (:cur ent)) (cell (:bal ent))]
            cell/SPACE-MEDIUM))
