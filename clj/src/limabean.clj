(ns limabean
  (:require [clojure.java.io :as io]
            [limabean.adapter.json]
            [limabean.adapter.print]
            [limabean.core.filters :as f]
            [limabean.core.inventory :as inventory]
            [limabean.core.xf :as xf]
            [limabean.core.journal :as journal]
            [limabean.core.registry :as registry]
            [limabean.core.rollup :as rollup]
            [limabean.adapter.loader :as loader]))


(defn- postings
  [directives filters]
  (eduction (comp (xf/postings) (xf/all-of filters)) directives))

(defn inventory
  "Build inventory from `beans` after applying filters, if any.
   Invocation without filters simply returns the pre-calculated inventory."
  ([beans] (inventory beans []))
  ([{:keys [inventory directives registry]} filters]
   (if (seq filters)
     (inventory/build (postings directives filters)
                      (partial registry/acc-booking registry))
     inventory)))

(defn inventory-history
  "Build inventory history from `beans` after applying filters, if any.
   Invocation without filters simply returns the pre-calculated inventory."
  ([beans] (inventory-history beans []))
  ([{:keys [history directives registry]} filters]
   (if (seq filters)
     (:history (inventory/build-with-history (postings directives filters)
                                             (partial registry/acc-booking
                                                      registry)))
     history)))

(defn rollup
  "Build a rollup for the primary currency from an inventory.

  To build for a different currency, simply filter by that currency, e.g
  ```
  (rollup (inventory (f/cur \"CHF\")))
  ```
  "
  [inv]
  (let [primary-cur (first (apply max-key val (inventory/cur-freq inv)))]
    (rollup/build inv primary-cur)))

(defn balances
  "Build balances from `beans`, optionally further filtered."
  ([beans] (balances beans []))
  ([{:keys [inventory options], :as beans} filters]
   (let [assets-and-liabilities [(:name-assets options)
                                 (:name-liabilities options)]]
     (if (seq filters)
       (limabean/inventory beans
                           (conj filters
                                 (apply f/sub-acc assets-and-liabilities)))
       (inventory/sub-accs inventory assets-and-liabilities)))))

(defn income-statement
  "Calculate income statement across a half-open date range, optionally further filtered."
  ([beans date-range] (income-statement beans date-range []))
  ([{:keys [options registry], :as beans} [begin-date end-date] filters]
   (let [income-and-expenses [(:name-income options) (:name-expenses options)]
         history (inventory-history beans filters)
         begin (-> (inventory/history-at history begin-date)
                   (inventory/sub-accs income-and-expenses))
         end (-> (inventory/history-at history end-date)
                 (inventory/sub-accs income-and-expenses))]
     (inventory/diff begin end (partial registry/acc-booking registry)))))

(defn journal
  "Build a journal of postings from `beans` with running balance."
  ([beans] (journal beans []))
  ([{:keys [directives]} filters]
   (journal/build (postings directives filters))))

(defn load-beanfile
  "Load beans from the beanfile at path"
  [path]
  (loader/load-beanfile path))

(defn version
  "Get the library version from pom.properties, else returns \"unknown\"."
  []
  (or
    (let [props (java.util.Properties.)]
      (try
        (with-open
          [in
             (io/input-stream
               (io/resource
                 "META-INF/maven/io.github.tesujimath/limabean/pom.properties"))]
          (.load props in)
          (.getProperty props "version"))
        (catch Exception _ nil)))
    "unknown"))
