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
  "Build inventory from `beans` after applying filters, if any."
  ([beans] (inventory beans []))
  ([beans filters]
   (inventory/build (postings (:directives beans) filters)
                    (partial registry/acc-booking (:registry beans)))))

(defn inventory-with-history
  "Build inventory from `beans` after applying filters, if any."
  ([beans] (inventory beans []))
  ([beans filters]
   (inventory/build-with-history (postings (:directives beans) filters)
                                 (partial registry/acc-booking
                                          (:registry beans)))))

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
  ([beans filters]
   (inventory beans
              (conj filters
                    (f/sub-acc (:name-assets (:options beans))
                               (:name-liabilities (:options beans)))))))

(defn income-statement
  "Build balances from `beans`, optionally further filtered.

  Custom directives may be passed in after the filters using :directives.
  "
  ([beans] (income-statement beans []))
  ([beans filters]
   (inventory beans
              (conj filters
                    (f/sub-acc (:name-income (:options beans))
                               (:name-expenses (:options beans)))))))

(defn journal
  "Build a journal of postings from `beans` with running balance.

   Custom directives may be passed in after the filters using :directives."
  ([beans] (journal beans []))
  ([beans filters] (journal/build (postings (:directives beans) filters))))

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
