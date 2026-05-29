(ns limabean.user
  "Top-level limabean functions for use from the REPL."
  (:require [limabean]
            [limabean.adapter.error :as error]
            [limabean.adapter.loader :as loader]
            [limabean.adapter.logging :as logging]
            [limabean.adapter.show :as show]
            [limabean.adapter.pod :as pod]
            [limabean.core.coerce :as coerce]))

(def ^:dynamic *beans*
  "An aggregate of the elements which were used in deriving the directives for the current beanfile.

  Useful in case of failed plugins, for inspecting partial state."
  nil)
(def ^:dynamic *directives*
  "Vector of all directives form the beanfile after running plugins."
  nil)
(def ^:dynamic *options* "Map of options from the beanfile." nil)
(def ^:dynamic *registry*
  "Map of attributes derived from directives and options, e.g. booking method for account."
  nil)

(defn- assign-limabean-globals
  [beans]
  (let [directives (get beans :directives [])
        options (get beans :options {})]
    (alter-var-root #'*beans* (constantly beans))
    (alter-var-root #'*directives* (constantly directives))
    (alter-var-root #'*options* (constantly options))
    (alter-var-root #'*registry* (constantly (:registry beans)))))

(defn load-beanfile
  [path]
  (when (:pod *beans*) (pod/stop (:pod *beans*)))
  (assign-limabean-globals {})
  (logging/initialize)
  (let [beans (loader/load-beanfile path)]
    (binding [*out* *err*]
      (println "[limabean]" (count (:raw-directives beans))
               "directives loaded from" path)
      (error/print-errors beans)
      (assign-limabean-globals beans)
      (when-not (:error beans)
        (println "[limabean]"
                 (count (:directives beans))
                 "directives resulting from booking and running plugins")))
    :ok))

(defn inventory
  "Build inventory from `*beans*` after applying filters, if any."
  [& filters]
  (limabean/inventory *beans* filters))

(defn rollup
  "Build a rollup for the primary currency from an inventory.

  To build for a different currency, simply filter by that currency, e.g
  ```
  (rollup (inventory (f/cur \"CHF\")))
  ```
  "
  [inv]
  (limabean/rollup inv))

(defn balances
  "Build balances from `*beans*`, optionally further filtered."
  [& filters]
  (limabean/balances *beans* filters))

(defn income-statement
  "Calculate income statement across a half-open date range, optionally further filtered."
  [date-range & filters]
  (limabean/income-statement *beans*
                             (apply coerce/->local-date-pair date-range)
                             filters))

(defn journal
  "Build a journal of postings from `*beans*` with running balance."
  [& filters]
  (limabean/journal *beans* filters))

(defn show "Convert `x` to a cell and tabulate it." [x] (show/show x))
