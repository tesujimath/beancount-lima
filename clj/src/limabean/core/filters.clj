(ns limabean.core.filters
  "Functions to filter directives and postings according to their various fields, conventionally aliased to `f`.

  Example:
  ```
  (show (inventory (f/date> 2025)))
  ```

  In general these filters reject anything missing the target field."
  (:require [java-time.api :as jt]
            [limabean.core.account :as account]
            [limabean.core.coerce :as coerce]))

(defn date<
  "Predicate for `:date` field to be `< args`.

  `args` may be:

    - a `java-time.api/local-date`
    - a string in ISO 8601 format
    - an integer year, with month and day inferred as 1
    - integers year and month, with day inferred as 1
    - integers year, month, and day"
  [& args]
  (let [end-date (coerce/->local-date args)]
    #(let [date (:date %)] (and date (jt/before? date end-date)))))

(defn date<=
  "Predicate for `:date` field to be `<= args`.

  args are as described in [[date<]]"
  [& args]
  (let [end-date (coerce/->local-date args)]
    #(let [date (:date %)] (and date (jt/not-after? date end-date)))))

(defn date>
  "Predicate for `:date` field to be `> args`.

  args are as described in [[date<]]"
  [& args]
  (let [begin-date (coerce/->local-date args)]
    #(let [date (:date %)] (and date (jt/after? date begin-date)))))

(defn date>=
  "Predicate for `:date` field to be `>= args`.

  args are as described in [[date<]]"
  [& args]
  (let [begin-date (coerce/->local-date args)]
    #(let [date (:date %)] (and date (jt/not-before? date begin-date)))))

(defn- date-between
  [begin-date end-date]
  #(let [date (:date %)]
     (and date (jt/not-before? date begin-date) (jt/before? date end-date))))

(defn date>=<
  "Predicate for `:date` field to be `>= begin-date` and `< end-date`.

  Precisely 2, 4, or 6 args must be given,
  the first half of which are the begin date and the second half the end date,
   and are as described in [[date<]]"
  [& args]
  (let [[b e] (apply coerce/->local-date-pair args)] (date-between b e)))

(defn acc
  "Predicate for `:acc` field to be equal to one of `target-accs`."
  [& target-accs]
  #(let [acc (:acc %)] (and acc (contains? (set target-accs) acc))))

(defn sub-acc
  "Predicate for `:acc` field to be equal to one of `target-accs` or a subaccount of any of them."
  [& target-accs]
  #(let [acc (:acc %)]
     (and acc
          (boolean (some (fn [target] (account/sub-acc? target acc))
                         target-accs)))))

(defn cur
  "Predicate for `:cur` field to be equal to `target-cur`."
  [target-cur]
  #(let [cur (:cur %)] (and cur (= cur target-cur))))

(defn- field-match
  "Helper for whether the given field matches the regex"
  [key regex]
  #(let [field (get % key)] (and field (seq (re-seq regex field)) true)))

(defn payee-match
  "Predicate for whether the payee matches the given regex"
  [regex]
  (field-match :payee regex))

(defn narration-match
  "Predicate for whether the narration matches the given regex"
  [regex]
  (field-match :narration regex))

(defn every-f
  "Combinator filter which selects only what every filter selects"
  [& filters]
  (fn [x] (every? #(% x) filters)))

(defn some-f
  "Combinator filter which selects only what at least one filter selects"
  [& filters]
  (fn [x] (some #(% x) filters)))

(defn none-f
  "Combinator filter which selects only what none of the filters select"
  [& filters]
  (fn [x] (not (some #(% x) filters))))
