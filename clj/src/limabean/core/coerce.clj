(ns limabean.core.coerce
  "Functions for coercing user input into domain types."
  (:require [java-time.api :as jt]))

(defn ->local-date
  "Convert `args` to a `local-date` or throw user error.

   `args` may be:

    - a `java-time.api/local-date`
    - a string in ISO 8601 format
    - an integer year, with month and day inferred as 1
    - integers year and month, with day inferred as 1
    - integers year, month, and day"
  [args]
  (try (apply jt/local-date args)
       (catch Exception e
         (throw (ex-info "Bad date"
                         (let [msg (if (.getCause e)
                                     (.getMessage (.getCause e))
                                     (.getMessage e))]
                           {:user-error (format "Bad date: %s\n" msg)})
                         e)))))

(defn ->local-date-pair
  "Convert `args` to a pair of `local-date`s or throw user error.

  Precisely 2, 4, or 6 args must be given,
   the first half of which are the first date and the second half the second date,
   and are as described in [[->local-date]]"
  ([b1 e1] [(->local-date [b1]) (->local-date [e1])])
  ([b1 b2 e1 e2] [(->local-date [b1 b2]) (->local-date [e1 e2])])
  ([b1 b2 b3 e1 e2 e3] [(->local-date [b1 b2 b3]) (->local-date [e1 e2 e3])]))

