(ns limabean.core.account
  (:require [clojure.string :as str]))

(defn sub-acc?
  "Is `child` equal to or a sub account of `parent`"
  [parent child]
  (or (= parent child) (str/starts-with? child (str parent ":"))))
