(ns lima.pp
  (:require [clojure.edn :as edn]))

(require '[clojure.edn :as edn])
(require '[clojure.java.shell :as shell])
(require '[java-time.api :as jt])

(def readers
  {'time/date #(jt/local-date %)})

(defn read-edn-string
  "Read string as LIma PP EDN"
  [s]
  (edn/read-string {:readers readers} s))

(defn book
  "Read EDN from lima-pod book and return or throw"
  [beancount-path]
  (let [booked (shell/sh "lima-pod" "book" beancount-path)]
    (if (= (booked :exit) 0)
      (read-edn-string (booked :out))
      (do (println "lima-pod error" (booked :err))
          (throw (Exception. "lima-pod failed"))
          ))))
