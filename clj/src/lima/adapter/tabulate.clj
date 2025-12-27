(ns lima.adapter.tabulate
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as shell]
            [java-time.api :as jt]
            [lima.core.count :as count]
            [cheshire.core :as cheshire]
            [clojure.string :as str]))

(defn inventory
  "Tabulate an inventory using lima-pod"
  [inv]
  (tabulate-cell (inventory->cell inv)))

(defn inventory->cell
  "Format an inventory into a cell, ready for tabulation"
  [inv]
  (let [accounts (sort (keys inv))]
    (stack (mapv (fn [account]
                   (let [positions (get inv account)
                         positions-cell (case (count positions)
                                          0 EMPTY
                                          1 (position->cell (first positions))
                                          (stack (mapv position->cell
                                                   positions)))]
                     (row [(align-left account) positions-cell] SPACE-MEDIUM)))
             accounts))))

(defn position->cell
  "Format a single position into a cell"
  [pos]
  ;; TODO cost
  (row [(decimal->cell (:units pos)) (align-left (:cur pos))] SPACE-MINOR))

(defn stack "A stack of cells" [cells] {:stack cells})

(defn row
  "Convert a row to cells with gutter"
  [cells gutter]
  {:row [cells gutter]})

(defn decimal->cell
  "Convert decimal to cell anchored at the units digit, so will align with e.g. integers"
  [d]
  (let [s (str d)
        dp (or (str/index-of s ".") (count s))]
    {:anchored [s (dec dp)]}))

(defn align-left "Convert string to left-aligned cell" [s] {:aligned [s :left]})

(defn tabulate-cell
  "Tabulate a cell using lima-pod"
  [cell]
  (let [cell-json (cheshire/generate-string cell)
        tabulated (shell/sh "lima-pod" "tabulate" :in cell-json)]
    (if (= (tabulated :exit) 0)
      (tabulated :out)
      (do (println "lima-pod error" (tabulated :err))
          (throw (Exception. "lima-pod failed"))))))

(def EMPTY {:empty nil})

(def SPACE-MINOR " ")
(def SPACE-MEDIUM "  ")
