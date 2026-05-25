(ns limabean.test.create-golden
  (:require [clojure.java.io :as io]
            [limabean]
            [limabean.adapter.error :as error]
            [limabean.adapter.json]
            [limabean.adapter.loader :as loader]
            [limabean.adapter.print]
            [limabean.adapter.show :as show]
            [limabean.test]
            [zprint.core :refer [zprint]]))


(defn- create-output-file
  [beans f output-file]
  (with-open [w (io/writer output-file)] (binding [*out* w] (f beans))))

(def ^{:private true} OUTPUTS
  {:directives {:filename "directives.edn",
                :f (fn [beans] (zprint (:directives beans))),
                :fyi-filename "directives.fyi.beancount",
                :fyi-f (fn [beans] (print (:directives beans))),
                :required-f (fn [beans exists]
                              (or (contains? beans :booked-xf-directives)
                                  exists))},
   :raw-xf-directives {:filename "raw-xf-directives.edn",
                       :f (fn [beans] (zprint (:raw-xf-directives beans))),
                       :fyi-filename "raw-xf-directives.fyi.beancount",
                       :fyi-f (fn [beans] (print (:raw-xf-directives beans))),
                       :required-f (fn [beans exists]
                                     (or (contains? beans :raw-xf-directives)
                                         exists))},
   :inventory {:filename "inventory",
               :f (fn [beans] (show/show (limabean/inventory beans))),
               :required-f (fn [_beans exists] exists)},
   :rollup {:filename "rollup",
            :f (fn [beans]
                 (show/show (limabean/rollup (limabean/inventory beans)))),
            :required-f (fn [_beans exists] exists)},
   :journal {:filename "journal",
             :f (fn [beans] (show/show (limabean/journal beans))),
             :required-f (fn [_beans exists] exists)}})

(defn ->path
  "Convert string or io/file to Java.nio Path"
  [x]
  (condp instance? x
    java.nio.file.Path x
    java.io.File (.toPath x)
    String (java.nio.file.Paths/get x (make-array String 0))))

(defn mkdir
  [dir]
  (.toFile (java.nio.file.Files/createDirectories
             (->path dir)
             (make-array java.nio.file.attribute.FileAttribute 0))))

(defn update-all
  "Update any golden test output files which exist, or any required by plugins.

   Any errors will be written irrespective of existing golden output."
  [{:keys [root-dir]}]
  (run!
    (fn [{:keys [beanfile golden-dir]}]
      (let [beans (loader/load-beanfile beanfile)
            golden-dir (if (.exists golden-dir) golden-dir (mkdir golden-dir))]
        (if (not (:error beans))
          (run! (fn [[k output]]
                  (let [output-file (io/file golden-dir (:filename output))
                        exists (.exists output-file)
                        fyi-filename (:fyi-filename output)
                        fyi-f (:fyi-f output)
                        required-f (:required-f output)]
                    (when (required-f beans exists)
                      (println "writing" (name k) "to" (.getPath output-file))
                      (create-output-file beans (:f output) output-file)
                      (when (and fyi-filename fyi-f)
                        (let [fyi-file (io/file golden-dir fyi-filename)]
                          (println "writing"
                                   (name k)
                                   "to"
                                   (.getPath fyi-file)
                                   "for information only")
                          (create-output-file beans fyi-f fyi-file))))))
                OUTPUTS)
          (let [error-edn-file (io/file golden-dir "error.edn")
                error-ansi-file (io/file golden-dir "error.ansi")
                trimmed-beans (limabean.test/trim-exception beans)]
            (println "ERROR loading" beanfile
                     "written to" (.getPath error-edn-file))
            (with-open [w (io/writer error-edn-file)]
              (binding [*out* w] (zprint (:error trimmed-beans))))
            (with-open [w (io/writer error-ansi-file)]
              (binding [*out* w] (error/print-errors trimmed-beans)))))))
    (limabean.test/find-golden-tests root-dir :ignore-golden-dirs true)))
