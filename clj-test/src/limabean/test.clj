(ns limabean.test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.test :refer [is testing]]
            [clojure.walk :as walk]
            [limabean]
            [limabean.adapter.edn :as limabean-edn]
            [limabean.adapter.error :as error]
            [limabean.adapter.json]
            [limabean.adapter.print]
            [limabean.app :as app]
            [matcho.core :as matcho])
  (:import [java.nio.file Files]))

(defn trim-exception
  "Trim any exception for test comparison"
  [data]
  (walk/postwalk
    (fn [x]
      (if (and (map? x) (:exception x) (instance? Throwable (:exception x)))
        (update x :exception (fn [exc] {:message (.getMessage exc)}))
        x))
    data))

(defn find-golden-tests
  "Walk the filesystem from root-dir looking for beancount files and golden directories, ignoring .fyi.beancount files."
  [root-dir & {:keys [ignore-golden-dirs]}]
  (let [root-dir (.getPath (io/file root-dir))]
    (into []
          (comp (filter #(let [filename (.getName %)]
                           (and (str/ends-with? filename ".beancount")
                                (not (str/ends-with? filename
                                                     ".fyi.beancount")))))
                (map (fn [beanfile]
                       (let [base-path (io/file (str/replace (.getPath beanfile)
                                                             #".beancount$"
                                                             ""))
                             test-name (.getName base-path)
                             golden-dir (io/file (str base-path ".golden"))]
                         {:test-name test-name,
                          :beanfile (.getPath beanfile),
                          :golden-dir golden-dir})))
                (filter #(or ignore-golden-dirs (.exists (:golden-dir %)))))
          (file-seq (io/file root-dir)))))


(defn- temp-file-path
  [prefix ext]
  (str (Files/createTempFile prefix
                             ext
                             (make-array java.nio.file.attribute.FileAttribute
                                         0))))

(defn- diff
  "Return diff as a string, or nil if no diffs"
  [actual expected]
  (let [diff (shell/sh "diff" actual expected)]
    (case (:exit diff)
      0 nil
      1 (:out diff)
      (throw (Exception. (str "unexpected diff failure, exit code"
                              (:exit diff)
                              (:err diff)))))))

(defn- golden-text
  "Golden test of actual and expected paths"
  [test-name actual expected]
  (let [diffs (diff actual expected)]
    (if diffs
      (do
        (println
          (format
            "%s actual != expected\n====================\n%s\n====================\n"
            test-name
            diffs))
        false)
      true)))

(defmacro with-out-file-path
  "Run forms with `*out*` bound to a writer for `out-file-path`."
  [out-file-path & forms]
  `(with-open [w# (io/writer ~out-file-path)] (binding [*out* w#] ~@forms)))

(defn app-tests
  [root-dir]
  (doseq [{:keys [test-name beanfile golden-dir]} (find-golden-tests root-dir)]
    (testing test-name
      (doseq [query ["inventory" "rollup" "journal"]]
        (let [actual (temp-file-path test-name query)
              expected (io/file golden-dir query)
              query-expr (case query
                           "rollup" "(show (rollup (inventory)))"
                           (format "(show (%s))" query))]
          (when (.exists expected)
            (with-out-file-path actual
                                (app/run {:beanfile beanfile,
                                          :eval query-expr}))
            (is (golden-text (format "%s.%s" test-name query)
                             actual
                             (.getPath expected)))))))))

(defn api-tests
  [root-dir]
  (doseq [{:keys [test-name beanfile golden-dir]} (find-golden-tests root-dir)]
    (testing test-name
      (let [beans (delay (try (println "loading" beanfile)
                              (limabean/load-beanfile beanfile)
                              (catch Exception e
                                (println "Exception while processing"
                                         beanfile
                                         (.getMessage e))
                                (pprint (Throwable->map e))
                                nil)))]
        (doseq [key [:raw-xf-directives :directives :error]]
          (let [expected-file (io/file golden-dir (str (name key) ".edn"))]
            (when (.exists expected-file)
              (let [actual (cond-> (force beans)
                             (= :error key) (trim-exception))
                    expected (limabean-edn/read-string (slurp expected-file))
                    expected-strict (walk/postwalk
                                      (fn [x]
                                        (if (instance? clojure.lang.IObj x)
                                          (with-meta x {:matcho/strict true})
                                          x))
                                      expected)]
                (matcho/assert expected-strict (get actual key))
                (when (= key :error)
                  (let [actual-ansi-file (temp-file-path test-name "error.ansi")
                        _ (println "writing ANSI output to" actual-ansi-file)
                        expected-ansi-file (io/file golden-dir
                                                    (str (name key) ".ansi"))]
                    (with-out-file-path actual-ansi-file
                                        (error/print-errors actual))
                    (is (golden-text (format "%s/error.ansi" test-name)
                                     actual-ansi-file
                                     (.getPath expected-ansi-file)))))))))))))
