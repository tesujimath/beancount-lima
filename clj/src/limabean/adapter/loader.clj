(ns limabean.adapter.loader
  "Load from beanfile and run plugins"
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [limabean.adapter.debug :as debug]
            [limabean.adapter.plugins :as plugins]
            [limabean.adapter.pod :as pod]
            [limabean.adapter.synthetic-spans :as synthetic-spans]
            [limabean.core.inventory :as inventory]
            [limabean.core.registry :as registry]
            [limabean.core.type :as type]
            [limabean.core.xf :as xf]
            [limabean.macros :as macros]
            [limabean.spec :as spec]))

(defn- dump
  "Dump the beans and return the map"
  [m k]
  (let [basename (-> (io/file (:path m))
                     .getName
                     (str/replace #"\.[^.]*$" ""))
        prefix (str/replace (name k) #"-directives$" "")]
    (debug/dump (get m k) (str basename "." prefix ".beancount"))
    m))

(defn- get-plugins-and-options
  [m]
  (let [{:keys [ok err]} (pod/plugins (:pod m))]
    (if ok
      (let [plugins ok
            options (:ok (pod/options (:pod m)))
            resolved-plugins (plugins/resolve-symbols plugins options)
            plugin-resolution-errors (vec (filter :err resolved-plugins))]
        (cond-> (assoc m
                  :plugins resolved-plugins
                  :options options)
          (seq plugin-resolution-errors) (assoc-in [:error :plugins]
                                           plugin-resolution-errors)))
      (let [spanned-reports (:spanned-reports err)]
        ;; this is where we first encounter parse errors
        (assoc-in m [:error :parser :spanned-reports] spanned-reports)))))

(defn- get-raw-directives
  [m]
  (let [{:keys [ok err]} (pod/directives (:pod m))]
    (if ok
      (let [{:keys [directives warnings]} ok]
        (cond-> (assoc m :raw-directives (type/directives directives))
          (seq warnings) (assoc :raw-warnings warnings)
          (debug/dump-configured?) (dump :raw-directives)))
      (throw (ex-info "unexpected parser failure on directives" err)))))

(defn- run-plugins
  "Run plugins, kind being :raw or :booked"
  [m kind]
  (let [key (get {:raw {:xf :raw-xf,
                        :plugins :raw-plugins,
                        :directives :raw-directives,
                        :xf-directives :raw-xf-directives},
                  :booked {:xf :booked-xf,
                           :plugins :booked-plugins,
                           :directives :booked-directives,
                           :xf-directives :booked-xf-directives}}
                 kind)]
    (if-not (plugins/has-specified-plugins? (:plugins m) (:xf key))
      m
      (try (let [xf-directives- (plugins/run-plugins-of-kind
                                  (get m (:directives key))
                                  (:plugins m)
                                  (:xf key)
                                  (spec/directive-spec kind))
                 xf-directives (type/directives
                                 (synthetic-spans/merge-with-provenance
                                   xf-directives-
                                   (:pod m)))
                 dct-errors (filterv :err xf-directives)]
             (cond-> (assoc m (:xf-directives key) xf-directives)
               (debug/dump-configured?) (dump (:directives key))
               (seq dct-errors) (assoc-in [:error (:plugins key) :dct-errors]
                                  dct-errors)))
           (catch Exception e
             (assoc-in m
               [:error (:plugins key)]
               {:message (let [kind-name (name kind)]
                           (str "Exception thrown by "
                                kind-name
                                " plugin, all "
                                kind-name
                                " plugins ignored")),
                :exception e}))))))

(defn- merge-span-and-provenance
  [booked raw]
  (merge booked (select-keys raw [:span :span-p :provenance])))

(defn- resolve-directives-from-raw
  "Populate spans and provenance from raw directives using idx."
  [booked-directives raw-directives]
  (mapv (fn [booked-dct]
          (let [raw-dct (get raw-directives (:idx booked-dct))
                raw-postings (:postings raw-dct)]
            (cond-> (merge-span-and-provenance booked-dct raw-dct)
              (:postings booked-dct)
                (update
                  :postings
                  (fn [booked-postings]
                    (mapv (fn [booked-pst]
                            (let [raw-pst (get raw-postings (:idx booked-pst))]
                              (merge-span-and-provenance booked-pst raw-pst)))
                      booked-postings))))))
    booked-directives))

(defn- dct-name
  "Return the name of a directive, which is the name of its :dct field except for transactions."
  [dct]
  (let [variant (:dct dct)] (if (= :txn variant) "transaction" (name variant))))

(defn- resolve-report-idx
  "Resolve idx fields from report into spans, preferring :span over :span-p where both exist."
  [[dct-idx pst-idx] raw-directives]
  (let [dct (get raw-directives dct-idx)
        dct-span (or (:span dct) (:span-p dct))
        pst (get (:postings dct) pst-idx)
        pst-span (or (:span pst) (:span-p pst))]
    (cond-> {}
      (or pst dct) (assoc :description (if pst "posting" (dct-name dct)))
      (or pst-span dct-span) (assoc :span (or pst-span dct-span))
      (and (:provenance dct) (or (:span pst) (:span dct)))
        (update :description
                #(str % ", modified by " (str/join " " (:provenance dct))))
      pst (assoc :context ["txn" dct-span]))))

(defn- resolve-report-related
  "Return a function to resolve the related field in a report"
  [raw-directives]
  (fn [idx]
    (let [{:keys [description span]} (resolve-report-idx idx raw-directives)]
      [description span])))

(defn- resolve-report-from-raw
  "Resolve idx to span in indexed report"
  [report raw-directives]
  (let [{:keys [description span context]} (resolve-report-idx (:idx report)
                                                               raw-directives)]
    (cond-> (assoc (select-keys report [:reason :annotation])
              :message (str "invalid " description)
              :span span)
      context (assoc :context context)
      (:related report) (assoc :related
                          (mapv (resolve-report-related raw-directives)
                            (:related report))))))

(defn- resolve-reports-from-raw
  [reports directives]
  (map #(resolve-report-from-raw % directives) reports))

(defn- book-raw-directives
  "Book the raw-xf directives if any, otherwise use the raw directives as parsed.

   Booked directives are annotated with their source span (real or synthetic) using the
   returned `idx` fields."
  [m]
  (try
    (let [{:keys [ok err]} (pod/book (:pod m) (:raw-xf-directives m))
          raw-directives (or (:raw-xf-directives m) (:raw-directives m))]
      (if ok
        (let [{:keys [directives warnings]} ok
              resolved-directives (resolve-directives-from-raw directives
                                                               raw-directives)]
          (cond-> (assoc m
                    :booked-directives (type/directives resolved-directives))
            (debug/dump-configured?) (dump :booked-directives)
            (seq warnings) (assoc :booked-warnings warnings)))
        (let [indexed-reports (:indexed-reports err)
              spanned-reports (or (:spanned-reports err)
                                  (resolve-reports-from-raw indexed-reports
                                                            raw-directives))]
          (assoc-in m [:error :booking] {:spanned-reports spanned-reports}))))
    (catch Exception e
      (assoc-in m [:error :booking] {:exception (Throwable->map e)}))))

(defn- build-inventory
  "Build full inventory with history and merge into `m`."
  [m]
  (let [postings (eduction (xf/postings) (:directives m))
        acc-booking-fn (partial registry/acc-booking (:registry m))]
    (merge m (inventory/build-with-history postings acc-booking-fn))))

(defn load-beanfile
  [path]
  (let [pod (pod/start path)]
    (s/check-asserts true)
    (macros/bind->
      {:path path, :pod pod}
      (get-plugins-and-options)
      (get-raw-directives)
      (run-plugins :raw)
      (book-raw-directives)
      (run-plugins :booked)
      (as-> m (assoc m
                :directives
                  (or (:booked-xf-directives m) (:booked-directives m) [])))
      (as-> m (assoc m :registry (registry/build (:directives m) (:options m))))
      (build-inventory))))
