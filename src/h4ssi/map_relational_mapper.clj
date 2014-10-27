(ns h4ssi.map-relational-mapper
  (:require [clojure.string :as string]))

(defn- to-camel-case [s] (string/join (map string/capitalize (string/split s #"-"))))
(defn- to-camel-case-sym [s] (symbol (to-camel-case (if (symbol? s) (name s) s))))

#_(to-camel-case "test-case")
#_(to-camel-case-sym 'test-case)

(defmacro defcollect [singular-name plural-name unique-cols mapping]
  (let [combined-name         (fn [& parts] (symbol (apply str (map name parts))))
        prefixed              (fn [col-name] (combined-name singular-name "_" col-name))
        unique-cols           (if (coll? unique-cols) unique-cols [unique-cols])
        multi-record-children (map first (filter coll? mapping))]
    `(do
       (defrecord ~(to-camel-case-sym singular-name) [~@(map #(symbol (name (if (coll? %) (first %) %))) mapping)])
       (defn ~(combined-name "collect-unique-cols-for-" singular-name) [~'row]
         ~(mapv #(list (keyword (prefixed %)) 'row) unique-cols))

       (defn ~(combined-name "collect-" singular-name) [[~'r & ~'rs :as ~'rrs]]
         (cond
          ; we are at end
          (empty? ~'rrs)
          nil

          ; no valid record, skip it
          (or
           (nil? ~'r)
           (every? nil? (~(combined-name "collect-unique-cols-for-" singular-name) ~'r)))
          (~(combined-name "collect-" singular-name) ~'rs)

          :else
          (let [~'remaining-rows ~'rrs
                ~@(mapcat
                   #(list [%, 'remaining-rows] `(~(combined-name "collect-" % "-with-remaining-rows") ~'remaining-rows))
                   multi-record-children)]
            (~(combined-name "->" (to-camel-case-sym singular-name))
              ~@(map
                 #(cond
                   (coll? %)
                   (first %)

                   (keyword? %)
                   `(keyword (~(keyword (combined-name singular-name "_" %))  ~'r))

                   :else
                   `(~(keyword (combined-name singular-name "_" %))  ~'r))
                 mapping)))))

       (defn ~(combined-name "collect-" plural-name "-with-remaining-rows") [[~'r & ~'rs :as ~'rrs]]
         (if (empty? ~'rrs)
           [nil nil] ; safe guard against invalid input
           (let [~'first-record-unique-cols (~(combined-name "collect-unique-cols-for-" singular-name) ~'r)]
             (if (every? nil? ~'first-record-unique-cols)
               [nil ~'rrs] ; no match on outer join, so there is nothing to collect at all
               (loop [~'current-record-rows        [~'r]
                      [~'r & ~'rs :as ~'rrs]       ~'rs ; we process first ~'r directly on loop initialization
                      ~'current-record-unique-cols ~'first-record-unique-cols
                      ~'records                    nil]
                 (let [~'current-row-unique-cols (~(combined-name "collect-unique-cols-for-" singular-name) ~'r)]
                   (cond
                    (or
                     (empty? ~'rrs)
                     (every? nil? ~'current-row-unique-cols)) ; we are at end, but we also have something collected, convert it into object now
                    [(concat
                      ~'records
                      (lazy-seq (cons (~(combined-name "collect-" singular-name) ~'current-record-rows) nil)))
                     ~'rrs]

                    (not= ~'current-record-unique-cols ~'current-row-unique-cols) ; record switch immanent
                    (recur
                     [~'r]
                     ~'rs
                     ~'current-row-unique-cols
                     (concat
                      ~'records
                      (lazy-seq (cons (~(combined-name "collect-" singular-name) ~'current-record-rows) nil))))

                    :else
                    (recur
                     (conj ~'current-record-rows ~'r)
                     ~'rs
                     ~'current-record-unique-cols
                     ~'records))))))))

       (def ~(combined-name "collect-" plural-name) (comp first ~(combined-name "collect-" plural-name "-with-remaining-rows"))))))

#_(require '[clojure.pprint :as pprint])
#_(pprint/pprint (macroexpand-1 '(defcollect record records id [id field [sub-records]])))

