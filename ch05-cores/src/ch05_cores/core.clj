(ns ch05-cores.core
  (:import [java.util.concurrent LinkedBlockingQueue Executors])
  (:require [clojure.core.reducers :as r]
            [clojure.core.async :refer [chan dropping-buffer
                                        sliding-buffer pipeline
                                        >!! <!! go
                                        >! <! ;; for go blocks
                                        ]])
  (:gen-class))

;;; chapter 5 use your cores

;; push waiting to the background
(def pageview-stat (agent 0))

;; check every 10th update
(comment
  (add-watch
   pageview-stat
   :pageview
   (fn [key agent old new]
     (when (zero? (mod new 10))
       (remote-send key new)))))

(defn inc-stat [stat]
  (send-off stat inc))

;; querying prices of a product in stores linearly
(comment
  (defn query-stores [produce stores]
    (for [store stores]
      (query store product))))

;; querying prices simultaneously using futures
(comment
  (defn query-stores [product stores]
    (for [store stores]
      (future (query store product))))) ;; return java.lang.Future, needs to be realized

;; querying and realizing with future and deref
(comment
  (defn query-stores [product stores]
    (let [futures (doall
                   (for [store stores]
                     (future (query store product))))]
      (map deref futures))))


;;; promises
(comment
  (defn launch-times []
    (let [begin-promise (promise)
          end-promise (promise)]
      (future (deliver begin-promise (System/currentTimeMillis))
              (long-running-task)
              (deliver end-promise (System/currentTimeMillis)))
      (println "task begin at" @begin-promise)
      (println "task end at" @end-promise))))

;;; queues and workers
;; wrapping persistent queue in ref (not good example)
(defn queue
  "create a new stateful queue"
  []
  (ref clojure.lang.PersistentQueue/EMPTY))

(defn enq
  "enqueue item in q"
  [q item]
  (dosync
   (alter q conj item)))

(defn deq
  "dequeue item from q (nil if none)"
  [q]
  (dosync
   (let [item (peek @q)]
     (alter q pop)
     item)))

;; use java queue instead
(defn pusher [q n]
  (loop [i 0]
    (when (< i n)
      (.put q i)
      (recur (inc i))))
  (.put q :END))

(defn popper [q]
  (loop [items []]
    (let [item (.take q)]
      (if (= item :END)
        items
        (recur (conj items item))))))

(defn flow [n]
  (let [q (LinkedBlockingQueue.)
        consumer (future (popper q))
        begin (System/currentTimeMillis)
        producer (future (pusher q n))
        received @consumer
        end (System/currentTimeMillis)]
    (println "Received:" (count received) "in" (- end begin) "ms")))

;; making threads
(def processors (.availableProcessors (Runtime/getRuntime)))
(defonce executor (Executors/newFixedThreadPool processors))

(defn submit-task [^Runnable task]
  (.submit executor task))

;;; parallelism with reducers
(def shipping-data {:id "1234"
                    :class :ground
                    :weight 10
                    :volume 300})

(defn ground? [product]
  (= :ground (:class product)))

(defn ground-weight [products]
  (->> products
       (filter ground?)
       (map :weight)
       (reduce +)))

;; using reducers
;; reducers will be significantly faster since it uses multithreads,
;; depending on the data partitions (default 512).
(defn ground-weight-reducer [products]
  (->> products
       (r/filter ground?)
       (r/map :weight)
       (r/fold +)))

;;; thinking in process
;; (using core.async)

;; channels
;; creating channels
(comment
  (chan) ;; unbuffered channel (length 0)
  (chan 10) ;; buffered length 10
  (chan (dropping-buffer 10)) ;; drop new values when full
  (chan (sliding-buffer 10)) ;; drop old values when full
  )

(def c (chan 1))
(>!! c "halo") ;; put halo to channel
;; => true
(println (<!! c)) ;; take value from channel
;; halo
;; => nil

;; go blocks
(defn go-print
  "pull messages from channel c and print them"
  [c]
  (go
    (loop []
      (when-some [val (<! c)]
        (println "Received a message:" val)
        (recur)))))

;; processing a stream of social media messages
;; using series of transformations
(def parse-words
  (map #(set (clojure.string/split % #"\s"))))

;; filter messages that contain a word of interest
(def interesting
  (filter #(contains? % "Clojure")))

;; detect sentiment based on different word list
(defn match [search-words message-words]
  (count (clojure.set/intersection search-words message-words)))

(def happy
  (partial match #{"happy" "awesome" "rocks" "amazing"}))

(def sad
  (partial match #{"sad" "bug" "crash"}))

(def score
  (map #(hash-map :words %1
                  :happy (happy %1)
                  :sad (sad %1))))

;; define pipeline stage
(defn sentiment-stage [in out]
  (let [xf (comp parse-words interesting score)]
    (pipeline 4 out xf in)))

;; or split the stage
(defn interesting-stage-split [in intermediate]
  (let [xf (comp parse-words interesting)]
    (pipeline 4 intermediate xf in)))

(defn score-stage-split [intermediate out]
  (pipeline 1 out score intermediate))

(defn assemble-stages [in out]
  (let [intermediate (chan 100)]
    (interesting-stage-split in intermediate)
    (score-stage-split intermediate out)))
