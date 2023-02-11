(ns ch05-cores.core
  (:import [java.util.concurrent LinkedBlockingQueue Executors])
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
