(ns ch06-component.core
  (:gen-class))

;;; creating components

(defn main []
  (println "test"))

;; connecting components with channels

;; one-to-one connection (direct)
(comment
 (let [component-1 (make-comp-1)
       output-chan (get-output component-1)
       component-2 (make-comp-2)
       input-chan (get-input component-2)]
   (pipe output-chan input-chan)))

;; one-to-many (fan out)
(comment
  (defn connect-and-tap
    "connect input and output and return channel logging data flow between them"
    [input output]
    (let [m (mult input)
          log (chan (dropping-buffer 100))]
      (tap m output)
      (tap m log)
      log)))

;; pub/sub
(comment
  (defn assemble-chans []
    (let [in (chan 10)
          p (pub in :topic)
          news-ch (chan 10)
          weather-ch (chan 10)]
      (sub p :news news-ch)
      (sub p :weather weather-ch)
      [in news-ch weather-ch])))

;; many-to-one (fan in)
(comment
  (defn combine-channels
    [twitter-chan fb-chan]
    (merge [twitter-chan fb-chan] 100)))

(comment
  (defn mix-channels
    [twitter-chan fb-chan out]
    (let [m (mix out)]
      (admix m twitter-chan)
      (admix m fb-chan)
      (toogle m {twitter-chan {:mute true}
                 fb-chan {:mute true}})
      m)))

;; implementing components

;; two reference states in one structure
(defrecord CustomerAccounts [accounts customers])
(defn make-customer-accounts []
  (map->CustomerAccounts {:accounts (atom {})
                          :customers (atom {})}))

;; life cycle
(defrecord KnowledgeEngine
    [config ch-in ch-out
     rules active])

(defn make-knowledge-engine
  [config ch-in ch-out rule-set]
  (->KnowledgeEngine config ch-in ch-out (atom rule-set) (atom false)))

(defn start-knowledge-engine
  [{:keys (ch-in ch-out rules active) :as ke}]
  (reset! active true)
  (go-loop [request (<! ch-in)
            response (fire-rules ke request)]
    (>! ch-out response)
    (when @active (recur)))
  ke)

(defn stop-knowledge-engine
  [{:keys (ch-out active) :as ke}]
  (reset! active false)
  (async/close! ch-out)
  ke)
