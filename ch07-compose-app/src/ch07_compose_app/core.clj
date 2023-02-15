(ns ch07-compose-app.core
  (:require [com.stuartsierra.component :as component])
  (:gen-class))

;;; compose your app

(defrecord Feed [auth status msg-chan response-chan]
  component/Lifecycle
  (start [component]
    (reset! (:status component) :running)
    (process-messages status msg-chan)
    (handle-responses status response-chan)
    component)
  (stop [component]
    (reset! (:status component) :stopped)
    component))

(defn new-feed [auth msg-chan response-chan]
  (->Feed auth (atom :init) msg-chan response-chan))

(defrecord KnowledgeEngine
    [ke-config feed-chan alert-chan rules]
  component/Lifecycle
  (start [component]
    (watch-feeds feed-chan alert-chan)
    component)
  (stop [component]
    component))

(defn new-knowledge-engine
  "create a new knowledge engine with no initial rules"
  [ke-config feed-chan alert-chan]
  (->KnowledgeEngine ke-config feed-chan alert-chan
                     (atom (:rule-set ke-config))))

(defn add-rule
  "add rule to set"
  [ke rule]
  (swap! (:rules ke) conj rule))

(defrecord Approvals
    [approval-config alert-chan
     knowledge-engine response-chan]
  component/Lifecycle
  (start [component]
    (process-alerts alert-chan)
    (process-responses knowledge-engine response-chan)
    component)
  (stop [component]
    component))

(defn new-approvals [approval-config alert-chan response-chan]
  (map->Approvals {:approval-config approval-config
                   :alert-chan alert-chan
                   :response-chan response-chan}))

(defn system [{:keys (twitter facebook knowledge approvals) :as config}]
  (let [twitter-chan (async/chan 100)
        twitter-response-chan (async/chan 100)
        facebook-chan (async/chan 100)
        facebook-response-chan (async/chan 100)
        alert-chan (async/chan 100)
        response-chan (async/chan 100)
        feed-chan (async/merge [twitter-chan facebook-chan])
        response-pub (async/pub response-chan :feed)]
    (async/sub response-pub :twitter twitter-response-chan)
    (async/sub response-pub :facebook facebook-response-chan)
    (component/system-map
     :twitter (new-feed twitter twitter-chan twitter-response-chan)
     :facebook (new-feed facebook facebook-chan facebook-response-chan)
     :knowledge-engine
     (new-knowledge-engine knowledge feed-chan alert-chan)
     :approvals (component/using
                 (new-approvals approvals alert-chan response-chan)
                 [:knowledge-engine]))))

;; system configuration

