(ns provisdom.todo.app
  (:require [provisdom.todo.common :as common]
            [provisdom.todo.rules :as todo]
            [provisdom.todo.view :as view]
            [provisdom.maali.rules :refer-macros [defsession] :as rules]
            [cljs.core.async :refer [<! >!] :as async]
            [cljs.pprint :refer [pprint]]
            [provisdom.integration-test :as test]
            [provisdom.maali.tracing :as tracing]
            [clara.tools.inspect :as inspect]))

#_(st/instrument)

(enable-console-print!)

(defn reload
  [])

(def todos [(todo/new-todo "Rename Cloact to Reagent" 1)
            (todo/new-todo "Add undo demo" 2)
            (todo/new-todo "Make all rendering async" 3)
            (todo/new-todo "Allow any arguments to component functions" 4)])

(def *test* true)
(defn init []
  (let [session-atom (atom todo/init-session)
        response-fn (fn [spec response]
                      (swap! session-atom common/handle-response [spec response]))
        session (-> (apply rules/insert @session-atom ::todo/Todo todos)
                    (rules/insert ::common/ResponseFunction {::common/response-fn response-fn})
                    (rules/fire-rules))]
    ;;; Initialize with the session.
    (reset! session-atom session)
    (view/run session-atom)
    (when *test*
       (test/abuse session-atom 1000 20))))

(init)