(ns provisdom.todo.app
  (:require-macros [alter-cljs.core :refer [alter-var-root]])
  (:require [provisdom.todo.common :as common]
            [provisdom.todo.rules :as todo]
            [provisdom.todo.view :as view]
            [provisdom.maali.rules :refer-macros [defsession] :as rules]
            [cljs.core.async :refer [<! >!] :as async]
            [cljs.pprint :refer [pprint]]
            [provisdom.integration-test :as test]))


#_(st/instrument)

(enable-console-print!)

(declare init-session)

(defonce session-atom (atom nil))

(defn response-fn
  [spec response]
  (swap! session-atom common/handle-response [spec response]))

(defonce history (atom []))

(defn history-response-fn
  [spec response]
  (swap! history conj [spec response])
  (response-fn spec response))

(defn init-session
  []
  (reset! common/request-id 0)
  (-> (apply rules/insert todo/init-session ::todo/Todo [(todo/new-todo "Rename Cloact to Reagent" 1)
                                                         (todo/new-todo "Add undo demo" 2)
                                                         (todo/new-todo "Make all rendering async" 3)
                                                         (todo/new-todo "Allow any arguments to component functions" 4)])
      ;;; Use history-response-fn here if you want to remember history
      (rules/insert ::common/ResponseFunction {::common/response-fn response-fn})
      (rules/fire-rules)))

;;; HACK - is there a better way to call init-session and view/run only on load?
(defonce hackorama
         (do
           (reset! session-atom (init-session))
           ;;; Comment out view/run to run tests headless
           (view/run session-atom)))


;;; Uncomment to run tests. First argument is the number
;;; of app-level responses simulated, second is delay in
;;; milliseconds between responses.
#_(test/abuse session-atom 100 20)

(defn reload
  []
  (time
    (when (not-empty @history)
      (reset! session-atom (init-session))
      (doseq [[spec response] @history]
        (response-fn spec response))))
  (println "RELOADED" (count @history)))
