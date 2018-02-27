(ns provisdom.todo.app
  (:require [provisdom.maali.common :as common]
            [provisdom.todo.rules :as todo]
            [provisdom.todo.view :as view]
            [provisdom.maali.rules :refer-macros [defsession] :as rules]
            [provisdom.todo.integration-test :as test]))


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
  (reset! common/request-id 0) ;;; Reset this to ensure we get the same id's when replaying responses
  (-> (apply rules/insert todo/init-session ::todo/Todo [(todo/new-todo "Rename Cloact to Reagent")
                                                         (todo/new-todo "Add undo demo")
                                                         (todo/new-todo "Make all rendering async")
                                                         (todo/new-todo "Allow any arguments to component functions")])
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
#_(test/abuse session-atom 1000 20)

(defn reload
  []
  (time
    (when (not-empty @history)
      (println "REPLAYING")
      (reset! session-atom (init-session))
      (doseq [[spec response] @history]
        (response-fn spec response))
      (println "REPLAYED" (count @history)))))
