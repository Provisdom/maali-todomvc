(ns provisdom.todo.app
  (:require [provisdom.todo.common :as common]
            [provisdom.todo.rules :as todo]
            [provisdom.todo.view :as view]
            [provisdom.maali.rules :as rules]
            [cljs.core.async :refer [<! >!] :as async]
            [cljs.pprint :refer [pprint]]
            [provisdom.integration-test :as test]))

#_(st/instrument)

(enable-console-print!)

(defn reload
  [])

;;; Handy function to help initialize the list of todos
(defn todo-response
  [{::todo/keys [new-todo-request] :as result} todo]
  (let [{::common/keys [response-fn]} new-todo-request]
    (response-fn {::common/Request new-todo-request ::todo/Todo todo})))

(def todos [(todo/new-todo "Rename Cloact to Reagent" 1)
            (todo/new-todo "Add undo demo" 2)
            (todo/new-todo "Make all rendering async" 3)
            (todo/new-todo "Allow any arguments to component functions" 4)])

(def session (apply rules/insert todo/session ::todo/Todo todos))

(def *test* nil)
;;; Input channel for responses


(defn init []
  #_(reset! view/session-atom (rules/fire-rules session))

  (let [session-ch (async/chan 10 todo/handle-response-xf)
        response-ch (async/chan 10)
        response-fn (fn [spec response]
                      (async/put! response-ch [spec response]))
        session (-> session
                    (rules/insert ::common/ResponseFunction {::common/response-fn response-fn})
                    (rules/fire-rules))]
    ;;; Initialize with the session.
    (async/put! session-ch [nil session])

    ;;; Connect the response channel to the processing pipeline.
    (async/pipe response-ch session-ch)

    (condp = *test*
      :sync (test/abuse session-ch 1000 20)
      :async (test/abuse-async session-ch 1000 20 10)
      (async/go-loop []
        (when-some [s (<! session-ch)]
          (view/run s)
          (recur))))))
