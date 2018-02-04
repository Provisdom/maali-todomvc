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

(def todos [(todo/new-todo "Rename Cloact to Reagent" 1)
            (todo/new-todo "Add undo demo" 2)
            (todo/new-todo "Make all rendering async" 3)
            (todo/new-todo "Allow any arguments to component functions" 4)])

(def *test* nil)

(defn init []
  (let [session-ch (async/chan 10 todo/handle-response-xf)
        response-fn (fn [spec response]
                      (async/put! session-ch [spec response]))
        session (-> (apply rules/insert todo/session ::todo/Todo todos)
                    (rules/insert ::common/ResponseFunction {::common/response-fn response-fn})
                    (rules/fire-rules))]
    ;;; Initialize with the session.
    (async/put! session-ch [nil session])

    (condp = *test*
      :sync (test/abuse session-ch 1000 20)
      :async (test/abuse-async session-ch 1000 20 10)
      (async/go-loop []
        (when-some [s (<! session-ch)]
          (view/run s)
          (recur))))))
