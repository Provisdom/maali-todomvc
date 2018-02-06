(ns provisdom.todo.app
  (:require [provisdom.todo.common :as common]
            [provisdom.todo.rules :as todo]
            [provisdom.todo.view :as view]
            [provisdom.maali.rules :refer-macros [defsession] :as rules]
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

(defsession session [provisdom.todo.common/rules
                     provisdom.todo.text-input/rules
                     provisdom.todo.text-input/view-queries
                     provisdom.todo.text-input/request-queries
                     provisdom.todo.rules/rules
                     provisdom.todo.rules/view-queries
                     provisdom.todo.rules/request-queries])

(def *test* :sync)
(defn init []
  (condp = *test*
    :sync (test/abuse 10 200)
    :async (test/abuse-async 10 20 3)

    (let [session-ch (async/chan 10 todo/handle-response-xf)
          response-fn (fn [spec response]
                        (async/put! session-ch [spec response]))
          session (-> (apply rules/insert session ::todo/Todo todos)
                      (rules/insert ::todo/Anchor {::common/time (common/now)})
                      (rules/insert ::common/ResponseFunction {::common/response-fn response-fn})
                      (rules/fire-rules))]
      ;;; Initialize with the session.
      (async/put! session-ch [nil session])
      (async/go-loop []
        (when-some [s (<! session-ch)]
          (view/run s)
          (recur))))))
