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

#_(do
  (defsession init-session [provisdom.todo.common/rules
                            provisdom.todo.text-input/rules
                            provisdom.todo.text-input/view-queries
                            provisdom.todo.text-input/request-queries
                            provisdom.todo.rules/rules
                            provisdom.todo.rules/view-queries
                            provisdom.todo.rules/request-queries])

  def *test* nil)

(do
  (defsession init-session [provisdom.todo.common/rules
                            provisdom.todo.text-input/rules
                            provisdom.todo.text-input/view-queries
                            provisdom.todo.text-input/request-queries
                            provisdom.todo.rules/rules
                            provisdom.todo.rules/view-queries
                            provisdom.todo.rules/request-queries

                            provisdom.integration-test/request-queries])

  (def *test* :sync))

(defn init []
  (let [session-atom (atom init-session)
        response-fn (fn [spec response]
                      (swap! session-atom todo/handle-response [spec response]))
        session (-> (apply rules/insert init-session ::todo/Todo todos)
                    (rules/insert ::todo/Anchor {::common/time (common/now)})
                    (rules/insert ::common/ResponseFunction {::common/response-fn response-fn})
                    (rules/fire-rules))]
    ;;; Initialize with the session.
    (add-watch session-atom :foo (fn [_ _ _ session] (view/run session)))
    (reset! session-atom session)
    (condp = *test*
      :sync (test/abuse session-atom 100 20)
      :async (test/abuse-async 10 20 3)

      nil)))
