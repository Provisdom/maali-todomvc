(ns provisdom.integration-test
  (:require [provisdom.todo.common :as common]
            [provisdom.todo.text-input :as input]
            [provisdom.todo.rules :as todo]
            [provisdom.todo.view :as view]
            [provisdom.maali.rules :refer-macros [defqueries defsession] :as rules]
            [clara.tools.inspect :as inspect]
            [cljs.core.async :refer [<! >!] :as async]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sg]
            [cljs.pprint :refer [pprint]]))

(defqueries request-queries
  [::inputs [] [?request <- ::input/InputRequest]]
  [::edit-requests [] [?request <- ::todo/EditRequest]]
  [::update-done-requests [] [?request <- ::todo/UpdateDoneRequest]]
  [::retract-todo-requests [] [?request <- ::todo/RetractTodoRequest]]
  [::complete-all-request [] [?request <- ::todo/CompleteAllRequest]]
  [::retract-complete-request [] [?request <- ::todo/RetractCompletedRequest]]
  [::visibility-request [] [?request <- ::todo/VisibilityRequest]])

(def query-pdf
  {::inputs                   20
   ::edit-requests            5
   ::update-done-requests     20
   ::retract-todo-requests    3
   ::complete-all-request     1
   ::retract-complete-request 1
   ::visibility-request       5})

(def query-cdf
  (let [z (apply + (vals query-pdf))
        [cdf p] (reduce (fn [[cdf p] [k v]]
                          (let [p (+ p (/ v z))]
                            [(assoc cdf p k) p]))
                        [(sorted-map) 0] query-pdf)]
    cdf))

(defn sample-query
  []
  (let [r (rand)]
    (val (first (filter (fn [[p q]] (>= p r)) query-cdf)))))

(def todos [(todo/new-todo "Rename Cloact to Reagent")
            (todo/new-todo "Add undo demo")
            (todo/new-todo "Make all rendering async")
            (todo/new-todo "Allow any arguments to component functions")])

(defsession init-session [provisdom.todo.common/rules
                          provisdom.todo.text-input/rules
                          provisdom.todo.text-input/view-queries
                          provisdom.todo.text-input/request-queries
                          provisdom.todo.rules/rules
                          provisdom.todo.rules/view-queries
                          provisdom.todo.rules/request-queries
                          provisdom.integration-test/request-queries])

(def request->response
  {::todo/EditRequest             ::common/Response
   ::todo/UpdateDoneRequest       ::todo/UpdateDoneResponse
   ::todo/RetractTodoRequest      ::common/Response
   ::todo/CompleteAllRequest      ::common/Response
   ::todo/RetractCompletedRequest ::common/Response
   ::todo/VisibilityRequest       ::todo/VisibilityResponse})

(defn input-responses
  [session-atom input delay-ms]
  (async/go
    (let [value (sg/generate (s/gen ::input/value))
          values (reductions #(str %1 %2) (first value) (rest value))]
      (loop [[value & values] values]
        (if value
          (let [value-request (common/query-one :?request @session-atom ::input/value-request :?input input)]
            (common/respond-to value-request {::input/value value})
            (<! (async/timeout (* 0.1 delay-ms)))
            (recur values))))
      (let [commit-request (common/query-one :?request @session-atom ::input/commit-request :?input input)]
        (if (and commit-request (< (rand) 0.9))
          (common/respond-to commit-request)
          (common/cancel-request input))))
    true))

(defn gen-response
  [request]
  (let [response (sg/generate (s/gen (request->response (rules/spec-type request))))]
    (assoc response ::common/Request request)))

(defn select-request
  [session]
  (loop [i 0]
    (when (< i 100)
      (let [query (sample-query)
            requests (rules/query session query)]
        (if (empty? requests)
          (recur (inc i))
          (:?request (rand-nth requests)))))))

(defn abuse
  [iterations delay-ms]
  (let [session-atom (atom init-session)
        response-fn (fn [spec response]
                      (swap! session-atom todo/handle-response [spec response])
                      #_(inspect/explain-activations @session-atom))
        session (-> (apply rules/insert init-session ::todo/Todo todos)
                    (rules/insert ::todo/Anchor {::common/time (common/now)})
                    (rules/insert ::common/ResponseFunction {::common/response-fn response-fn})
                    (rules/fire-rules))]
    (add-watch session-atom :foo (fn [_ _ _ session] (view/run session)))
    (reset! session-atom session)
    (async/go
      (loop [i 0]
        (enable-console-print!)
        (when (< i iterations)
          (let [request (select-request @session-atom)]
            (condp = (rules/spec-type request)
              ::input/InputRequest
              (<! (input-responses session-atom request delay-ms))

              ::todo/EditRequest
              (do
                (common/respond-to request (gen-response request))
                (let [input (common/query-one :?request @session-atom ::todo/input :?id (::todo/Todo request))]
                  (<! (input-responses session-atom input delay-ms))))

              (common/respond-to request (gen-response request)))
            (<! (async/timeout delay-ms))
            (recur (inc i)))))
      (println "DONE!"))))


(defn abuse-async
  [iterations delay-ms max-responses]
  (async/go-loop [i 0
                  session (<! session-ch)]
    (view/run session)
    (enable-console-print!)
    (when (< i iterations)
      (let [n (if (< (rand) (/ 2 (dec max-responses))) (rand-int max-responses) 0)
            requests (repeatedly n #(select-request session))]
        (let [s (if (= 0 n)
                  session
                  (loop [[request & requests] requests
                         session session]
                    (if request
                      (let [response-fn (::common/response-fn request)
                            response (gen-response request)]
                        (response-fn response)
                        (recur requests (<! session-ch)))
                      session)))]
          (<! (async/timeout delay-ms))
          (recur (+ i (inc n)) s))))
    (println "DONE!")))

