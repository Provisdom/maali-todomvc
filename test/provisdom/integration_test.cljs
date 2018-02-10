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

(def query-pdf
  {::todo/input                    20
   ::todo/edit-request             5
   ::todo/update-done-request      20
   ::todo/retract-todo-request     5
   ::todo/complete-all-request     1
   ::todo/retract-complete-request 1
   ::todo/visibility-request       5})

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

(def request->response
  {::todo/EditRequest             ::common/Response
   ::todo/UpdateDoneRequest       ::todo/UpdateDoneResponse
   ::todo/RetractTodoRequest      ::common/Response
   ::todo/CompleteAllRequest      ::common/Response
   ::todo/RetractCompletedRequest ::common/Response
   ::todo/VisibilityRequest       ::todo/VisibilityResponse})

;;; TODO - implement backspaces and delete all
(defn input-responses
  [input iterations delay-ms]
  (let [session-atom (::common/session input)]
    (async/go
      (let [value (sg/generate (s/gen ::input/value))
            values (reductions #(str %1 %2) "" value)]
        (loop [i 0
               value-index 0
               backspaces 0]
          (when (< i iterations)
            (let [value (nth values value-index)
                  value-request (common/query-one :?request @session-atom ::input/value-request)]
              (common/respond-to value-request {::input/value value})
              (<! (async/timeout (* 1.0 delay-ms)))
              (let [i (inc i)]
                (if (< (rand) 0.01)
                  (recur i 0 0)
                  (let [backspaces (if (and (= 0 backspaces) (< (rand) 0.1)) (rand-int (dec value-index)) (max 0 (dec backspaces)))
                        value-index (if (= 0 backspaces) (min (dec (count values)) (inc value-index)) (max 0 (dec value-index)))]
                    (recur i value-index backspaces)))))))
        (let [commit-request (common/query-one :?request @session-atom ::input/commit-request)]
          (if (and commit-request (< (rand) 0.9))
            (common/respond-to commit-request)
            (common/cancel-request input))))
      true)))

(defn gen-response
  [request]
  (let [response (sg/generate (s/gen (request->response (rules/spec-type request))))]
    (assoc response ::common/Request request)))

(defn select-request
  [session]
  (loop [i 0]
    (when (< i 100)
      (let [query (sample-query)
            requests (rules/query-partial session query)]
        (if (empty? requests)
          (recur (inc i))
          (:?request (rand-nth requests)))))))

(defn abuse
  [session-atom iterations delay-ms]
  (async/go
    (loop [i 0]
      (enable-console-print!)
      (when (< i iterations)
        (let [request (select-request @session-atom)]
          (condp = (rules/spec-type request)
            ::input/InputRequest
            (<! (input-responses request (rand-int 20) delay-ms))

            ::todo/EditRequest
            (do
              (common/respond-to request (gen-response request))
              (let [input (common/query-one :?request @session-atom ::todo/input :?id (::todo/Todo request))]
                (<! (input-responses input (rand-int 20) delay-ms))))

            (common/respond-to request (gen-response request)))
          (<! (async/timeout delay-ms))
          (recur (inc i)))))
    (println "DONE!")))


