(ns provisdom.todo.integration-test
  (:require [provisdom.maali.common :as common]
            [provisdom.todo.text-input :as input]
            [provisdom.todo.rules :as todo]
            [provisdom.maali.rules :refer-macros [defqueries defsession check-invariant] :as rules]
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

(defn input-responses
  [session-atom input iterations delay-ms]
  (async/go
    (let [value (sg/generate (s/gen ::input/value))
          values (reductions #(str %1 %2) "" value)]
      (loop [i 0
             value-index 0
             backspaces 0]
        (when (< i iterations)
          (let [value (nth values value-index)
                value-request (rules/query-one :?request @session-atom ::input/value-request :?input input)]
            (common/respond-to value-request {::input/value value})
            (<! (async/timeout delay-ms))
            (let [i (inc i)]
              (if (< (rand) 0.01)
                (recur i 0 0)
                (let [backspaces (if (and (= 0 backspaces) (< (rand) 0.1)) (rand-int (dec value-index)) (max 0 (dec backspaces)))
                      value-index (if (= 0 backspaces) (min (dec (count values)) (inc value-index)) (max 0 (dec value-index)))]
                  (recur i value-index backspaces)))))))
      (let [commit-request (rules/query-one :?request @session-atom ::input/commit-request :?input input)
            commit? (and commit-request (< (rand) 0.9))]
        (if commit?
          (common/respond-to commit-request)
          (common/cancel-request input))
        commit?))))


(defn gen-response
  [request]
  (let [response (sg/generate (s/gen (request->response (rules/spec-type request))))]
    (assoc response ::common/Request request)))

(defn gen-visibility-response
  [visibility-request]
  (let [response (gen-response visibility-request)]
    (assoc response ::todo/visibility (sg/generate (s/gen (::todo/visibilities visibility-request))))))

(defn select-request
  [session]
  (loop [i 0]
    (when (< i 100)
      (let [query (sample-query)
            requests (rules/query-partial session query)]
        (if (empty? requests)
          (recur (inc i))
          [query (:?request (rand-nth requests))])))))

(defn check-invariants
  [request result old-session new-session]
  (let [old-todos (rules/query-many :?todo old-session ::todo/todos)
        new-todos (rules/query-many :?todo new-session ::todo/todos)]

    (condp = (rules/spec-type request)
      ::todo/RetractTodoRequest
      (check-invariant request result (= 1 (- (count old-todos) (count new-todos)))
                       {:old-count (count old-todos)
                        :new-count (count new-todos)})

      ::input/InputRequest
      (check-invariant request result (= (if result 1 0) (- (count new-todos) (count old-todos)))
                       {:commit? result
                        :old-count (count old-todos)
                        :new-count (count new-todos)})

      ::todo/EditRequest
      (check-invariant request result (= 0 (- (count new-todos) (count old-todos)))
                       {:old-count (count old-todos)
                        :new-count (count new-todos)})

      ::todo/RetractCompletedRequest
      (check-invariant request result (not-any? #(= true (::todo/done %)) new-todos)
                       {:dones (map ::todo/done new-todos)})

      ::todo/CompleteAllRequest
      (do
        (check-invariant request result (= 0 (- (count new-todos) (count old-todos)))
                         {:old-count (count old-todos)
                          :new-count (count new-todos)})
        (check-invariant request result (not-any? #(= (not (::todo/done request)) (::todo/done %)) new-todos)
                         {:toggle (not (::todo/done request))
                          :dones (map ::todo/done new-todos)}))

      ::todo/VisibilityRequest
      (do
        (check-invariant request result (= 0 (- (count new-todos) (count old-todos))))
        {:old-count (count old-todos)
         :new-count (count new-todos)}
        (check-invariant request result ((::todo/visibilities request) (::todo/visibility result))
                         {:visibility (::todo/visibility result)
                          :visibilities (::todo/visibilities request)})
        (let [visible-todos (rules/query-many :?todo new-session ::todo/visible-todos)]
          (condp = (::todo/visibility result)
            :all (check-invariant request result (= (count visible-todos) (count old-todos))
                                  {:visible-count (count visible-todos)
                                   :old-count (count old-todos)})
            :active (check-invariant request result (every? #(= false (::todo/done %)) visible-todos)
                                     {:dones (map ::todo/done new-todos)})
            :completed (check-invariant request result (every? #(= true (::todo/done %)) visible-todos)
                                        {:dones (map ::todo/done new-todos)}))))
      nil)))


(defn abuse
  [session-atom iterations delay-ms]
  (async/go
    (enable-console-print!)
    (time
      (loop [i 0]
        (enable-console-print!)
        (when (< i iterations)
          (let [session @session-atom
                [query request] (select-request session)
                result (condp = (rules/spec-type request)
                         ::input/InputRequest
                         (<! (input-responses session-atom request (rand-int 20) (* 0.1 delay-ms)))

                         ::todo/EditRequest
                         (do
                           (common/respond-to request (gen-response request))
                           (let [input (rules/query-one :?request @session-atom ::todo/input :?id (::todo/Todo request))]
                             (<! (input-responses session-atom input (rand-int 20) (* 0.1 delay-ms)))))

                         ::todo/VisibilityRequest
                         (let [response (gen-visibility-response request)]
                           (common/respond-to request response)
                           response)

                         (let [response (gen-response request)]
                           (common/respond-to request response)
                           response))]

            (check-invariants request result session @session-atom)

            (<! (async/timeout delay-ms))
            (recur (inc i))))))
    (println "DONE!")))
