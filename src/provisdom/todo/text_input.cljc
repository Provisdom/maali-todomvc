(ns provisdom.todo.text-input
  (:require [clojure.spec.alpha :as s]
            [rum.core :as rum]
            [hasch.core :as hasch]
            [provisdom.maali.rules #?(:clj :refer :cljs :refer-macros) [defrules defqueries defsession def-derive] :as rules]
            [provisdom.todo.common :as common]))

(s/def ::correlation-id any?)
(s/def ::value string?)
(s/def ::initial-value ::value)
(def-derive ::InputRequest ::common/Request (s/keys :req [::correlation-id ::initial-value]))
(derive ::InputRequest ::common/Cancellable)
(def-derive ::InputValueRequest ::common/Response (s/merge ::common/Request (s/keys :req [::value])))
(derive ::InputValueRequest ::common/Request)
(def-derive ::InputValueResponse ::common/Response (s/keys :req [::value]))
(def-derive ::CommitRequest ::common/Request (s/keys ::req [::InputRequest]))
(def-derive ::InputResponse ::common/Response (s/keys :req [::value]))

;;; Rules
(defrules base-rules
  [::value-request!
   "Create an InputValue fact to hold the current value of the Input."
   [?input <- ::InputRequest (= ?id correlation-id) (= ?initial-value initial-value)]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert-unconditional! ::InputValueRequest (common/request {::common/Request ?input ::value ?initial-value} ::InputValueResponse ?response-fn))]

  [::value-response!
   "Handle the response to ValueRequest, update InputValue."
   [?request <- ::InputValueRequest]
   [::InputValueResponse (= ?request Request) (= ?value value)]
   =>
   (rules/upsert! ::InputValueRequest ?request assoc ::value ?value)]

  [::commit-response!
   "Handle response to CommitRequest, insert InputResponse to signal
    the value has been committed."
   [?request <- ::CommitRequest (= ?input InputRequest)]
   [::common/Response (= ?request Request)]
   [?input <- ::InputRequest (= ?id correlation-id)]
   [?input-value <- ::InputValueRequest (= ?value value) (= ?input Request)]
   =>
   (rules/insert! ::InputResponse {::common/Request ?input ::value ?value})
   #_(common/respond-to ?input {::value ?value})])

(defrules validation-rules
  [::comittable-input!
   "If the value of the InputValueRequest is not empty, it is legal to commit the value."
   [?input-value <- ::InputValueRequest (= ?input Request) (not-empty value)]
   [?input <- ::InputRequest]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::CommitRequest (common/request {::InputRequest ?input} ::common/Response ?response-fn))])

;;; Queries
(defqueries request-queries
  [::commit-request [:?input] [?request <- ::CommitRequest (= ?input InputRequest)]]
  [::value-request [:?input] [?request <- ::InputValueRequest (= ?input Request)]])

(defqueries view-queries
  [::value [:?input] [::InputValueRequest (= ?value value) (= ?input Request)]])

(defsession init-session [provisdom.todo.common/rules
                          provisdom.todo.text-input/base-rules
                          provisdom.todo.text-input/validation-rules
                          provisdom.todo.text-input/request-queries
                          provisdom.todo.text-input/view-queries])

(defn session-meta
  ([x] (-> x meta :session))
  ([x s] (vary-meta x assoc :session s)))

(defn create
  [id initial-value response-fn]
  (common/request {::correlation-id id ::initial-value initial-value}
                  ::InputResponse response-fn))


;;; View
(defn input-id
  "The logical ID used in facts could be anything, including a Clojure map.
   Use this function to make a HTML-friendly id from the hash of whatever data
   is contained in id."
  [id]
  (if (string? id) id (hasch/uuid id)))

(rum/defc text-input < rum/reactive
  [session input & attrs]
  (let [#_session #_(rum/react (session-meta input))
        attrs-map (into {} (map vec (partition 2 attrs)))
        commit-request (common/query-one :?request session ::commit-request :?input input)
        value-request (common/query-one :?request session ::value-request :?input input)
        value (common/query-one :?value session ::value :?input input)]
    (when value
      [:input
       (cond-> attrs-map
               (not commit-request) (update :class str " error")
               true (merge {:type        "text"
                            :value       value
                            :id          (input-id (::correlation-id input))
                            :on-change   #(common/respond-to value-request {::value (-> % .-target .-value)})
                            :on-blur     #(if commit-request (common/respond-to commit-request) (common/cancel-request input))
                            :on-key-down #(let [key-code (.-keyCode %)]
                                            (case key-code
                                              27 (common/cancel-request input)
                                              13 (if commit-request (common/respond-to commit-request) (common/cancel-request input))
                                              nil))}))])))
