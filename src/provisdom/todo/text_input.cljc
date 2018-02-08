(ns provisdom.todo.text-input
  (:require [clojure.spec.alpha :as s]
            [rum.core :as rum]
            [hasch.core :as hasch]
            [provisdom.maali.rules #?(:clj :refer :cljs :refer-macros) [defrules defqueries defsession def-derive] :as rules]
            [provisdom.todo.common :as common]))

(s/def ::id any?)
(s/def ::value string?)
(s/def ::initial-value ::value)
(s/def ::persistent? boolean?)
(s/def ::committed? boolean?)
(def-derive ::InputRequest ::common/Request (s/keys :req [::id ::initial-value]))
(derive ::InputRequest ::common/Cancellable)
(def-derive ::InputValueRequest ::common/Response (s/merge ::common/Request (s/keys :req [::value])))
(derive ::InputValueRequest ::common/Request)
(def-derive ::InputValueResponse ::common/Response (s/keys :req [::value]))
(def-derive ::CommitRequest ::common/Request (s/keys ::req [::InputRequest]))
(def-derive ::InputResponse ::common/Response (s/keys :req [::value]))

(def request->response
  {::CommitRequest ::common/Response
   ::ValueRequest ::ValueResponse})

;;; Rules
(defrules rules
  [::input!
   "Create an InputValue fact to hold the current value of the Input."
   [?input <- ::InputRequest (= ?initial-value initial-value)]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert-unconditional! ::InputValueRequest (common/request {::common/Request ?input ::value ?initial-value} ::InputValueResponse ?response-fn))]

  [::value-response!
   "Handle the response to ValueRequest, update InputValue."
   [?request <- ::InputValueRequest]
   [::InputValueResponse (= ?request Request) (= ?value value)]
   =>
   (rules/upsert! ::InputValueRequest ?request assoc ::value ?value)]

  [::comittable-input!
   "If the value of the InputValueRequest is not empty, it is legal to commit the value."
   [?input-value <- ::InputValueRequest (= ?input Request) (not-empty value)]
   [?input <- ::InputRequest]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::CommitRequest (common/request {::InputRequest ?input} ::common/Response ?response-fn))]

  [::commit-response!
   "Handle response to CommitRequest, insert InputResponse to signal
    the value has been committed."
   [?request <- ::CommitRequest (= ?input InputRequest)]
   [::common/Response (= ?request Request)]
   [?input <- ::InputRequest]
   [?input-value <- ::InputValueRequest (= ?value value) (= ?input Request)]
   =>
   (rules/insert! ::InputResponse {::common/Request ?input ::value ?value})])

;;; Queries
(defqueries request-queries
  [::commit-request [:?input] [?request <- ::CommitRequest (= ?input InputRequest)] ]
  [::value-request [:?input] [?request <- ::InputValueRequest (= ?input Request)]])

(defqueries view-queries
  [::value [:?input] [::InputValueRequest (= ?input Request) (= ?value value)]])

;;; View
(defn input-id
  "The logical ID used in facts could be anything, including a Clojure map.
   Use this function to make a HTML-friendly id from the hash of whatever data
   is contained in id."
  [id]
  (if (string? id) id (hasch/uuid id)))

(rum/defc text-input
          [session input & attrs]
          (let [attrs-map (into {} (map vec (partition 2 attrs)))
                commit-request (common/query-one :?request session ::commit-request :?input input)
                value-request (common/query-one :?request session ::value-request :?input input)
                value (common/query-one :?value session ::value :?input input)]
            (when value
              [:input (merge attrs-map
                             {:type        "text"
                              :value       value
                              :id          (input-id (::id input))
                              :on-change   #(common/respond-to value-request {::value (-> % .-target .-value)})
                              :on-blur     #(when commit-request (common/respond-to commit-request))
                              :on-key-down #(let [key-code (.-keyCode %)]
                                              (cond
                                                (and text-input (= 27 key-code)) (common/cancel-request input)
                                                (and commit-request (= 13 key-code)) (common/respond-to commit-request)))})])))
