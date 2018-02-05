(ns provisdom.todo.text-input
  (:require [clojure.spec.alpha :as s]
            [rum.core :as rum]
            [hasch.core :as hasch]
            [provisdom.maali.rules #?(:clj :refer :cljs :refer-macros) [defrules defqueries defsession def-derive] :as rules]
            [provisdom.todo.common :as common]))

(s/def ::id any?)
(s/def ::value string?)
(s/def ::persistent? boolean?)
(s/def ::committed? boolean?)
(def-derive ::Input ::common/RequestWithResponseFn (s/keys :req [::id]))
(derive ::Input ::common/Cancellable)
(def-derive ::InputValue ::common/Response (s/keys :req [::value]))
(def-derive ::InputResponse ::InputValue)

(def-derive ::InputRequest ::common/Request (s/keys :req [::Input]))
(def-derive ::CommitRequest ::InputRequest)
(def-derive ::ValueRequest ::InputRequest)
(def-derive ::ValueResponse ::common/Response (s/keys :req [::value]))

(def request->response
  {::CommitRequest ::common/Response
   ::ValueRequest ::ValueResponse})

;;; Rules
(defrules rules
  [::input!
   "Create an InputValue fact to hold the current value of the Input."
   [?input <- ::Input]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert-unconditional! ::InputValue {::common/Request ?input ::value ""})]

  [::comittable-input!
   "If the value of the Input is not empty, it is legal to commit the value."
   [?input <- ::Input]
   [::InputValue (= ?input Request) (not-empty value)]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::CommitRequest {::Input ?input ::common/response-fn (common/response ?response-fn ::common/Response)})]

  [::value-request!
   "Request an new value for the Input."
   [?input-value <- ::InputValue (= ?input Request)]
   [?input <- ::Input]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::ValueRequest {::Input ?input ::common/response-fn (common/response ?response-fn ::ValueResponse)})]

  [::value-response!
   "Handle the response to ValueRequest, update InputValue."
   [?request <- ::ValueRequest (= ?input Input)]
   [::ValueResponse (= ?request Request) (= ?value value)]
   [?input-value <- ::InputValue (= ?input Request)]
   =>
   (rules/upsert! ::InputValue ?input-value assoc ::value ?value)]

  [::commit-response!
   "Handle response to CommitRequest, insert InputResponse to signal
    the value has been committed."
   [?request <- ::CommitRequest (= ?input Input)]
   [::common/Response (= ?request Request)]
   [?input-value <- ::InputValue (= ?value value) (= ?input Request)]
   =>
   (rules/insert! ::InputResponse ?input-value)])

;;; Queries
(defqueries request-queries
  [::commit-request [:?input] [?request <- ::CommitRequest (= ?input Input)]]
  [::value-request [:?input] [?request <- ::ValueRequest (= ?input Input)]]
  )

(defqueries view-queries
  [::value [:?input] [::InputValue (= ?input Request) (= ?value value) ]]
  [::input [:?id] [?input <- ::Input (= ?id id)]])

;;; View

;;; rum mixin to pass the initial value passed to the component to
;;; the rulebase, setting the InputValue for the associated Input.
(def init-text-input
  {:will-mount
   (fn [{[session input initial-value] :rum/args :as state}]
     (let [value-request (common/query-one :?request session ::value-request :?input input)]
       (if value-request (common/respond-to value-request {::value initial-value}))
       state))})

(defn input-id
  "The logical ID used in facts could be anything, including a Clojure map.
   Use this function to make a HTML-friendly id from the hash of whatever data
   is contained in id."
  [id]
  (if (string? id) id (hasch/uuid id)))

(rum/defc text-input < init-text-input
          [session input initial-value & attrs]
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
