(ns provisdom.todo.text-input
  (:require [clojure.spec.alpha :as s]
            [rum.core :as rum]
            [provisdom.maali.rules #?(:clj :refer :cljs :refer-macros) [defrules defqueries defsession def-derive] :as rules]
            [provisdom.todo.common :as common]))

(s/def ::id any?)
(s/def ::value string?)
(s/def ::persistent? boolean?)
(s/def ::committed? boolean?)
(def-derive ::Input ::common/Request (s/merge ::common/Request (s/keys :req [::id ::persistent?])))
(def-derive ::InputValue ::common/Response (s/merge ::common/Response (s/keys :req [::value ::committed?])))

(def-derive ::InputRequest ::common/Request (s/merge ::common/RequestWithResponseFn (s/keys :req [::Input])))
(def-derive ::CommitRequest ::InputRequest)
(def-derive ::ValueRequest ::InputRequest)
(def-derive ::ValueResponse ::common/Response (s/merge ::common/Response (s/keys :req [::value])))
(def-derive ::CancelRequest ::InputRequest)

(def request->response
  {::CommitRequest ::common/Response
   ::ValueRequest ::ValueResponse
   ::CancelRequest ::common/Response})

(defn new-input
  ([id] (new-input id false))
  ([id persistent?]
   {::id id ::persistent? persistent? :token (random-uuid)}))

;;; TODO - decouple value state from input
(defrules rules
  [::input!
   [?input <- ::Input]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert-unconditional! ::InputValue {::common/Request ?input ::value "" ::committed? false})
   (rules/insert! ::CancelRequest {::Input ?input ::common/response-fn (partial common/response ?response-fn ::common/Response)})]

  [::comittable-input!
   [?input <- ::Input]
   [::InputValue (= ?input Request) (not-empty value)]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::CommitRequest {::Input ?input ::common/response-fn (partial common/response ?response-fn ::common/Response)})]

  [::value-request!
   [?input-value <- ::InputValue (= ?input Request)]
   [?input <- ::Input]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::ValueRequest {::Input ?input ::common/response-fn (partial common/response ?response-fn ::ValueResponse)})]

  [::value-response!
   [?request <- ::ValueRequest (= ?input Input)]
   [::ValueResponse (= ?request Request) (= ?value value)]
   [?input-value <- ::InputValue (= ?input Request)]
   =>
   (rules/upsert! ::InputValue ?input-value assoc ::value ?value)]

  [::commit-response!
   [?request <- ::CommitRequest (= ?input Input)]
   [::common/Response (= ?request Request)]
   [?input-value <- ::InputValue (= ?value value) (= ?input Request)]
   =>
   (rules/upsert! ::InputValue ?input-value assoc ::committed? true)]

  [::cancel-response!
   [?request <- ::CancelRequest (= ?input Input)]
   [::common/Response (= ?request Request)]
   [?input <- ::Input (= ?id id) (= ?persistent? persistent?)]
   [?input-value <- ::InputValue (= ?input Request)]
   =>
   (rules/retract! ::Input ?input)
   (when ?persistent?
     (rules/insert! ::Input (new-input ?id true)))])

(defqueries queries
  [::cancel-request [:?input] [?request <- ::CancelRequest (= ?input Input)]]
  [::commit-request [:?input] [?request <- ::CommitRequest (= ?input Input)]]
  [::value-request [:?input] [?request <- ::ValueRequest (= ?input Input)]]
  [::value [:?input] [::InputValue (= ?input Request) (= ?value value) ]]
  [::input [:?id] [?input <- ::Input (= ?id id)]])

(def init-text-input
  {:will-mount
   (fn [{[session id initial-value] :rum/args :as state}]
     (let [value-request (common/query-one :?request session ::value-request :?id id)]
       (if value-request (common/respond-to value-request {::value initial-value}))
       state))})

(rum/defc text-input < init-text-input
          [session id initial-value & attrs]
          (let [attrs-map (into {} (map vec (partition 2 attrs)))
                input (common/query-one :?input session ::input :?id id)
                cancel-request (common/query-one :?request session ::cancel-request :?input input)
                commit-request (common/query-one :?request session ::commit-request :?input input)
                value-request (common/query-one :?request session ::value-request :?input input)
                value (common/query-one :?value session ::value :?input input)]
            (when value
              [:input (merge attrs-map
                             {:type        "text"
                              :value       value
                              :id          id
                              :on-change   #(common/respond-to value-request {::value (-> % .-target .-value)})
                              :on-blur     #(when commit-request (common/respond-to commit-request))
                              :on-key-down #(let [key-code (.-keyCode %)]
                                              (cond
                                                (and cancel-request (= 27 key-code)) (common/respond-to cancel-request)
                                                (and commit-request (= 13 key-code)) (common/respond-to commit-request)))})])))
