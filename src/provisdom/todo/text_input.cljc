(ns provisdom.todo.text-input
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sg]
            [clojure.test.check.generators]
            [rum.core :as rum]
            [provisdom.maali.rules #?(:clj :refer :cljs :refer-macros) [defrules defqueries defsession def-derive] :as rules]
            [provisdom.todo.common :as common]))

(s/def ::id uuid?)
(s/def ::value string?)
(s/def ::value-handler-fn (s/with-gen fn? #(sg/return (fn [_]))))
(s/def ::Input (s/keys :req [::id ::value ::value-handler-fn]))

(def-derive ::NewInputRequest ::common/Request)
(def-derive ::NewInputResponse ::common/Response (s/merge ::common/Response (s/keys :req [::Input])))

(def-derive ::InputRequest ::common/Request (s/merge ::common/Request (s/keys :req [::Input])))

(def-derive ::ClearRequest ::InputRequest)
(def-derive ::CommitRequest ::InputRequest)

(def-derive ::ValueRequest ::InputRequest)
(def-derive ::ValueResponse ::InputResponse (s/merge ::InputResponse (s/keys :req [::value])))

(def request->response
  {::NewInputRequest ::NewInputResponse
   ::ClearRequest ::common/Response
   ::CommitRequest ::common/Response
   ::ValueRequest ::ValueResponse})

(defrules rules
  [::new-input-request
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert-unconditional! ::NewInputRequest {::response-fn (partial common/response ?response-fn ::NewInputResponse)})]

  [::new-input-response!
   [?request <- ::NewInputRequest]
   [::NewInputResponse (= ?request Request) (= ?input Input)]
   =>
   (rules/insert-unconditional! ::Input ?input)]

  [::input!
   [?input <- ::Input]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::ValueRequest {::Input ?input ::response-fn (partial common/response ?response-fn ::ValueResponse)})
   (rules/insert! ::ClearRequest {::Input ?input ::response-fn (partial common/response ?response-fn ::common/Response)})
   (rules/insert! ::CommitRequest {::Input ?input ::response-fn (partial common/response ?response-fn ::common/Response)})]

  [::value-response!
   [?request <- ::ValueRequest (= ?input Input)]
   [::ValueResponse (= ?request Request) (= ?value value)]
   =>
   (rules/upsert! ::Input ?input assoc ::value ?value)]

  [::key-response!
   [?request <- ::ClearRequest (= ?input Input)]
   [::common/Response (= ?request Request)]
   =>
   (upsert! ::Input ?input assoc ::value "")]

  [::key-response!
   [?request <- ::CommitRequest (= ?input Input)]
   [::common/Response (= ?request Request)]
   [?input <- ::Input (= ?value value) (= ?value-handler-fn value-handler-fn)]
   [:test (not= "" ?value)]
   =>
   (upsert! ::Input ?input assoc ::value "")
   (?value-handler-fn ?value)])

(defqueries queries
  [::new-input-request [] [?request <- ::NewInputRequest]]
  [::clear-request [:?input] [?request <- ::ClearRequest (= ?input Input)]]
  [::commit-request [:?input] [?request <- ::CommitRequest (= ?input Input)]]
  [::value-request [:?input] [?request <- ::ValueRequest (= ?input Input)]]
  [::value [:?input] [?input <- ::Input (= ?value value)]])

(defn new-input
  ([value-handler-fn] (new-input value-handler-fn ""))
  ([value-handler-fn value]
   {::id (random-uuid) ::value value ::value-handler-fn value-handler-fn}))

(def init-input
  {:will-mount
   (fn [{[session initial-value value-handler-fn] :rum/args :as state}]
     (let [new-input-request (common/query-one :?request session ::new-input-request)
           input (new-input value-handler-fn initial-value)]
       (common/respond-to new-input-request {::Input input})
       (assoc state ::Input input)))})

(rum/defcs text-input < init-input
  [state session initial-value value-handler-fn & attrs]
  (let [attrs-map (into {} (partition 2 attrs))
        input (::Input state)
        id (::id input)
        clear-request (common/query-one :?request session ::clear-request :?input input)
        commit-request (common/query-one :?request session ::commit-request :?input input)
        value-request (common/query-one :?request session ::value-request :?input input)
        value (common/query-one :?value session ::value :?input id)]
    [:input (merge attrs-map
                   {:type        test
                    :value       value
                    :id          id
                    :on-change   #(common/respond-to value-request {::value %})
                    :on-blur     #(when commit-request (common/respond-to commit-request))
                    :on-key-down #(let [key-code (.-keyCode %)]
                                    (cond
                                      (and clear-request (= 27 key-code)) (common/respond-to clear-request)
                                      (and commit-request (= 13 key-code) (common/respond-to commit-request))))})]))