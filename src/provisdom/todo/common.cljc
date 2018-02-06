(ns provisdom.todo.common
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sg]
            [clojure.test.check.generators]
            [provisdom.maali.rules #?(:clj :refer :cljs :refer-macros) [defrules defqueries defsession def-derive] :as rules]
            [clojure.pprint :refer [pprint]]
            [hasch.core :as hasch]))

(s/def ::id uuid?)
(s/def ::Request (s/keys :req [::id]))
(s/def ::Response (s/keys :req [::Request]))
(def-derive ::Cancellable ::Request)
(def-derive ::Cancellation ::Response)
;;; TODO - add predicate that ensures request conforms to spec?
(s/def ::response-fn (s/with-gen fn? #(sg/return (fn [_ _]))))
(s/def ::ResponseFunction (s/keys :req [::response-fn]))

(s/def ::time nat-int?)
(defn now [] #?(:clj (System/currentTimeMillis) :cljs (.getTime (js/Date.))))

;;; Used to fill in the ::specs/response-fn field in requests.
(defn response
  "Given a response-fn and a spec, return a function that calls response-fn
   with a supplied response and the captured spec. This makes it convenient
   for consumers of requests to simply provide a response without having to
   map the request spec to the response spec. response-fn accepts a spec
   and a response map. Also handles special cases like cancellation by
   looking for specific spec types in the response metadata."
  [response-fn spec]
  (fn [response]
    (condp = (rules/spec-type response)
      ::Cancellation
      (response-fn ::Cancellation response)

      (if (s/valid? spec response)
        (response-fn spec response)
        (let [explanation (s/explain-data spec response)]
          ;;; Pretty-print the explanation here so it is readable in the browser
          #?(:cljs (enable-console-print!))
          (pprint explanation)
          (throw (ex-info (str "Invalid response - must conform to spec " spec)
                          {:response response :spec spec :explanation explanation})))))))

(defn response-fn
  ([x] (-> x meta ::response-fn))
  ([x f] (vary-meta x assoc ::response-fn f)))

(defn request
  ([] (request {}))
  ([req] (assoc req ::id (hasch/uuid)))
  ([resp-spec resp-fn] (request {} resp-spec resp-fn))
  ([req resp-spec resp-fn]
   (response-fn (request req) (response resp-fn resp-spec))))

;;; Convenience function to respond to a request
(defn respond-to
  ([request] (respond-to request {}))
  ([request response]
   ((response-fn request) (merge {::Request request} response))))

;;; Convenience function to cancel a request
(defn cancel-request
  [request]
  ((response-fn request) (rules/spec-type {::Request request} ::Cancellation)))

;;; Some query boilerplate functions
(defn query-many
  [map-fn session query & args]
  (mapv map-fn (apply rules/query session query args)))

(defn query-one
  [map-fn session query & args]
  (-> (apply rules/query session query args) first map-fn))

;;; Common rules for request/response logic.
(defrules rules
  [::cancel-request!
   "Cancellation is a special response that always causes the corresponding
    request to be retracted. Note that the ::retract-orphan-response! rule
    below will then cause the cancellation fact to also be retracted."
   [?request <- ::Cancellable]
   [::Cancellation (= ?request Request)]
   =>
   (rules/retract! (rules/spec-type ?request) ?request)]

  [::retract-orphan-response!
   "Responses are inserted unconditionally from outside the rule engine, so
    explicitly retract any responses without a corresponding request."
   [?response <- ::Response (= ?request Request)]
   [:not [?request <- ::Request]]
   =>
   (rules/retract! (rules/spec-type ?response) ?response)])

(def productions #{provisdom.todo.common/rules})