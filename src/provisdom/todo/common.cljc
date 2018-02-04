(ns provisdom.todo.common
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sg]
            [clojure.test.check.generators]
            [provisdom.maali.rules #?(:clj :refer :cljs :refer-macros) [defrules defqueries defsession def-derive] :as rules]
            [clojure.pprint :refer [pprint]]))

(s/def ::Request (s/keys))
(s/def ::Response (s/keys :req [::Request]))
(def-derive ::Cancellation ::Response)
;;; TODO - add predicate that ensures request conforms to spec?
(s/def ::response-fn (s/with-gen fn? #(sg/return (fn [_ _]))))
(s/def ::ResponseFunction (s/keys :req [::response-fn]))
(def-derive ::RequestWithResponseFn ::Request (s/keys :req [::response-fn]))

(s/def ::time nat-int?)
(defn now [] #?(:clj (System/currentTimeMillis) :cljs (.getTime (js/Date.))))

;;; Used to fill in the ::specs/response-fn field in requests. The code which responds
;;; to a request can use ::specs/response-fn to provide the response.
(defn response
  [response-fn spec]
  (fn [response]
    (condp = (rules/spec-type response)
      ::Cancellation
      (response-fn ::Cancellation response)

      (if (s/valid? spec response)
        (response-fn spec response)
        (let [explanation (s/explain-data spec response)]
          #?(:cljs (enable-console-print!))
          (pprint explanation)
          (throw (ex-info (str "Invalid response - must conform to spec " spec)
                          {:response response :spec spec :explanation explanation})))))))

;;; Convenience function to respond to a request
(defn respond-to
  ([request] (respond-to request {}))
  ([request response]
   (let [response-fn (::response-fn request)]
     (response-fn (merge {::Request request} response)))))

(defn cancel-request
  [request]
  (let [response-fn (::response-fn request)]
    (response-fn (rules/spec-type {::Request request} ::Cancellation))))

;;; Some query boilerplate functions
(defn query-many
  [map-fn session query & args]
  (mapv map-fn (apply rules/query session query args)))

(defn query-one
  [map-fn session query & args]
  (-> (apply rules/query session query args) first map-fn))

(defrules rules
  [::cancel-request!
   [?request <- ::Request]
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