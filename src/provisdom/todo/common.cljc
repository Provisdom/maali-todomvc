(ns provisdom.todo.common
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sg]
            [clojure.test.check.generators]
            [provisdom.maali.rules #?(:clj :refer :cljs :refer-macros) [defrules defqueries defsession def-derive] :as rules]
            [clojure.pprint :refer [pprint]]))

(s/def ::Request (s/keys))
(s/def ::Response (s/keys :req [::Request]))
;;; TODO - add predicate that ensures request conforms to spec?
(s/def ::response-fn (s/with-gen fn? #(sg/return (fn [_ _]))))
(s/def ::ResponseFunction (s/keys :req [::response-fn]))
(def-derive ::RequestWithResponseFn ::Request (s/keys :req [::response-fn]))

;;; Used to fill in the ::specs/response-fn field in requests. The code which responds
;;; to a request can use ::specs/response-fn to provide the response.
(defn response
  [response-fn spec response]
  (if (s/valid? spec response)
    (response-fn spec response)
    (let [explanation (s/explain-data spec response)]
      (enable-console-print!)
      (pprint explanation)
      (throw (ex-info (str "Invalid response - must conform to spec " spec)
                      {:response response :spec spec :explanation explanation})))))

;;; Convenience function to respond to a request
(defn respond-to
  ([request] (respond-to request {}))
  ([request response]
   (let [response-fn (::response-fn request)]
     (response-fn (merge {::Request request} response)))))

;;; Some query boilerplate functions
(defn query-many
  [map-fn session query & args]
  (mapv map-fn (apply rules/query session query args)))

(defn query-one
  [map-fn session query & args]
  (-> (apply rules/query session query args) first map-fn))

(defrules rules
  [::retract-orphan-response!
   "Responses are inserted unconditionally from outside the rule engine, so
    explicitly retract any responses without a corresponding request."
   [?response <- ::Response (= ?request Request)]
   [:not [?request <- ::Request]]
   =>
   (rules/retract! ::Response ?response)])