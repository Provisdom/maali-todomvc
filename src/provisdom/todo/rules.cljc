(ns provisdom.todo.rules
  (:require [clojure.spec.alpha :as s]
            [provisdom.todo.common :as common]
            [provisdom.maali.rules #?(:clj :refer :cljs :refer-macros) [defrules defqueries defsession def-derive] :as rules]
            [clara.rules.accumulators :as acc]
            [provisdom.todo.text-input :as input]
            [net.cgrand.xforms :as xforms])
  #?(:clj (:import (java.util UUID))))

;;; Fact specs. Use convention that specs for fact "types" are camel-cased.
(s/def ::Anchor (s/keys :req [::common/time]))

(s/def ::id uuid?)
(s/def ::title string?)
(s/def ::done boolean?)
(s/def ::created-at ::common/time)
(s/def ::Todo (s/keys :req [::id ::title ::done]))

(s/def ::visibility #{:all :active :completed})
(s/def ::Visibility (s/keys :req [::visibility]))

(def-derive ::UpdateTodoRequest ::common/RequestWithResponseFn (s/keys :req [::Todo]))
(def-derive ::UpdateTitleRequest ::UpdateTodoRequest)
(def-derive ::UpdateTitleResponse ::common/Response (s/keys :req [::title]))
(def-derive ::UpdateDoneRequest ::UpdateTodoRequest)
(def-derive ::UpdateDoneResponse ::common/Response (s/keys :req [::done]))
(def-derive ::RetractTodoRequest ::UpdateTodoRequest)
(def-derive ::CompleteAllRequest ::common/RequestWithResponseFn (s/keys :req [::done]))
(def-derive ::RetractCompletedRequest ::common/RequestWithResponseFn)
(s/def ::visibilities (s/coll-of ::visibility :kind set?))
(def-derive ::VisibilityRequest ::common/RequestWithResponseFn (s/keys :req [::visibilities]))
(def-derive ::VisibilityResponse ::common/Response (s/keys :req [::visibility]))
(def-derive ::EditRequest ::common/RequestWithResponseFn (s/keys :req [::Todo]))


(def request->response
  {::EditRequest             ::common/Response
   ::UpdateTitleRequest      ::UpdateTitleResponse
   ::UpdateDoneRequest       ::UpdateDoneResponse
   ::RetractTodoRequest      ::common/Response
   ::CompleteAllRequest      ::common/Response
   ::RetractCompletedRequest ::common/Response
   ::VisibilityRequest       ::VisibilityResponse})

;;; Reducing function to produce the new session from a supplied response.
(defn handle-response
  [session [spec response]]
  (if session
    (-> session
        (rules/insert spec response)
        (rules/fire-rules))
    response))

;;; Transducer which takes in responses and provides reductions over the handle-response function,
;;; i.e. updated sessions.
(def handle-response-xf
  (comp
    (xforms/reductions handle-response nil)
    (drop 1)))                                              ;;; drop the initial nil value

;;; Convenience function to create new ::Todo facts
(defn new-todo
  ([title] (new-todo title (common/now)))
  ([title time]
   {::id #?(:clj (UUID/randomUUID) :cljs (random-uuid)) ::title title ::done false ::created-at time}))

;;; Rules
(defrules rules
  [::anchor!
   "Initialize visibility and the request for a new todo. Both are singletons
    so insert unconditionally here."
   [::Anchor (= ?time time)]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert-unconditional! ::Visibility {::visibility :all})]

  [::new-todo-request!
   [:not [::input/Input (= "new-todo" id)]]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert-unconditional! ::input/Input {::input/id "new-todo" ::common/response-fn (common/response ?response-fn ::input/InputResponse)})]

  [::new-todo-response!
   "Handle a new todo."
   [?request <- ::input/Input (= "new-todo" id)]
   [?input-value <- ::input/InputResponse (= ?request Request) (= ?title value)]
   =>
   (rules/insert-unconditional! ::Todo (new-todo ?title))
   (rules/retract! ::input/Input ?request)
   #_(rules/insert-unconditional! ::input/Input (input/new-input "new-todo" true))]

  [::update-request!
   "When visibility changes or a new todo is inserted, conditionally insert
    requests to update todos."
   [?todo <- ::Todo]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::EditRequest {::Todo ?todo ::common/response-fn (common/response ?response-fn ::common/Response)})
   (rules/insert! ::UpdateDoneRequest {::Todo ?todo ::common/response-fn (common/response ?response-fn ::UpdateDoneResponse)})
   (rules/insert! ::RetractTodoRequest {::Todo ?todo ::common/response-fn (common/response ?response-fn ::common/Response)})]

  [::edit-todo-response!
   [?request <- ::EditRequest (= ?todo Todo)]
   [::common/Response (= ?request Request)]
   [?todo <- ::Todo]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::input/Input {::input/id ?todo ::common/response-fn (common/response ?response-fn ::input/InputResponse)})]

  [::update-title-response!
   "Update the title when the edit value is committed."
   [?request <- ::input/Input (= ?todo id)]
   [::input/InputResponse (= ?request Request) (= ?title value)]
   [?todo <- ::Todo]
   =>
   (rules/upsert! ::Todo ?todo assoc ::title ?title)]

  [::update-done-response!
   "Handle response to a one update request."
   [?request <- ::UpdateDoneRequest (= ?todo Todo)]
   [::UpdateDoneResponse (= ?request Request) (= ?done done)]
   =>
   (rules/upsert! ::Todo ?todo assoc ::done ?done)]

  [::retract-todo-response!
   "Handle response to retract todo request."
   [?request <- ::RetractTodoRequest (= ?todo Todo)]
   [::common/Response (= ?request Request)]
   =>
   (rules/retract! ::Todo ?todo)]

  [::complete-all-request!
   "Toggles the done attribute of all todos. If all todos are not done,
    then the request implies we set them all to done. If all todos are
    done, then the request means we will set them all to not done."
   [?todos <- (acc/grouping-by ::done) :from [::Todo]]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::CompleteAllRequest {::done               (not= 0 (count (?todos false)))
                                        ::common/response-fn (common/response ?response-fn ::common/Response)})]

  [::complete-all-response!
   "Handle response to complete all request."
   [?request <- ::CompleteAllRequest (= ?done done)]
   [::common/Response (= ?request Request)]
   [?todos <- (acc/all) :from [::Todo (= (not ?done) done)]]
   =>
   (rules/upsert-seq! ::Todo ?todos update ::done not)]

  [::retract-completed-request!
   "Request to retract all todo's marked done."
   [?todos <- (acc/all) :from [::Todo (= true done)]]
   [:test (not-empty ?todos)]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::RetractCompletedRequest {::common/response-fn (common/response ?response-fn ::common/Response)})]

  [::retract-completed-response!
   "Handle response to retract completed request."
   [?request <- ::RetractCompletedRequest]
   [::common/Response (= ?request Request)]
   [?todos <- (acc/all) :from [::Todo (= true done)]]
   =>
   (apply rules/retract! ::Todo ?todos)]

  [::visibility-request!
   "Request to update the visibility. Includes the valid choices for
    visibility given the current set of todos, e.g. if no todos are
    marked done, don't include completed as a valid option."
   [::Visibility (= ?visibility visibility)]
   [?todos <- (acc/grouping-by ::done) :from [::Todo]]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (let [visibilities (cond-> #{:all}
                              (not= 0 (count (?todos true))) (conj :completed)
                              (not= 0 (count (?todos false))) (conj :active))]
     (rules/insert! ::VisibilityRequest {::visibility         ?visibility
                                         ::visibilities       visibilities
                                         ::common/response-fn (common/response ?response-fn ::VisibilityResponse)}))]

  [::visibility-response!
   "Handle response to visibility request."
   [?request <- ::VisibilityRequest (= ?visibilities visibilities)]
   [::VisibilityResponse (= ?request Request) (= ?visibility visibility) (contains? ?visibilities visibility)]
   [?Visibility <- ::Visibility]
   =>
   (rules/upsert! ::Visibility ?Visibility assoc ::visibility ?visibility)])

;;; Queries
(defqueries view-queries
  [::visible-todos []
   [::Visibility (= ?visibility visibility)]
   [?todo <- ::Todo (condp = ?visibility
                      :all true
                      :active (= false done)
                      :completed (= true done))]]
  [::active-count [] [?count <- (acc/count) :from [::Todo (= false done)]]]
  [::completed-count [] [?count <- (acc/count) :from [::Todo (= true done)]]])

(defqueries request-queries
  [::edit-request [:?todo] [?request <- ::EditRequest (= ?todo Todo)]]
  [::update-title-request [:?todo] [?request <- ::UpdateTitleRequest (= ?todo Todo)]]
  [::update-done-request [:?todo] [?request <- ::UpdateDoneRequest (= ?todo Todo)]]
  [::retract-todo-request [:?todo] [?request <- ::RetractTodoRequest (= ?todo Todo)]]
  [::complete-all-request [] [?request <- ::CompleteAllRequest]]
  [::retract-complete-request [] [?request <- ::RetractCompletedRequest]]
  [::visibility-request [] [?request <- ::VisibilityRequest]])

(defsession init-session [provisdom.todo.common/rules
                          provisdom.todo.text-input/rules
                          provisdom.todo.text-input/queries
                          provisdom.todo.rules/rules
                          provisdom.todo.rules/view-queries
                          provisdom.todo.rules/request-queries])

(def session (-> init-session
                 (rules/insert ::Anchor {::common/time (common/now)})
                 (rules/fire-rules)))
