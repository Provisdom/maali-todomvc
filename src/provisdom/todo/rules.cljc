(ns provisdom.todo.rules
  (:require [clojure.spec.alpha :as s]
            [provisdom.todo.common :as common]
            [provisdom.maali.rules #?(:clj :refer :cljs :refer-macros) [defrules defqueries defsession def-derive] :as rules]
            [clara.rules.accumulators :as acc]
            [provisdom.todo.text-input :as input]))


;;; Fact specs. Use convention that specs for fact "types" are camel-cased.
(s/def ::id ::common/id)
(s/def ::title string?)
(s/def ::done boolean?)
(s/def ::Todo (s/keys :req [::id ::title ::done]))

(s/def ::visibility #{:all :active :completed})
(s/def ::Visibility (s/keys :req [::visibility]))

(def-derive ::UpdateTodoRequest ::common/Request (s/keys :req [::Todo]))
(def-derive ::UpdateTitleResponse ::common/Response (s/keys :req [::title]))
(def-derive ::UpdateDoneRequest ::UpdateTodoRequest)
(def-derive ::UpdateDoneResponse ::common/Response (s/keys :req [::done]))
(def-derive ::RetractTodoRequest ::UpdateTodoRequest)
(def-derive ::CompleteAllRequest ::common/Request (s/keys :req [::done]))
(def-derive ::RetractCompletedRequest ::common/Request)
(s/def ::visibilities (s/coll-of ::visibility :kind set?))
(def-derive ::VisibilityRequest ::common/Request (s/keys :req [::visibilities]))
(def-derive ::VisibilityResponse ::common/Response (s/keys :req [::visibility]))
(def-derive ::EditRequest ::common/Request (s/keys :req [::Todo]))

;;; Convenience function to create new ::Todo facts

(defn new-todo
  [title]
  {::id (common/next-id) ::title title ::done false})

;;; Rules
(defrules rules
  [::initialize-visibility!
   "Initialize visibility and the request for a new todo. Both are singletons
    so insert unconditionally here."
   [:not [:exists [::Visibility]]]
   =>
   (rules/insert-unconditional! ::Visibility {::visibility :all})]

  [::update-request!
   "When visibility changes or a new todo is inserted, conditionally insert
    requests to update todos."
   [?todo <- ::Todo]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::EditRequest (common/request {::Todo ?todo} ::common/Response ?response-fn))
   (rules/insert! ::UpdateDoneRequest (common/request {::Todo ?todo} ::UpdateDoneResponse ?response-fn))
   (rules/insert! ::RetractTodoRequest (common/request {::Todo ?todo} ::common/Response ?response-fn))]

  [::update-done-response!
   "Handle response to a one update request."
   [?request <- ::UpdateDoneRequest (= ?todo Todo)]
   [::UpdateDoneResponse (= ?request Request) (= ?done done)]
   =>
   (rules/upsert! ::Todo ?todo assoc ::done ?done)]

  [::retract-todo-response!
   "Handle response to retract todo request."
   [?request <- ::RetractTodoRequest (= ?todo Todo)]
   [::common/Response (= ?request Request) #_(= ::RetractTodoRequest (rules/spec-type ?request))]
   =>
   (rules/retract! ::Todo ?todo)]

  [::edit-todo-response!
   "Make an Input when the user chooses to edit a todo."
   [?request <- ::EditRequest (= ?todo Todo)]
   [::common/Response (= ?request Request)]
   [?todo <- ::Todo (= ?title title)]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::input/InputRequest (input/create ?todo ?title ?response-fn))]

  [::comittable-todo!
   "If the value of the InputValueRequest is not empty, it is legal to commit the value."
   [?input-value <- ::input/InputValueRequest (= ?input Request) (not-empty value)]
   [?input <- ::input/InputRequest (= ?id correlation-id)]
   [:or
    [?id <- ::Todo]
    [:test (= "new-todo" ?id)]]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::input/CommitRequest (common/request {::input/InputRequest ?input} ::common/Response ?response-fn))]

  [::update-title-response!
   "Update the title when the edit value is committed."
   [?request <- ::input/InputRequest (= ?todo correlation-id)]
   [::input/InputResponse (= ?request Request) (= ?title value)]
   [?todo <- ::Todo]
   =>
   (rules/upsert! ::Todo ?todo assoc ::title ?title)]

  [::new-todo-request!
   "Always want to allow a new todo to be entered, so if the input does not
    exist, insert it."
   [:not [::input/InputRequest (= "new-todo" correlation-id)]]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert-unconditional! ::input/InputRequest (input/create "new-todo" "" ?response-fn))]

  [::new-todo-response!
   "Handle a new todo."
   [?request <- ::input/InputRequest (= "new-todo" correlation-id)]
   [?input-value <- ::input/InputResponse (= ?request Request) (= ?title value)]
   =>
   (rules/insert-unconditional! ::Todo (new-todo ?title))
   ;;; Input state is initialized by inserting a new input, so just retract the old one
   ;;; and let the ::new-todo-request! rule and it's logical consequents handle the rest.
   (rules/retract! ::input/InputRequest ?request)]

  [::complete-all-request!
   "Toggles the done attribute of all todos. If all todos are not done,
    then the request implies we set them all to done. If all todos are
    done, then the request means we will set them all to not done."
   [?todos <- (acc/grouping-by ::done) :from [::Todo]]
   [:test (> (count ?todos) 0)]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::CompleteAllRequest (common/request {::done (not= 0 (count (?todos false)))} ::common/Response ?response-fn))]

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
   (rules/insert! ::RetractCompletedRequest (common/request ::common/Response ?response-fn))]

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
     (rules/insert! ::VisibilityRequest (common/request {::visibility   ?visibility
                                                         ::visibilities visibilities}
                                                        ::VisibilityResponse ?response-fn)))]

  [::visibility-response!
   "Handle response to visibility request."
   [?request <- ::VisibilityRequest (= ?visibilities visibilities)]
   [::VisibilityResponse (= ?request Request) (= ?visibility visibility) (contains? ?visibilities visibility)]
   [?Visibility <- ::Visibility]
   =>
   (rules/upsert! ::Visibility ?Visibility assoc ::visibility ?visibility)])

;;; Queries
(defqueries view-queries
  [::todos [] [?todo <- ::Todo]]
  [::visible-todos []
   [::Visibility (= ?visibility visibility)]
   [?todo <- ::Todo (condp = ?visibility
                      :all true
                      :active (= false done)
                      :completed (= true done))]]
  [::active-count [] [?count <- (acc/count) :from [::Todo (= false done)]]]
  [::completed-count [] [?count <- (acc/count) :from [::Todo (= true done)]]])

(defqueries request-queries
  [::input [:?id] [?request <- ::input/InputRequest (= ?id correlation-id)]]
  [::edit-request [:?todo] [?request <- ::EditRequest (= ?todo Todo)]]
  [::update-done-request [:?todo] [?request <- ::UpdateDoneRequest (= ?todo Todo)]]
  [::retract-todo-request [:?todo] [?request <- ::RetractTodoRequest (= ?todo Todo)]]
  [::complete-all-request [] [?request <- ::CompleteAllRequest]]
  [::retract-complete-request [] [?request <- ::RetractCompletedRequest]]
  [::visibility-request [] [?request <- ::VisibilityRequest]])

(defsession init-session [common/rules
                          input/init-session
                          rules
                          view-queries
                          request-queries])
