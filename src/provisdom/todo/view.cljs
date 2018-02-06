(ns provisdom.todo.view
  (:require [rum.core :as rum]
            [provisdom.todo.common :as common]
            [provisdom.todo.rules :as todo]
            [provisdom.todo.text-input :as input]
            [provisdom.maali.rules :as rules]))

;;; View components
(rum/defc stats [session]
  (let [visibility-request (common/query-one :?request session ::todo/visibility-request)
        retract-complete-request (common/query-one :?request session ::todo/retract-complete-request)
        active-count (common/query-one :?count session ::todo/active-count)
        completed-count (common/query-one :?count session ::todo/completed-count)
        visibilities (or (::todo/visibilities visibility-request) #{})
        props-for (fn [name]
                    (cond->
                      {:class (cond
                                (= name (::todo/visibility visibility-request)) "selected"
                                (not (visibilities name)) "inactive")}
                      (visibilities name) (assoc :on-click #(common/respond-to visibility-request {::todo/visibility name}))))]
    [:div
     [:span#todo-count
      [:strong active-count] " " (case active-count 1 "item" "items") " left"]
     [:ul#filters
      [:li [:a (props-for :all) "All"]]
      [:li [:a (props-for :active) "Active"]]
      [:li [:a (props-for :completed) "Completed"]]]
     (when retract-complete-request
       [:button#clear-completed {:on-click #(common/respond-to retract-complete-request)}
        "Clear completed " completed-count])]))

(rum/defc item [session todo]
  (let [{::todo/keys [id done title]} todo
        edit-request (common/query-one :?request session ::todo/edit-request :?todo todo)
        title-input (common/query-one :?request session ::todo/input :?id todo)
        done-request (common/query-one :?request session ::todo/update-done-request :?todo todo)
        retract-request (common/query-one :?request session ::todo/retract-todo-request :?todo todo)]
    [:li {:class (str (if done "completed ")
                      (if title-input "editing"))}
     [:div.view
      ;;; Conditionally render controls only if the associated request exists
      (when done-request
        [:input.toggle {:type      "checkbox" :checked done
                        :on-change #(common/respond-to done-request {::todo/done (not done)})}])
      [:label (when edit-request {:on-double-click #(common/respond-to edit-request)}) title]
      (when retract-request
        [:button.destroy {:on-click #(common/respond-to retract-request)}])]
     (when title-input
       (input/text-input session title-input title :class "edit" :auto-focus true))]))

(rum/defc header [session]
  [:header#header
   [:h1 "todos"]
   (let [new-todo-input (common/query-one :?request session ::todo/input :?id "new-todo")]
     (when new-todo-input
       (input/text-input session new-todo-input "" :placeholder "What needs to be done?")))])

(rum/defc complete-all [session]
  (let [complete-all-request (common/query-one :?request session ::todo/complete-all-request)]
    (when complete-all-request
      (let [complete-all (::todo/done complete-all-request)]
        [:div
         [:input#toggle-all {:type      "checkbox" :checked (not complete-all)
                             :on-change #(common/respond-to complete-all-request {::todo/done complete-all})}]
         [:label {:for "toggle-all"} "Mark all as complete"]]))))

(rum/defc app [session]
  (let [todos (common/query-many :?todo session ::todo/visible-todos)]
    [:div
     [:section#todoapp
      (header session)
      [:div
       [:section#main
        (complete-all session)
        [:ul#todo-list
         (for [todo (sort-by ::todo/created-at todos)]
           (-> (item session todo)
               (rum/with-key (::todo/id todo))))]]
       [:footer#footer
        (stats session)]]]
     [:footer#info
      [:p "Double-click to edit a todo"]]]))

(defn ^:export run [session]
  (rum/mount (app session) (js/document.getElementById "app")))
