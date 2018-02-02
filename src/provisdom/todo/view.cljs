;;; Derived from precept todomvc example: https://github.com/CoNarrative/precept/tree/master/examples/todomvc

(ns provisdom.todo.view
  (:require [rum.core :as rum]
            [provisdom.todo.common :as common]
            [provisdom.todo.rules :as todo]))

;;; View components
(def todo-input-title
  {:will-mount
   (fn [{[title] :rum/args :as state}]
     (let [local-state (atom title)
           component (:rum/react-component state)]
       (add-watch local-state :todo-input-title
                  (fn [_ _ _ _]
                    (rum/request-render component)))
       (assoc state :todo-input-title local-state)))})

(rum/defcs todo-input < todo-input-title
  [state title {:keys [on-stop id class placeholder on-save]}]
  (let [val (:todo-input-title state)
        stop #(do (reset! val "")
                  (if on-stop (on-stop)))
        save (fn [on-save]
               (let [v (-> @val str clojure.string/trim)]
                 (if-not (empty? v) (on-save v))
                 (stop)))]
    [:input {:auto-focus  (boolean (not-empty title))
             :type        "text"
             :value       @val
             :id          id
             :class       class
             :placeholder placeholder
             :on-blur     #(save on-save)
             :on-change   #(reset! val (-> % .-target .-value))
             :on-key-down #(case (.-keyCode %)
                             13 (save on-save)
                             27 (stop)
                             nil)}]))

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

(rum/defcs item < (rum/local false :edit) [state session todo]
  (let [edit (:edit state)
        {::todo/keys [id done title]} todo
        title-request (common/query-one :?request session ::todo/update-title-request :?todo todo)
        done-request (common/query-one :?request session ::todo/update-done-request :?todo todo)
        retract-request (common/query-one :?request session ::todo/retract-todo-request :?todo todo)]
    [:li {:class (str (if done "completed ")
                      (if @edit "editing"))}
     [:div.view
      ;;; Conditionally render controls only if the associated request exists
      (when done-request
        [:input.toggle {:type      "checkbox" :checked done
                        :on-change #(common/respond-to done-request {::todo/done (not done)})}])
      [:label (when title-request {:on-double-click #(reset! edit true)}) title]
      (when retract-request
        [:button.destroy {:on-click #(common/respond-to retract-request)}])]
     (when @edit
       (todo-input title {:id      id
                          :class   "edit"
                          :on-save (fn [title] (common/respond-to title-request {::todo/title title}))
                          :on-stop #(reset! edit false)}))]))

(rum/defc header [session]
  [:header#header
   [:h1 "todos"]
   (let [new-todo-request (common/query-one :?request session ::todo/new-todo-request)]
     (when new-todo-request
       (todo-input "" {:id          "new-todo"
                       :placeholder "What needs to be done?"
                       :on-save     (fn [title] (common/respond-to new-todo-request {::todo/Todo (todo/new-todo title)}))})))])

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
  #_(rum/unmount (js/document.getElementById "app"))
  (rum/mount (app session) (js/document.getElementById "app")))
