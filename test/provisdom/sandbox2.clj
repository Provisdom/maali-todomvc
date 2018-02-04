(ns provisdom.sandbox2
  (:require [clojure.spec.alpha :as s]
            [provisdom.maali.rules :refer [defrules defqueries defsession] :as rules]))

(s/def ::foo int?)
(s/def ::bar string?)
(s/def ::fact (s/keys :req [::foo]))
(s/def ::child-fact ::fact)
(derive ::child-fact ::fact)
(s/def ::grandchild-fact (s/merge ::child-fact (s/keys :req [::bar])))
(derive ::grandchild-fact ::child-fact)

(defrules roolz
          [::foo!
           [?fact <- ::fact]
           =>
           (println "RETRACT" ?fact)
           (rules/retract! ::fact ?fact)])

(defqueries qs
  [::fact [] [?fact <- ::fact]])

(defsession session [provisdom.sandbox2/roolz provisdom.sandbox2/qs])



(println (rules/query (-> session
                          (rules/insert ::grandchild-fact {::foo 1 ::bar "1"})
                          (rules/fire-rules))
                      ::fact))