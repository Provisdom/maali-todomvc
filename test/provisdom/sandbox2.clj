(ns provisdom.sandbox2
  (:require [clojure.spec.alpha :as s]
            [provisdom.maali.rules :refer [defrules defqueries defsession] :as rules]
            [clara.rules.accumulators :as acc]))

(s/def ::foo int?)
(s/def ::bar string?)
(s/def ::fact (s/keys :req [::foo ::bar]))


(defrules roolz
          [::foo!
           [?fact <- ::fact]
           =>
           (println "RETRACT" ?fact)
           #_(rules/retract! ::fact ?fact)])

(defqueries qs
  [::fact [] [?fact <- ::fact]]
  [::foo [:?foo :?bar] [?fact <- ::fact (> ?foo foo) (= ?bar bar)]])

(defsession session [provisdom.sandbox2/roolz provisdom.sandbox2/qs])



(println (rules/query (-> session
                          (rules/insert ::fact {::foo 1 ::bar "1"})
                          (rules/fire-rules))
                      ::foo ))