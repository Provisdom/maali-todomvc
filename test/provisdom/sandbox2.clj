(ns provisdom.sandbox2
  (:require [clojure.spec.alpha :as s]
            [provisdom.maali.rules :refer [defrules defqueries defsession] :as rules]
            [clara.rules.accumulators :as acc]
            [clara.rules.engine :as eng]
            [clojure.pprint :refer [pprint]]
            [clara.rules.memory :as mem]))

(defn query
  [session query & args]
  (let [{:keys [memory rulebase]} (eng/components session)
        query-node (get-in rulebase [:query-nodes query])
        tokens (mem/get-tokens-all memory query-node)
        args-map (apply hash-map args)
        matching-tokens (filter (fn [%] (= args-map (select-keys % (keys args-map)))) (map :bindings tokens))]

    (pprint tokens)
    (pprint matching-tokens)))

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
  [::foo [:?foo :?bar] [?fact <- ::fact (= ?foo foo) (= ?bar bar)]])

(defsession session [provisdom.sandbox2/roolz provisdom.sandbox2/qs])



(query (-> session
           (rules/insert ::fact {::foo 1 ::bar "1"})
           (rules/insert ::fact {::foo 1 ::bar "2"})
           (rules/insert ::fact {::foo 100 ::bar "3"})
           (rules/fire-rules))
       ::foo  )