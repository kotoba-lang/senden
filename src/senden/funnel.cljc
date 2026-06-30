(ns senden.funnel
  "Marketing funnel stages + conversion (pure). Stages:
  :awareness → :interest → :consideration → :purchase → :retention.
  A Funnel is {stage → count}. Conversion rate = next/prev."
  (:refer-clojure :exclude [empty]))

(def stages [:awareness :interest :consideration :purchase :retention])

(defn stage-index [stage]
  (loop [i 0 [s & r] stages]
    (cond (nil? s) nil (= s stage) i :else (recur (inc i) r))))

(defrecord Funnel [counts])

(defn funnel [] (->Funnel (zipmap stages (repeat 0))))

(defn set-stage [f stage n] (assoc-in f [:counts stage] (max 0 n)))
(defn add-to-stage [f stage n] (update-in f [:counts stage] (fnil + 0) (max 0 n)))

(defn stage-count [f stage] (get-in f [:counts stage] 0))

(defn conversion-rate
  "Rate from stage → next stage (next/prev). 0 if prev is 0 or no next stage."
  [f stage]
  (let [i (stage-index stage)
        nxt (when i (nth stages (inc i) nil))]
    (if-not nxt
      0
      (let [prev-c (stage-count f stage)
            next-c (stage-count f nxt)]
        (if (pos? prev-c) (/ next-c prev-c) 0)))))

(defn overall-conversion-rate
  "End-to-end: retention / awareness."
  [f]
  (let [a (stage-count f :awareness)
        r (stage-count f :retention)]
    (if (pos? a) (/ r a) 0)))
