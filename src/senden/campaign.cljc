(ns senden.campaign
  "Marketing campaign model (pure). Campaign{:id :name :channel :status :start
  :end :budget :metrics}. Status: :draft → :scheduled → :running → :paused →
  :completed | :cancelled. Projects onto chobo.ledger as lane :marketing."
  (:require [chobo.ledger :as ledger]))

(defrecord Campaign [id name channel status start end budget metrics])

(def statuses #{:draft :scheduled :running :paused :completed :cancelled})
(def transitions
  {:draft     #{:scheduled :cancelled}
   :scheduled #{:running :cancelled}
   :running   #{:paused :completed :cancelled}
   :paused    #{:running :completed :cancelled}
   :completed #{}
   :cancelled #{}})

(defn campaign [m] (merge {:status :draft :metrics {}} m))

(defn can-transition? [from to] (contains? (get transitions from #{}) to))
(defn transition [c to] (when (can-transition? (:status c :draft) to) (assoc c :status to)))

(defn schedule [c] (transition c :scheduled))
(defn start-campaign [c] (transition c :running))
(defn pause [c] (transition c :paused))
(defn complete [c] (transition c :completed))
(defn cancel [c] (transition c :cancelled))

(defn spend
  "Add `amount` to the campaign's :spend metric (cumulative ad spend)."
  [c amount]
  (update-in c [:metrics :spend] (fnil + 0) (max 0 amount)))

(defn record-conversion
  "Increment the campaign's :conversions metric by n (default 1)."
  ([c] (record-conversion c 1))
  ([c n]
   (update-in c [:metrics :conversions] (fnil + 0) (max 0 n))))

(defn cpa
  "Cost per acquisition = spend / conversions (nil if no conversions)."
  [c]
  (let [spend (get-in c [:metrics :spend] 0)
        conv (get-in c [:metrics :conversions] 0)]
    (when (pos? conv) (/ spend conv))))

(defn marketing-activity
  "Project a campaign event onto chobo.ledger as a :marketing activity."
  [c opts]
  (ledger/activity
   (merge {:lane :marketing :kind :campaign
           :title (:name c) :state (:status c :draft)
           :props {:campaign-id (:id c) :channel (:channel c)
                   :spend (get-in c [:metrics :spend] 0)
                   :conversions (get-in c [:metrics :conversions] 0)}}
          opts)))
