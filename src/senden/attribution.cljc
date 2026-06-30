(ns senden.attribution
  "Attribution model (pure). A Touch is {:channel :campaign :timestamp :customer}.
  A conversion has a touch sequence. Attribution models: first-touch, last-touch,
  multi-touch (even), linear."
  (:refer-clojure :exclude [first last]))

(defrecord Touch [channel campaign timestamp customer])

(defn touch [m] (merge {} m))

(defn first-touch [touches] (clojure.core/first (sort-by :timestamp touches)))
(defn last-touch [touches] (clojure.core/last (sort-by :timestamp touches)))

(defn linear-attribution
  "Return {campaign → weight} where each touch in the conversion path gets
  1/N (even distribution)."
  [touches]
  (let [n (count touches)]
    (if (zero? n)
      {}
      (let [w (/ 1 n)]
        (reduce (fn [acc t] (update acc (:campaign t) (fnil + 0) w)) {} touches)))))

(defn multi-touch-credits
  "Return {campaign → credits} summing 1 credit per touch (for counting)."
  [touches]
  (reduce (fn [acc t] (update acc (:campaign t) (fnil + 0) 1)) {} touches))

(defn attribute
  "Attribution by model: :first, :last, :linear, :multi. Returns {campaign → weight/credit}."
  [model touches]
  (case model
    :first  (let [t (first-touch touches)] (if t {(:campaign t) 1.0} {}))
    :last   (let [t (last-touch touches)]  (if t {(:campaign t) 1.0} {}))
    :linear (linear-attribution touches)
    :multi  (multi-touch-credits touches)
    {}))
