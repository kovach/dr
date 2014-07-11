(ns dr.env)

(defn insert-one [dict val]
  (let [ref (:time dict)]
    (assoc-in
      (assoc dict
             :time (inc ref))
      [:env ref] val)))

(defn insert [dict & vals]
  (reduce insert-one dict vals))

(def empty {:env {} :time 0})

(defn refs [dict]
  (range 0 (:time dict)))

(defn look [val dict]
  (first (filter
           (fn [key] (= val (get-in dict [:env key])))
           (refs dict))))
