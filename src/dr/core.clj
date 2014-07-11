(ns dr.core
  (:require [dr.util :as u])
  (:require [dr.env :as env])
  )

(def modes
  [:read :focus])

(def context0
  {:dict env/empty
   :parse env/empty
   :focus nil
   :mode :read
   })

(defn set-focus [ref context]
  (assoc-in context [:focus] ref))

(defn read-word [word context]
  (if-let [ref (env/look word (:dict context))]
    (set-focus ref context)
    "word"))

(update-in {:a [1]} [:a] conj 2)
