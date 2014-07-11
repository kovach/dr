(ns dr.logic
  (:refer-clojure :exclude  [==])
  (:require [clojure.core.logic.fd :as fd])
  (:use clojure.core.logic)
  )

(comment
  Overview:
  This module is for processing documents specified by layout relations.
  The document consists of nodes, which are referred to by the relations.
  An implicit hierarchy is constructed based on the order in which
  relations are parsed. Ultimately, a layout (with exact coordinates) is
  computed with a logic program.)

(def str-sym (fn [s] (fn [r] (symbol (str r s)))) )
(def get-c (str-sym "-column"))
(def get-w (str-sym "-width"))
(def get-r (str-sym "-row"))
(def get-h (str-sym "-height"))

(defn process-relation [rel env]
  (let [[head r1 r2] rel]
    (case head
      ; r1 r2 : Obj
      left (if (contains? env r1)
             (add `(left ~r1 ~r2))
             (comp
               (add-vars j)
               )
             )

      right

      ; r2 = nil
      smaller

      ; r1 r2 : logic-var
      eq
      nil
      )
    )
  )

(defn compile-op [op]
  (let [[head r1 r2] op]
    (case head
      left `(fd/eq
              (~'< (~'+ ~(get-c r1)
                        ~(get-w r1))
                   ~(get-c r2)))
      right `(fd/eq
              (~'< (~'+ ~(get-c r2)
                        ~(get-w r2))
                   ~(get-c r1)))
      eq `(== ~r1 ~r2)
;      < `(fd/< ~(get var-map r1) ~(get var-map r2))
;      > `(fd/> ~(get var-map r1) ~(get var-map r2))
      nil)))

(defn obj-vars [sym]
  [(get-c sym)
   (get-w sym)
   (get-r sym)
   (get-h sym) ])


(defn compile-layout [ids ops]
  ; Require distinct identifiers
  (if (apply distinct? ids)
    (let
      [vars (flatten (map obj-vars ids))
       constraints (map (fn [op] (compile-op op)) ops)

       code `(fresh [~@vars]
               (fd/in ~@vars (fd/interval 0 3))
               ~@constraints
               (== ~'q [~@vars])
               )
       result `(run* [~'q] ~code)
       ]
      {:code code
       :result 
         (map (fn [outcome] (zipmap vars outcome))
              (take 1 (eval result))
              )})
    (str "error: ids not distinct "  ids)))

(:result
  (compile-layout
    '(x y)
    '((eq x-column 0)
      (eq x-row 0)
      (left x y)
      (eq x-width 2)
      (eq y-width 1)
      )))

(defn prefixes [list]
  (when (seq list)
    (cons list (prefixes (drop-last list)))) )
