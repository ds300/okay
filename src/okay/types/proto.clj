(ns okay.types.proto
  (:import clojure.lang.IFn))

(defprotocol TypeProtocol
  (validate [me value])
  (parse [me value])
  (get-default [me])
  (compose [me other])
  (finalize [me]))

(defrecord AbstractTypeProperty [])

(defn abstract-type-property [m]
  (merge (->AbstractTypeProperty) m))
