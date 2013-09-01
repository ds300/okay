(ns okay.types.vals
  (:require [okay.types.props :as props]
            [okay.types.proto :as proto]
            [okay.types.error :as error]
            [okay.types.util :as util]))



(defrecord ValType [properties-list properties parser validator default-getter]
  proto/TypeProtocol
  (validate [me value] (validator value))
  (parse [me value] (parser value))
  (get-default [me] (default-getter))

  (compose [me other]
    (cond
      (instance? ValType other)
        (->ValType (apply into (map :properties-list [me other]))
          nil nil nil nil)
      (satisfies? proto/TypeProtocol other)
        (util/add-parser-and-validator me other)
      (instance? proto/AbstractTypeProperty other)
        (update-in me [:properties-list] conj other)
      :else
        (error/fail! "Type " (type other) " is not composable.")))


  (finalize [me]
    (let [properties (reduce util/merge-type-properties {} properties-list)]
      (props/validate properties)
      (->ValType properties-list
        properties
        (make-parser properties)
        (validator-wrapper (make-validator properties))
        (make-default-getter properties)))))
