(ns okay.types.vals)



(defrecord ValType [properties-list properties parser validator default-getter]
  TypeProtocol
  (validate [me value] (validator value))
  (parse [me value] (parser value))
  (get-default [me] (default-getter))

  (compose [me other]
    (cond
      (instance? ValType other)
        (->ValType (apply into (map :properties-list [me other]))
          nil nil nil nil)
      (satisfies? TypeProtocol other)
        (add-parser-and-validator me other)
      (instance? AbstractTypeProperty other)
        (update-in me [:properties-list] conj other)
      :else
        (throw (Exception. (str "Type " (type other) " is not composable.")))))


  (finalize [me]
    (let [properties (reduce merge-type-properties {} properties-list)]
      (->ValType properties-list
        properties
        (make-parser properties)
        (validator-wrapper (make-validator properties))
        (make-default-getter properties)))))