(ns okay.types.seqs)

(defrecord SeqType [properties-list properties parser validator default-getter]
  TypeProtocol
  (validate [me value] (validator value))
  (parse [me value] (parser value))
  (get-default [me] (default-getter))


  (compose [me other]
    (cond
      (satisfies? TypeProtocol other)
        (add-parser-and-validator me other)
      (instance? AbstractTypeProperty other)
        (update-in me [:properties-list] conj other)
      :else
        (throw (Exception. (str "Type " (type other) " is not composable.")))))


  (finalize [{properties-list :properties-list}]
    (let [properties (reduce merge-type-properties {} properties-list)
          parser (make-parser properties)
          validator (make-validator properties)]
      (->SeqType
        properties-list
        properties
        #(mapv parser %)
        (validator-wrapper #(and (sequential? %) (every? validator %)))
        (make-default-getter properties)))))