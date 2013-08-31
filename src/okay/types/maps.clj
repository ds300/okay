(ns okay.types.maps)


(defn make-map-field-parsers [structure]
  (for [key-path (all-key-paths structure)]
    (let [field-type (get-in structure key-path)
          parser (partial parse field-type)
          f (fn f [[k & ks] m]
              (if k ;; probably unneccesary. do a check dave
                (if (and (map? m) (contains? m k))
                  (if ks
                    (assoc m k (f ks (m k)))
                    (assoc m k (parser (m k))))
                  m)
                m))]
      (fn [m]
        (try
          (f key-path m)
        (catch Exception e
          (throw-parse-error key-path (get-in m key-path) e)))))))


(defn make-map-parser [structure]
  (let [parsers (make-map-field-parsers structure)]
    (fn [value]
      (reduce 
        (fn [acc f]
          (f acc))
        value
        parsers))))


(defn make-map-field-validators [structure]
  (for [key-path (get-topological-sort structure)]
    (let [field-type (get-in structure key-path)
          field-properties (:properties field-type)
          cohort-fields (:validate-with-fields field-properties)
          cohort-fn (:validate-with-fn field-properties)
          required? (:required field-properties)
          validator (if (empty? cohort-fields) 
                      (fn [map-value field-value]
                        (validate field-type field-value))
                      (fn [map-value field-value]
                        (and
                          (validate field-type field-value)
                          (apply cohort-fn
                            (map (partial get-in map-value) cohort-fields)))))]
      (fn [value]
        (try
          (or
            (loop [m value [k & ks] key-path]
              (if (and (map? m) (contains? m k))
                (if ks
                  (recur (m k) ks)
                  (validator value (m k)))
                (not required?)))
            (throw-validation-error key-path (get-in value key-path)))
          (catch Exception e
            (throw-validation-error key-path (get-in value key-path) e)))))))


(defn make-map-validator [structure]
  (let [validators (make-map-field-validators structure)]
    (fn [value]
      (loop [[v & more] validators]
        (if v
          (if (v value)
            (recur more)
            false)
          true)))))

(defn all-key-paths [m]
  (apply concat
    (for [[k v] m]
      (if (and (not (satisfies? TypeProtocol v)) (map? v))
        (map #(into [k] %) (all-key-paths v))
        [[k]]))))

(defn wrap-map-validator:no-other-fields [validator key-paths]
  (fn [value]
    (if (validator value)
      (let [reduced (reduce dissoc-in value key-paths)]
        (if (empty? reduced)
          true
          (throw (Exception. (str "Unauthorized fields in map: "(all-key-paths reduced))))))
      false)))


(defn make-graph [structure]
  (into {}
    (for [ks (all-key-paths structure)]
      (let [withs (get-in structure
                    (into ks [:properties :validate-with-fields]))]
      (if (empty? withs)
        [ks #{}]
        [ks (into #{} withs)])))))

(defn get-topological-sort [structure]
  (let [graph (make-graph structure)
        sorted (kahn-sort graph)]
    (if sorted
      (reverse sorted)
      (throw (Exception. "Cyclical dependency detected")))))

(defn make-map-default-getter [structure]
  (let [key-paths (all-key-paths structure)]
    (fn []
      (reduce (partial apply assoc-in) {}
        (map (juxt identity #(get-default (get-in structure %))) key-paths)))))


(defrecord MapType [properties parser validator default-getter]
  TypeProtocol
  (validate [me value] (validator value))
  (parse [me value] (parser value))
  (get-default [me] (default-getter))

  (compose [me other]
    (cond
      (instance? MapType other)
        (->MapType (apply map-merge (map :properties [me other])) nil nil nil)
      (instance? AbstractTypeProperty other)
        (->MapType (merge (:properties me) other) nil nil nil)
      :else
        (throw (Exception.
                 (str "Map types are only composable with other map types")))))

  (finalize [me]
    (if-let [structure (:structure properties)]
      (let [parser (make-map-parser structure)
            validator (make-map-validator structure)
            validator (if (and (contains? properties :allow-other-fields)
                               (not (:allow-other-fields properties)))
                        (wrap-map-validator:no-other-fields validator
                          (all-key-paths structure))
                        validator)
            default-getter (make-default-getter properties
                             (make-map-default-getter structure))]
        (->MapType
          properties
          parser
          (validator-wrapper validator)
          default-getter))

      (->MapType
        properties
        (partial update-with (partial parse (:base-type properties)))
        #(every? (partial validate (:base-type properties)) (keys %))
        (constantly {})))))