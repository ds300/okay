(ns okay.types.maps
  (:require [okay.types.error :as error]
            [okay.types.proto :as proto]
            [okay.types.props :as props]
            [aladipert.kahn :refer [kahn-sort]]))

(defn all-key-paths
  "Finds all the key paths in a possibly-nested maptype structure"
  [structure]
  (apply concat
    (for [[k v] structure]
      (if (and (not (satisfies? proto/TypeProtocol v)) (map? v))
        (map #(into [k] %) (all-key-paths v))
        [[k]]))))

(defn make-graph
  "Extracts a (nodes => incoming-edges) map from a maptype structure, for use
   with Alan Dipert's topological sort."
  [structure]
  (into {}
    (for [ks (all-key-paths structure)]
      (let [withs (get-in structure
                    (into ks [:properties :validate-with-fields]))]
      (if (empty? withs)
        [ks #{}]
        [ks (into #{} withs)])))))

(defn get-topological-sort
  "Retrieves a topological sort for the given maptype structure, or throws
  a NotOkayException if a cyclical dependency is detected."
  [structure]
  (let [graph (make-graph structure)
        sorted (kahn-sort graph)]
    (if sorted
      (reverse sorted)
      (error/fail! "Cyclical dependency detected"))))

(defn update-in-when-not-nil
  "Like update-in, but does not modify the map's structure if the key path
  is not present."
  [m [k & ks] f]
  (if (and (map? m) (contains? m k))
    (if ks
      (assoc m k (update-in-when-not-nil ks (m k)))
      (assoc m k (f (m k))))
    m))

(defn make-field-parser
  "Returns a fn which updates a map to apply the field-type's parse method to
  the value at key-path, if the value exists."
  [key-path field-type]
  (let [parser (partial proto/parse field-type)]
    (fn [value]
      (try
        (update-in-when-not-nil value key-path parser)
        (catch Exception e
          (error/parse-fail! key-path (get-in value key-path) e))))))

(defn make-field-parsers
  "Takes a maptype structure and returns a list of fns which take a map value
  and modify its contents according to the structure's field specifications"
  [structure]
  (for [key-path (all-key-paths structure)]
    (let [field-type (get-in structure key-path)]
      (make-field-parser key-path field-type))))

(defn reverse-apply
  "calls f on value"
  [value f]
  (f value))

(defn make-parser
  "Returns a fn which takes a map and updates its values according to the
  given maptype structure."
  [structure]
  (let [parsers (make-field-parsers structure)]
    (fn [value]
      (reduce reverse-apply value parsers))))


(defn wrap-field-validator:cohort
  "If a field validator requries validation with other map fields, this
  funciton wraps it in one which does so."
  [validator fields f]
  (if (empty? fields)
    validator
    (fn [value]
      (and (validator value)
        (apply f
          (map (partial get-in value) fields))))))

(defn wrap-field-validator:exceptions
  "Wraps a field validator such that it throws an exception in stead of returns
  false. For fine-grained error reporting. (i.e. you know which key threw the
  exception)"
  [validator key-path]
  (fn [value]
    (try
      (or
        (validator value)
        (error/validation-fail! key-path (get-in value key-path)))
      (catch Exception e
        (error/validation-fail! key-path (get-in value key-path) e)))))

(defn wrap-field-validator:get-in
  "wraps the given field validator in a fn that searches for the right
  value to validate."
  [validator key-path required?]
  (fn [value]
    (loop [m value [k & ks] key-path]
      (if (and (map? m) (contains? m k))
        (if ks
          (recur (m k) ks)
          (validator value (m k)))
        (not required?)))))

(defn make-field-validator
  "makes a feild validator for the given key-path and field type."
  [key-path {props :properties :as field-type}]
  (let [cohort-fields (:validate-with-fields props)
        cohort-fn (:validate-with-fn props)
        required? (:required props)]
    (-> (partial proto/validate field-type)
      (wrap-field-validator:get-in key-path required?)
      (wrap-field-validator:cohort cohort-fields cohort-fn)
      (wrap-field-validator:exceptions key-path))))

(defn make-field-validators
  "returns a list of fns which each take a map, and validate a key path
  in that map, according to the maptype structure definition."
  [structure]
  (for [key-path (get-topological-sort structure)
        :let [field-type (get-in structure key-path)]]
    (make-field-validator key-path field-type)))

(defn wrap-validator:no-other-fields
  "if a maptype disallows unspecified fields, wraps validator in a fn
  which checks also whether any extraneous fields have been set."
  [validator structure properties]
  (if (and (contains? properties :allow-other-fields)
        (not (:allow-other-fields properties))
    (let [key-paths (all-key-paths structure)]
      (fn [value]
        (and (validator value)
          (let [reduced (reduce dissoc-in value key-paths)]
            (or (empty? reduced)
              (error/fail!
                "Unauthorized fields in map: " (all-key-paths reduced)))))))
    validator)))

(defn make-validator
  "makes the whole-map validator function."
  [structure properties]
  (-> (every-pred (make-field-validators structure))
    (wrap-validator:no-other-fields structure properties)
    error/validator-wrapper))


(defn make-map-default-getter
  "returns a function which gets the default values for the fields as specified
  in the maptype structure."
  [structure]
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
      (instance? proto/AbstractTypeProperty other)
        (->MapType (merge (:properties me) other) nil nil nil)
      :else
        (error/fail! "Map types are only composable with other map types")))

  (finalize [me] ;; you were here
    (props/validate properties)
    (if-let [structure (:structure properties)]
      (let [parser (make-map-parser structure)
            validator (make-validator structure properties)
            default-getter (util/make-default-getter properties
                             (make-map-default-getter structure))]
        (->MapType
          properties
          parser
          validator
          default-getter))

      (->MapType
        properties
        (partial update-with (partial parse (:base-type properties)))
        #(every? (partial validate (:base-type properties)) (keys %))
        (constantly {})))))
