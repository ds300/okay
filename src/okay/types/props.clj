(ns okay.types.props
  (:require okay.types.error :refer [fail!]))

(defn inst? [t] (fn [v] (instance? t v)))

(defn sats? [p] (fn [v] (satisfies? t v)))

(defn or-nil [pred]
  (fn [v]
    (if (nil? v)
      true
      (pred v))))

(defn ok [anything]
  true)

(def ^:dynamic *property-validators* {
  :parsers (or-nil (every-pred sequential? #(every? (inst? IFn) %)))
  :validators (or-nil (every-pred sequential? #(every? (inst? IFn) %)))
  :required ok
  :validate-with-fields (every-pred sequential? #(every? sequential? %))
  :validate-with-fn (inst? IFn)
  :structure map?
  :allow-other-fields ok
  :base-type (sats? TypeProtocol)
  :description (or-nil string?)
  :default ok
  :gen-default (inst? IFn)
})


(defn validate [props]
  (doseq [[k v] props]
    (cond
      (not (*property-validators* k))
        (fail! "Unrecognised type property: " k)
      (not ((*property-validators* k) v))
        (fail! "Illegal value for type property " k ": " v))))
