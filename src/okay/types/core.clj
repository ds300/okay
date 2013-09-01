(ns okay.types.core)



(declare seqtype)

(defn process-mixins [ms]
  (vec
    (for [m ms]
      (cond
        (satisfies? TypeProtocol m)
          m
          ;; allow abstract type properties
        (map? m)
          (abstract-type-property {:add-parser m})
        (vector? m)
          (seqtype m)
        (instance? clojure.lang.IFn m)
          (abstract-type-property {:add-validator m})
        :else
          (throw (Exception. (str "Type " (type m) " cannot be mixed in")))))))

(defn get-mixins-and-docstring [args]
  (let [[[mixins] remaining] (split-with vector? args)
        [[description] remaining] (split-with string? remaining)]
    [(or (process-mixins mixins) []) description remaining]))

(defn process-description-and-fields [description fields]
  (let [properties (abstract-type-property (apply hash-map fields))]
    (if description
      (assoc properties :description description)
      properties)))

(defn valtype [& args]
  (let [[mixins description fields] (get-mixins-and-docstring args)]
    (finalize
      (reduce compose (->ValType [] nil nil nil nil)
        (conj mixins (process-description-and-fields description fields))))))

(defn seqtype [& args]
  (let [[mixins description fields] (get-mixins-and-docstring args)]
    (finalize
      (reduce compose (->SeqType [] nil nil nil nil)
        (conj mixins (process-description-and-fields description fields))))))

(defn process-map-field [mixins field]
  (cond
    (satisfies? TypeProtocol field)
      field
    (vector? field)
      (finalize (valtype (into mixins field)))
    (map? field)
      (update-with (partial process-map-field mixins) field)))

(defn maptype [& args]
  (let [[mixins description fields] (get-mixins-and-docstring args)
        properties (process-description-and-fields description fields)
        base-type  (process-mixins (or (:base-type properties) []))
        structure  (when-let [structure (:structure properties)]
                     (update-with (partial process-map-field base-type)
                       structure))
        properties (if structure
                     (assoc properties :structure structure)
                     properties)]
    (finalize
      (reduce compose (->MapType {} nil nil nil)
        (conj mixins
          (->MapType (assoc properties :structure structure) nil nil nil))))))

(defmacro def-valtype [nm & args]
  `(def ~nm (valtype ~@args)))

(defmacro def-maptype [nm & args]
  `(def ~nm (maptype ~@args)))

(defmacro def-seqtype [nm & args]
  `(def ~nm (seqtype ~@args)))

(def-valtype integer [integer?]
  :parse  (fn [x]
            (if (integer? x)
              x
              (Integer. x))))

(def-valtype even-int [integer even?])

(def-seqtype int-seq [integer])

(def-valtype matrix [[[integer]]])

(validate matrix [[3] [4 5 6] [7 8 9]])

(def-valtype csv
  :parse #(clojure.string/split % #","))

(parse int-seq '(3 "3" 5 6 7))

(def-valtype csv-int [csv [integer]])

(def-valtype string [string?])

(def-maptype tweet
  :allow-other-fields false
  :structure
  {
    :text [string]
    :tokens [[string]]
    :id (valtype [integer] :required true)
    })

(validate tweet {:cheese false :text "scrotii are fun to poke" :id 7 :tokens ["yo"]})
