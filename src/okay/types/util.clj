(ns okay.types.util
  (:require [alandipert.kahn :refer [kahn]]))

(defn merge-type-properties [a b]
  (loop [acc a [[k v :as e] & more] (seq b)]
    (if e
      (case k
        :add-parser
          (recur (update-in acc [:parsers] (fnil conj []) v) more)
        :parse
          (recur (assoc acc :parsers [v]) more)
        :add-validator
          (recur (update-in acc [:validators] (fnil conj []) v) more)
        :validate
          (recur (assoc acc :parsers [v]) more)
        (recur (conj acc e) more))
      acc)))

(defn add-validator [abstype validator]
  (update-in abstype [:properties-list]
    conj (abstract-type-property {:add-validator validator})))

(defn add-parser [abstype parser]
  (update-in abstype [:properties-list]
    conj (abstract-type-property {:add-parser parser})))


(defn add-parser-and-validator [type-a type-b]
  (-> type-a
    (add-validator (partial validate type-b))
    (add-parser    (partial parse type-b))))

(defn update-with [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn map-merge [m1 m2]
  (into m1
    (for [[k v] m2]
      (if (every? map? [v (m1 k)])
        [k (map-merge v (m1 k))]
        [k v]))))

(defn make-parser [{parsers :parsers}]
  (if (empty? parsers)
    identity
    (apply comp (reverse parsers))))

(defn make-validator [{validators :validators}]
  (case (count validators)
    0 (constantly true)
    1 (first validators)
    (apply every-pred validators)))

(defn make-default-getter 
  ([m] (make-default-getter m (constantly nil)))
  ([{generate :gen-default default :default :as props} otherwise]
    (if generate
      generate
      (if (contains? props :default)
        (constantly default)
        otherwise))))


(defn dissoc-in [m [k & ks]]
  (if ks
    (if-let [child (m k)]
      (let [r (dissoc-in child ks)]
        (if (empty? r)
          (dissoc m k)
          (assoc m k r)))
      m)
    (dissoc m k)))
