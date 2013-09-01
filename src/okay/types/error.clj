(ns okay.types.error
  (:import okay.NotOkayException))

(def ^:dynamic *emit-validation-errors* false)

(defn validator-wrapper [validator]
  (fn [value]
    (try
      (validator value)  
      (catch Exception e
        (if *emit-validation-errors*
          (throw e)
          false)))))

(defmacro not-ok! [msg key-path bad-value original-exception]
  `(throw (NotOkayException. ~msg ~key-path ~bad-value ~original-exception)))

(defmacro fail! [& args] `(not-ok! (str ~@args) nil nil nil))

(defmacro parse-fail! 
  ([key-path bad-value]
    `(parse-fail! ~key-path ~bad-value nil))
  ([key-path bad-value original-exception]
    `(not-ok! "Parsing failed." ~key-path ~bad-value ~original-exception)))

(defmacro validation-fail!
  ([key-path bad-value]
    `(validation-fail! ~key-path ~bad-value nil))
  ([key-path bad-value original-exception]
    `(not-ok! "Validation failed." ~key-path ~bad-value ~original-exception)))
