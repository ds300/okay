(ns okay.types.error)

(def ^:dynamic *emit-validation-errors* false)

(defn throw-error [msg] (throw (CFGException. msg nil nil nil)))

(defn throw-parse-error 
  ([key-path bad-value]
    (throw-parse-error key-path bad-value nil))
  ([key-path bad-value original-exception]
    (throw
      (CFGException. "Parsing failed." key-path bad-value original-exception))))

(defn throw-validation-error
  ([key-path bad-value]
    (throw-validation-error key-path bad-value nil))
  ([key-path bad-value original-exception]
    (throw
      (CFGException. "Validation failed." key-path bad-value original-exception))))