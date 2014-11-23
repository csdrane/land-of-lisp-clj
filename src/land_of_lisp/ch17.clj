(ns land-of-lisp.ch17
  (:require [clojure.string :as str]))

(defn print-tag [name alst closing?]
  (let [sb (new StringBuilder)]
    (.append sb \<)
    (when closing?
      (.append sb \/))
    (.append sb (str/lower-case name))
    (doall 
     (map (fn [att]
            (.append sb (format " %s=\"%s\"" (str/lower-case (first att)) (second att))))
          alst))
    (.append sb \>)
    (.toString sb)))

(defmacro tag [name atts & body]
  `(str (print-tag '~name
                  (list ~@(map (fn [x#]
                                 `(list '~(first x#) ~(second x#)))
                               (partition 2 atts)))
                  nil)
       ~@body
       (print-tag '~name nil true)))

(defmacro html [& body]
  `(tag ~'html ()
        ~@body))

(defmacro body [& body]
  `(tag ~'body ()
        ~@body))


