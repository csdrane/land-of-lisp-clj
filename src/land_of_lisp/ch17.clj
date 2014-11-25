(ns land-of-lisp.ch17
  (:require [clojure.string :as str])
  (:use [land-of-lisp.ch5_6]))

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
                  (doall (list ~@(map (fn [x#]
                                  `(list '~(first x#) ~(second x#)))
                                (partition 2 atts))))
                  nil)
       ~@body
       (print-tag '~name nil true)))

(defmacro html [& body]
  `(tag ~'html ()
        ~@body))

(defmacro body [& body]
  `(tag ~'body ()
        ~@body))

(defmacro svg [& body]
  `(tag ~'svg (~'xmlns "http://www.w3.org/2000/svg"
                       "xmlns:xlink" "http://www.w3.org/1999/xlink")
        ~@body))

(defn brightness [col amt]
  (map (fn [x]
         (min 255 (max 0 (+ x amt))))
       col))

(defn svg-style [color]
  (apply format "fill:rgb(%s,%s,%s);stroke:rgb(%s,%s,%s)"
          (concat color (brightness color -100))))

(defn circle [center radius color]
  (tag circle (cx (first center)
                  cy (second center)
                  r radius
                  style (svg-style color))))

(defn polygon [points color]
  (tag polygon (points 
                (str/join (map (fn [tp]
                                 (format
                                  "%s,%s " (first tp) (second tp)))
                               (partition 2 points)))
                style (svg-style color))))

(defn random-walk [value length]
  (when-not (zero? length)
    (cons value
          (random-walk (if (zero? (rand-int 2))
                         (dec value)
                         (inc value))
                       (dec length)))))

(defn generate-random-walk-image [] 
  (with-open [w (clojure.java.io/writer "random_walk.svg")] 
    (binding [*out* w]
      (println 
       (svg 
        (print-str
         (for [_ (range 10)] 
           (polygon (concat '(0 200) (interleave (range 400) 
                                                 (random-walk 100 400)) '(400 200))
                    (map (fn [_] (rand-int 256)) (range 3))))))))))

(defn have [object]
  (some #(= % object) (inventory)))

(def ^:dynamic *chain-welded* nil)

(defn weld [subject object]
  (if (and (= *location* 'attic)
           (= subject 'chain)
           (= object 'bucket)
           (have 'chain)
           (have 'bucket)
           (not *chain-welded*))
    (do (def *chain-welded* true)
        '(the chain is now securely welded to the bucket.))
    '(you cannot weld like that.)))

(swap! *allowed-commands* conj 'weld)

(def ^:dynamic *bucket-filled* nil)

(defn dunk [subject object]
  (if (and (= *location* 'garden)
           (= subject 'bucket)
           (= object 'well)
           (have 'bucket)
           *chain-welded*)
    (do (def *bucket-filled* true)
        '(the bucket is now full of water))
    '(you cannot dunk like that.)))

(swap! *allowed-commands* conj 'dunk)

(defmacro game-action [command subj obj place & body]
  `(do (defn ~command [subject# object#]
         (if (and (= *location* '~place)
                  (= subject# '~subj)
                  (= object# '~obj)
                  (have '~subj))
           ~@body
           '(i cant ~command like that.)))
       (swap! *allowed-commands* conj '~command)))

(game-action weld chain bucket attic
             (if (and (have 'bucket) 
                      (not *chain-welded*))
               (do (def *chain-welded* true)
                   '(the chain is now securely welded to the bucket.))
               '(you do not have a bucket.)))

(game-action dunk bucket well garden
             (if *chain-welded*
               (do (def *bucket-filled* true)
                      '(the bucket is now full of water))
               '(the water level is too low to reach.)))

(game-action splash bucket wizard living-room
             (cond 
              (not *bucket-filled*) '(the bucket has nothing in it.)
              (have 'frog) '(the wizard awakens and sees that you stole his frog.
                                 he is so upset he banishes you to the netherworlds- you lose! the end.)
              :else '(the wizard awakens from his slumber and greets you warmly.
                          he hands you the magic low-carb donut- you win! the end.)))
