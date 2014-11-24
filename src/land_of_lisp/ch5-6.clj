;; Land of Lisp, ch. 5 & 6
;; Translated into Clojure from Common Lisp
;; http://landoflisp.com/wizards_game.lisp

(ns land-of-lisp.ch5-6 
  (:require [clojure.string :as str]))

(def ^:dynamic *location* 'living-room)
(def ^:dynamic *allowed-commands* (atom '(look walk pickup inventory)))
(def *objects* '(whiskey bucket frog chain))

(def *nodes* {'living-room '(you are in the living-room.
                                a wizard is snoring loudly on the couch.)
              'garden '(you are in a beautiful garden.
                           there is a well in front of you.)
              'attic '(you are in the attic.
                          there is a giant welding torch in the corner.)})

(def *edges* {'living-room '((garden west door)  
                             (attic upstairs ladder))
              'garden '((living-room east door))
              'attic '((living-room downstairs ladder))})

(def ^:dynamic *object-locations* {'whiskey 'living-room
                         'bucket 'living-room
                         'chain 'garden                         
                         'frog 'garden})

(defn describe-location [location nodes]
  (location nodes))

(defn describe-path [edge]
  `(there is a ~((comp first rest rest) edge) going ~((comp first rest ) edge) from here.))

(defn describe-paths [location edges]
  (apply concat (map describe-path (location edges))))

(defn objects-at [loc objs obj-loc]
  (letfn [(is-at [obj]
            (= (obj obj-loc) loc))]
       (filter is-at objs)))

(defn describe-objects [loc objs obj-loc]
  (letfn [(describe-obj [obj]
            `(you see a ~obj on the floor.))]
    (apply concat (map describe-obj (objects-at loc objs obj-loc)))))
(describe-objects 'living-room *objects* *object-locations*)

(defn spel-print [list] (map (comp symbol name) list))

(defn look []
  (concat (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defn walk [direction]
  (let [next- (ffirst (filter (fn [x] (= direction (second x))) (*location* *edges*)))]
    (if next-
      (do 
        (def *location* next-)
        (look))
      '(you cannot go that way.))))

(defn pickup [object]
  (cond 
   (some #(= % object) (objects-at *location* *objects* *object-locations*)) 
   (do 
     (def *object-locations* (assoc *object-locations* object 'body))
     `(you are now carrying the ~object))
   :else `(you cannot get that.)))

(defn inventory []
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defn game-read []
  (read-string (str \( (read-line) \))))

(defn game-eval [sexp]
  (if (some #(= % (first sexp)) (deref *allowed-commands*))
    (apply (resolve (first sexp)) (rest sexp))
    '(i do not know that command.)))

(defn tweak-text [text]
  (apply str (map str/capitalize (map str/lower-case (re-seq #"[A-Za-z0-9\-\s]+[.!?]*\s*" (print-str text))))))

(defn game-print [lst]
  (println (tweak-text (list (str/replace (print-str (spel-print lst)) #"(^\()|(\)$)" "")))))

(defn game-repl []
  (let [cmd (game-read)] 
    (if-not (= (first cmd) 'quit) 
      (do (game-print (game-eval cmd))
          (game-repl)))))
