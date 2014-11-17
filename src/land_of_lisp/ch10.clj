;; Land of Lisp, ch. 10
;; Translated into Clojure from Common Lisp
;; http://landoflisp.com/evolution.lisp

(ns land-of-lisp.ch10)

(def *width* 100)
(def *height* 30)
(def *jungle* '(45 10 10 10))
(def *plant-energy* 80)
(def ^:dynamic *plants* (atom #{}))
(def ^:dynamic *animals* (atom []))

(defn make-animal [{x :x y :y energy :energy dir :dir genes :genes}]
  (swap! *animals* conj (atom {:x x :y y :energy energy :dir dir :genes genes})))

(make-animal {:x (bit-shift-right *width* 1) 
              :y (bit-shift-right *height* 1)
              :energy 1000
              :dir 0
              :genes (list (map (fn [_] (inc (rand-int 10))) (range 8)))})

(defn random-plant [left top width height]
  (let [pos (list (+ left (rand-int width)) 
                  (+ top (rand-int height)))]
    (swap! *plants* conj pos)))

(defn add-plants []
  (apply random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

(defn move [animal]
  (let [dir (:dir @animal)
        x (:x @animal)
        y (:y @animal)]

    (swap! animal merge {:x (mod (+ x
                                    (cond 
                                     (and (>= dir 2) (< dir 5)) 1
                                     (or (= dir 1) (= dir 5)) 0
                                     :else -1)
                                    *width*)
                                 *width*)}) 
    (swap! animal merge {:y (mod (+ y
                                    (cond 
                                     (and (>= dir 0) (< dir 3)) -1 
                                     (and (>= dir 4) (< dir 7)) 1 
                                     :else 0)
                                    *height*)
                                 *height*)})
    (swap! animal update-in [:energy] dec)))


