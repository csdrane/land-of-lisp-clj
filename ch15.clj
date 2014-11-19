(ns land-of-lisp.ch12
  (:use[clojure.pprint :only (fresh-line)]))

(def *num-players* 2)
(def *max-dice* 3)
(def *board-size* 2)
(def *board-hexnum* (* *board-size* *board-size*))

(defn gen-board []
  (loop [n 0
         coll '()] 
    (if (< n *board-hexnum*)
      (recur (inc n) (cons (list (rand-int *num-players*)
                                 (inc (rand-int *max-dice*))) coll))
      coll)))

(defn player-letter [n]
  (char (+ 97 n)))

(defn draw-board [board]
  (doall 
   (for [y (range *board-size*)
         x (range *board-size*)] 
     (letfn [(hex [] 
               (nth board (+ x (* *board-size* y))))] 
       (do (fresh-line)
           (dotimes [_ (- *board-size* y)]
             (print "  "))
           (print (format "%s-%s " (player-letter (first (hex)))
                    (second (hex)))))))))

;; (draw-board (gen-board))

