;; Land of Lisp, ch. 15
;; Translated into Clojure from Common Lisp
;; http://landoflisp.com/dice_of_doom_v1.lisp

(ns land-of-lisp.ch15
  (:use[clojure.pprint :only (fresh-line)]))

(def *num-players* 2)
(def *max-dice* 3)
(def *board-size* 2)
(def *board-hexnum* (* *board-size* *board-size*))

(defn board-set [board]
  board)

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
   (for [y (range *board-size*)] 
     (letfn [(hex [x] 
               (nth board (+ x (* *board-size* y))))] 
       (do (let [sb (new StringBuilder)] 
             (dotimes [_ (- *board-size* y)]
               (.append sb "  "))
             (dotimes [x *board-size*]
               (.append sb (format "%s-%s " (player-letter (first (hex x)))
                                   (second (hex x)))))
             (print (.toString sb))
             (fresh-line)))))))

(declare game-tree)

(defn add-new-dice [board player spare-dice] 
  (letfn [(f [lst n]
            (cond 
             (nil? lst) nil 
             (zero? n) lst
             :else (let [cur-player (ffirst lst) 
                         cur-dice ((comp first rest first) lst)]
                     (if (and (= cur-player player) 
                              (< cur-dice *max-dice*)) 
                       (cons (list cur-player (inc cur-dice))
                             (f (rest lst) (dec n)))
                       (cons (first lst) (f (rest lst) n))))))]
    (f board spare-dice)))

(defn add-passing-move [board player spare-dice first-move moves]
  (if first-move
    moves
    (cons (list nil
                (game-tree (add-new-dice board player (dec spare-dice)) 
                           (mod (inc player) *num-players*)
                           0
                           true))
          moves)))

(defn board-attack [board player src dst dice] 
  (board-set (for [pos (range (count board))] 
               (let [hex (nth board pos)] 
                 (cond 
                  (= pos src) (list player 1)
                  (= pos dst) (list player (dec dice))
                  :else hex)))))

(defn neighbors [pos]
  (let [up (- pos *board-size*)
        down (+ pos *board-size*)]
    (filter (fn [p] (and (>= p 0) 
                         (< p *board-hexnum*))) 
            (for [p (concat (list up down)
                            (if-not (zero? (mod pos *board-size*)) (list (dec up) (dec pos)))
                            (if-not (zero? (mod (inc pos) *board-size*)) (list (inc pos) (inc down))))]
              p))))

(defn attacking-moves [board cur-player spare-dice]
  (letfn [(player [pos]
            (first (nth board pos)))
          (dice [pos]
            ((comp first rest) (nth board pos)))]
    (mapcat (fn [src]
              (when (= (player src) cur-player)
                (mapcat (fn [dst]
                          (when (and (not= (player dst) cur-player) 
                                     (> (dice src) (dice dst)))
                            (list
                             (list (list src dst)
                                   (game-tree (board-attack board cur-player src dst (dice src))
                                              cur-player
                                              (+ spare-dice (dice dst))
                                              nil)))))
                        (neighbors src))))
            (range *board-hexnum*))))

(defn game-tree [board player spare-dice first-move] 
  (list player board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

(defn print-info [tree]
  (fresh-line)
  (println (format "current player = %s" (player-letter (first tree))))
  (draw-board ((comp first rest) tree)))

(defn handle-human [tree]
  (print "choose your move: ")
  (fresh-line)
  (let [moves ((comp first rest rest) tree)
        ctr (atom 0)]
    (doseq [move moves]
      (let [action (first move)]
        (println (format "%s. " (swap! ctr inc))
                 (if (seq action)
                   (format "%s -> %s" (first action) ((comp first rest) action))
                   "end turn"))))
    (fresh-line) 
    ((comp first rest) (nth moves (dec (Integer/parseInt (read-line)))))))

(defn winners [board]
  (let [tally (map first board)
        totals (frequencies tally)
        best (apply max (vals totals))]
    (map first
         (filter #(-> % val (= best)) totals))))

(defn announce-winner [board]
  (fresh-line)
  (let [w (winners board)]
    (if (> (count w) 1)
      (format "The game is a tie between %s" (map player-letter w)) 
      (format "The winner is %s" (player-letter (first w))))))

(defn play-vs-human [tree]
  (print-info tree)
  (if ((comp seq first rest rest) tree)
    (play-vs-human (handle-human tree))
    (announce-winner ((comp first rest) tree))))
