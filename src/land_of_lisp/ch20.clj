;; Land of Lisp, ch. 20
;; Translated into Clojure from Common Lisp
;; http://landoflisp.com/dice_of_doom_v4.lisp

(ns land-of-lisp.ch20
  (:use [clojure.pprint :only (fresh-line)])
  (:use [land-of-lisp.ch17 :only (brightness polygon tag)]))

(def *num-players* 4)
(def *max-dice* 5)
(def *board-size* 5)
(def *board-hexnum* (* *board-size* *board-size*))
(def *board-width* 1000)
(def *board-height* 500) 
(def *board-scale* 64) 
(def *top-offset* 3)
(def *dice-scale* 40)
(def *dot-size* 0.05)
(def *ai-level* 2)

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

; Function is crashing when dice and players get ~ >=3. Unclear whether this is due to code issue or
; insufficient memory allocated to Java.
(defn add-new-dice [board player spare-dice] 
  #_(println "add-new-dice: board " board " player " player " spare-dice " spare-dice )
  (loop [lst board
         n spare-dice
         acc []]
    #_(println "lst " lst " n " n " acc " acc)
    (cond
     (nil? lst) nil
     (zero? n) (concat (reverse acc) lst)
     (empty? lst) (reverse acc)
     :else (let [cur-player (ffirst lst) 
                 cur-dice ((comp first rest first) lst)]
             (if (and (= cur-player player) 
                      (< cur-dice *max-dice*)) 
               (recur (rest lst) (dec n) (cons (list cur-player (inc cur-dice)) acc) )
               (recur (rest lst) n (cons (first lst) acc)))))))

(defn add-new-dice-ch19 [board player spare-dice] 
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
  #_(println "add-passing-move: board " board " player " player " spare-dice " spare-dice " first-move " first-move " moves " moves) 
  (lazy-seq (if first-move
              moves
              (cons (list nil
                          (game-tree (add-new-dice board player (dec spare-dice)) 
                                     (mod (inc player) *num-players*)
                                     0
                                     true))
                    moves))))


(defn board-attack [board player src dst dice] 
  #_(println "board-attack: board " board " player " player " src " src " dst " dst " dice " dice) 
  (board-set (for [pos (range (count board))] 
               (let [hex (nth board pos)] 
                 (cond 
                  (= pos src) (list player 1)
                  (= pos dst) (list player (dec dice))
                  :else hex)))))

(defn board-attack-fail [board player src dst dice]
  #_(println "board-attack-fail: board " board " player " player " src " src " dst " dst " dice " dice) 
  (board-set (for [pos (range (count board))]
               (let [hex (nth board pos)]
                 (cond
                  (= pos src) (list player 1)
                  :else hex)))))

(defn roll-dice [dice-num]
  (let [total (reduce + (repeat dice-num (inc (rand-int 6))))]
    (fresh-line)
    (println (format "On %s dice rolled %s. " dice-num total))
    total))

(defn roll-against [src-dice dst-dice]
  (> (roll-dice src-dice) (roll-dice dst-dice)))

(defn pick-chance-branch [board move]
  #_(println "pick-change-branch") 
  (letfn [(dice [pos]
            ((comp first rest) (nth board pos)))]
    (let [path (first move)]
      (if (or (nil? path) (roll-against (dice (first path))
                                        (dice ((comp first rest) path))))
        ((comp first rest) move)
        ((comp first rest rest) move)))))

(defn neighbors* [pos]
  (let [up (- pos *board-size*)
        down (+ pos *board-size*)]
    (filter (fn [p] (and (>= p 0) 
                         (< p *board-hexnum*))) 
            (for [p (concat (list up down)
                            (if-not (zero? (mod pos *board-size*)) (list (dec up) (dec pos)))
                            (if-not (zero? (mod (inc pos) *board-size*)) (list (inc pos) (inc down))))]
              p))))

(def neighbors (memoize neighbors*))

(defn attacking-moves [board cur-player spare-dice]
  #_(println "attacking-moves: board " board " cur-player " cur-player " spare-dice " spare-dice)
  (lazy-seq (letfn [(player [pos]
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
                                                        nil)
                                             (game-tree (board-attack-fail board cur-player src dst (dice src))
                                                        cur-player
                                                        (+ spare-dice (dice dst))
                                                        nil)))))
                                  (neighbors src))))
                      (range *board-hexnum*)))))

(defn game-tree* [board player spare-dice first-move] 
  #_(println "game-tree: board " board " player " player " spare-dice " spare-dice " first-move " first-move)
  (list player board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

(def game-tree (memoize game-tree*))

(defn print-info [tree]
  (fresh-line)
  (println (format "current player = %s" (player-letter (first tree))))
  (draw-board ((comp first rest) tree)))

(defn handle-human [tree]
  (print "choose your move: ")
  (fresh-line)
  (lazy-seq 
   (let [moves ((comp first rest rest) tree)
         ctr (atom 0)]
     (doseq [move moves]
       (let [action (first move)]
         (println (format "%s. " (swap! ctr inc))
                  (if (seq action)
                    (format "%s -> %s" (first action) ((comp first rest) action))
                    "end turn"))))
     (fresh-line) 
     (pick-chance-branch ((comp first rest) tree) (nth moves (dec (Integer/parseInt (read-line))))))))

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
      (println (format "The game is a tie between %s" (apply pr-str (map player-letter w)))) 
      (println (format "The winner is %s" (player-letter (first w)))))))

(defn play-vs-human [tree]
  (print-info tree)
  (if ((comp seq first rest rest) tree)
    (play-vs-human (handle-human tree))
    (announce-winner ((comp first rest) tree))))

(declare get-ratings score-board)

(defn rate-position* [tree player]
  (let [moves ((comp first rest rest) tree)]
    (if (seq moves) 
      (apply (if (= (first tree) player)
               max
               min)
             (get-ratings tree player))
      (score-board ((comp first rest) tree) player))))

(def rate-position (memoize rate-position*))

(defn get-ratings [tree player]
  (map (fn [move] (rate-position ((comp first rest) move) player))
       ((comp first rest rest) tree)))

(defn limit-tree-depth [tree depth]
  (lazy-seq 
   (list (first tree)
         ((comp first rest) tree)
         (if (zero? depth)
           nil
           (map (fn [move] (list (first move)
                                 (limit-tree-depth ((comp first rest) move) (dec depth))))
                ((comp first rest rest) tree))))))

(declare ab-get-ratings-max ab-get-ratings-min)

(defn handle-computer [tree]
  #_(println "handle-computer") 
  (lazy-seq 
   (let [ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
                                     (first tree)
                                     Integer/MAX_VALUE
                                     Integer/MIN_VALUE)]
     ((comp first rest) (nth ((comp first rest rest) tree) (.indexOf ratings (apply max ratings)))))))

(defn play-vs-computer [tree]
  #_(println "play-vs-computer")
  (print-info tree)
  (cond
   (empty? ((comp first rest rest) tree)) (announce-winner ((comp first rest) tree))
   (zero? (first tree)) (play-vs-computer (handle-human tree))
   :else (play-vs-computer (handle-computer tree))))

(defn threatened [pos board]
  (let [hex (nth board pos)
        player (first hex)
        dice ((comp first rest) hex)]
    (loop [ns (neighbors pos)]
      (let [n (first ns)
            nhex (nth board n) 
            nplayer (first nhex) 
            ndice ((comp first rest) nhex)]
        (cond
         (empty? (rest ns)) nil
         (and (not (= player nplayer)) (> ndice dice)) true
         :else (recur (rest ns)))))))

(defn score-board [board player]
  (letfn [(score [position pos board] 
            (if (= (first position) player)
              (if (threatened pos board)
                1 
                2)
              -1))] 
    (loop [positions board
           pos 0
           sum 0]
      (if (seq positions)
        (recur (rest positions) (inc pos) (+ sum
                                             (score (first positions)
                                                    pos
                                                    board)))
        sum))))

(defn ab-rate-position [tree player upper-limit lower-limit] 
  (let [moves ((comp first rest rest) tree)]
    (if-not (empty? moves)
      (if (= (first tree) player)
        (apply max (ab-get-ratings-max tree player
                                       upper-limit
                                       lower-limit)) 
        (apply min (ab-get-ratings-min tree
                                       player
                                       upper-limit
                                       lower-limit)))
      (score-board ((comp first rest) tree) player))))

(defn ab-get-ratings-max [tree player upper-limit lower-limit]
  (letfn [(f [moves lower-limit]
            (when-not (empty? moves)
              (let [x (ab-rate-position ((comp first rest first) moves)
                                        player
                                        upper-limit
                                        lower-limit)]
                (if (>= x upper-limit)
                  (list x)
                  (cons x (f (rest moves) (max x lower-limit)))))))]
    (f ((comp first rest rest) tree) lower-limit)))

(defn ab-get-ratings-min [tree player upper-limit lower-limit]
  (letfn [(f [moves upper-limit]
            (when-not (empty? moves)
              (let [x (ab-rate-position ((comp first rest first) moves)
                                        player
                                        upper-limit
                                        lower-limit)]
                (if (<= x lower-limit)
                  (list x)
                  (cons x (f (rest moves) (min x upper-limit)))))))]
    (f ((comp first rest rest) tree) upper-limit)))

(defmacro svg [height width & body]
  `(tag ~'svg (~'height ~height
               ~'width ~width
               ~'xmlns "http://www.w3.org/2000/svg"
               "xmlns:xlink" "http://www.w3.org/1999/xlink")
        ~@body))

(defn draw-die-svg [x y col]
  (let [sb (new StringBuilder)] 
    (letfn [(calc-pt [pt]
              (list (+ x (* *dice-scale* (first pt)))
                    (+ y (* *dice-scale* (second pt)))))
            (f [pol col]
              (polygon (mapcat calc-pt pol) col))]
      (.append sb (print-str (f '((0 -1) (-0.6 -0.75) (0 -0.5) (0.6 -0.75))
                     (brightness col 40))
                  (f '((0 -0.5) (-0.6 -0.75) (-0.6 0) (0 0.25))
                     col)
                  (f '((0 -0.5) (0.6 -0.75) (0.6 0) (0 0.25))
                     (brightness col -40))))
      (doall (map (fn [x y]
              (.append sb (polygon (mapcat (fn [xx yy]
                                             (calc-pt (list (+ x (* xx *dot-size*))
                                                            (+ y (* yy *dot-size*)))))
                                           '(-1 -1 1 1)
                                           '(-1 1 1 -1))
                                   '(255 255 255))))
            '(-0.05 0.125 0.3 -0.3 -0.125 0.05 0.2 0.2 0.45 0.45 -0.45 -0.2) 
            '(-0.875 -0.80 -0.725 -0.775 -0.70 -0.625
                     -0.35 -0.05 -0.45 -0.15 -0.45 -0.05)))
      (.toString sb))))

(defn draw-tile-svg [x y pos hex xx yy col chosen-tile] 
  (let [sb (new StringBuilder)]
    (loop [z 0]
      (if (< z 2)
        (do (.append sb 
                     (polygon (mapcat (fn [pt]
                                        (list (+ xx (* *board-scale* (first pt)))
                                              (+ yy (* *board-scale*
                                                       (+ (second pt) (* (- 1 z) 0.1))))))
                                      '((-1 -0.2) (0 -0.5) (1 -0.2)
                                        (1 0.2) (0 0.5) (-1 0.2)))
                              (if (= pos chosen-tile)
                                (brightness col 100)
                                col)))
            (recur (inc z)))))
    (loop [z 0] 
      (if (< z (second hex))
        (do (.append sb (draw-die-svg (+ xx
                                         (* *dice-scale*
                                            0.3
                                            (if (odd? (+ x y z))
                                              -0.3 0.3)))
                                      (- yy (* *dice-scale* z 0.8)) col))
            (recur (inc z)))))
    (.toString sb)))

(def *die-colors* '((255 63 63) (63 63 255) (63 255 63) (255 63 255)))

(defn make-game-link [pos]
  (format "/game.html?chosen=%s" pos))

(defn draw-board-svg [board chosen-tile legal-tiles]
  (let [sb (new StringBuilder)]
    (loop [y 0]
      (when (< y *board-size*)
        (loop [x 0]
          (let [pos (+ x (* *board-size* y))
                hex (nth board pos)
                xx (* *board-scale* (+ (* 2 x) (- *board-size* y)))
                yy (* *board-scale* (+ (* y 0.7) *top-offset*))
                col (brightness (nth *die-colors* (first hex)) (* -15 (- *board-size* y)))] 
            #_(println  " x " x " y " y " pos " pos " < (inc pos) *board-size*" (< (inc pos) *board-size*))
            (if (some #(= % pos) legal-tiles)
              (.append sb (tag g ()
                               (tag a ("xlink:href" (make-game-link pos))
                                    (draw-tile-svg x y pos hex xx yy col chosen-tile)))) 
              (.append sb (draw-tile-svg x y pos hex xx yy col chosen-tile)))
            (when (and (< x *board-size*) (< (inc pos) *board-hexnum*))
              (recur (inc x)))))
        (recur (inc y))))
    (.toString sb)))

