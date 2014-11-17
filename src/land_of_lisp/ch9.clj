;; Land of Lisp, ch. 9
;; Translated into Clojure from Common Lisp
;; http://landoflisp.com/orc-battle.lisp

(ns land-of-lisp.ch9
  (:use[clojure.pprint :only (fresh-line)]))

(def ^:dynamic *player-health* (atom nil)) 
(def ^:dynamic *player-agility* (atom nil)) 
(def ^:dynamic *player-strength* (atom nil))

(def ^:dynamic *monsters* (atom [])) 
(def ^:dynamic *monster-builders* (atom []))
(def ^:dynamic *monster-num* 12)

(defn make-monster []
  {:health (atom (rand-int 10))})

(defn make-orc []
  (merge (make-monster) {:monster-type "Orc",
                         :club-level (rand-int 8)}))

(defn make-hydra []
  (merge (make-monster) {:monster-type "Hydra"}))

(defn make-slime-mold []
  (merge (make-monster) {:monster-type "Slime-Mold",
                         :sliminess (rand-int 5)}))

(defn make-brigand []
  (merge (make-monster) {:monster-type "Brigand"}))

(swap! *monster-builders* conj (make-orc))
(swap! *monster-builders* conj (make-hydra))
(swap! *monster-builders* conj (make-slime-mold))
(swap! *monster-builders* conj (make-brigand))

(defn monster-type [m]
  (m :monster-type))

(defn monster-health [m]
  (deref (m :health)))

(defn monster-dead [m]
  (<= (monster-health m) 0))

(defn monsters-dead [] 
  (every? monster-dead @*monsters*))

(defmulti monster-show (fn [m] (monster-type m)))
(defmethod monster-show "Orc" [m]
  (println "A wicked orc with a level" (:club-level m) "club"))

(defmethod monster-show "Hydra" [m]
  (println "A malicious hydra with" (monster-health m) "heads."))

(defmethod monster-show "Slime-Mold" [m]
  (println "A slime mold with a sliminess of" (:sliminess m)))

(defmethod monster-show :default [m]
  (println "A fierce" (monster-type m)))

(defmulti monster-hit (fn [m _] (monster-type m)))
(defmethod monster-hit "Hydra" [m x]
  (swap! (monster-health m) #(- @% x))
  (if (monster-dead m)
    (println "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
    (println "You lop off" x "of the hydra's heads!")))

(defmethod monster-hit :default [m x]
  (swap! (monster-health m) #(- % x))
  (if (monster-dead m)
    (println "You killed the" (monster-type m) "! ")
    (println "You hit the" (monster-type m) ", knocking off" x "health points! ")))

(defmulti monster-attack (fn [m] (monster-type m)))
(defmethod monster-attack "Orc" [m]
  (let [x (rand-int (:club-level m))]
    (println "An orc swings his club at you and knocks off" x "of your health points. ")
    (swap! *player-health* #(- % x))))

(defmethod monster-attack "Hydra" [m]
  (let [x (rand-int (bit-shift-right (monster-health m) 1))]
    (println "A hydra attacks you with" x "of its heads! It also grows back one more head!")
    (swap! (monster-health m) inc)
    (swap! *player-health* #(- % x))))

(defmethod monster-attack "Slime-Mold" [m]
  (let [x (rand-int (:sliminess m))]
    (println "A slime mold wraps around your legs and decreases your agility by"
             x "!")
    (swap! *player-agility* #(- % x))
    (when (zero? (rand-int 2))
      (println "It also squirts in your face, taking away a health point!")
      (swap! *player-health* dec))))

(defmethod monster-attack "Brigand" [m]
  (let [x (max @*player-health* @*player-agility* @*player-strength*)]
    (cond
     (= x @*player-health*) (do 
                             (println "A brigand hits you with his slingshot, taking off 2 health points!")
                             (swap! *player-health* #(- % 2)))
     (= x @*player-agility*) (do 
                              (println "A brigand catches your leg with his whip, taking off 2 agility points!")
                              (swap! *player-agility* #(- % 2)))
     (= x @*player-strength*) (do 
                               (println "A brigand cuts your arm with his whip, taing off 2 strength points!")
                               (swap! *player-strength* #(- % 2))))))

(defn init-player []
     (reset! *player-health* 30)
     (reset! *player-agility* 30)
     (reset! *player-strength* 30))

(defn player-dead []
  (<= @*player-health* 0))

(defn show-player []
  (fresh-line)
  (println "You are a valiant knight with a health of" @*player-health* ", "
           "an agility of" @*player-agility* ", and a strength of" 
           @*player-strength*))

(defn init-monsters []
  (reset! *monsters* 
          (vec (map (fn [_] (nth @*monster-builders* (rand-int (count @*monster-builders*))))
                    (range *monster-num*)))))

(defn show-monsters []
  (fresh-line)
  (println "Your foes:")
  (println @*monsters*)
  (dotimes [x (count @*monsters*)]
    ; without doall, printlns won't evaluate due to map's laziness
    (doall (map (fn [m] 
            (let [m (@*monsters* x)] 
              (fresh-line)
              (println "    " x ". ")
              (if (monster-dead m)
                (println "** dead **")
                (do (println "Health=" (monster-health m)) ") "
                    (monster-show m))))) 
          @*monsters*))))

(defn random-monster []
  (let [m (@*monsters* (rand-int (count @*monsters*)))]
    (if (monster-dead m)
      (random-monster)
      m)))

(defn pick-monster []
  (fresh-line)
  (println "Monster #: ")
  (let [x (read)]
    (if-not (and (integer? x) (>= x 1) (<= x *monster-num*))
      (do (println "That is not a valid monster number.")
          (pick-monster))
      (let [m (*monsters* (dec x))]
        (if (monster-dead m)
          (do (println "That monster is already dead.")
              (pick-monster))
          m)))))

(defn player-attack []
  (fresh-line)
  (println "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    ("s" (monster-hit (pick-monster)
                    (+ 2 (rand-int (bit-shift-right @*player-strength* 1)))))
    ("d" (let [x (rand-int (Math/floor (/ @*player-strength* 6)))]
         (println "Your double swing has a strength of " x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (when-not (monsters-dead)
           (monster-hit (pick-monster) x))))
    (dotimes [x (inc (rand-int (Math/floor (/ @*player-strength* 3))))]
      (when-not (monsters-dead)
        (monster-hit (random-monster) 1)))))

(defn game-loop []
  (when-not (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes [k (inc (int (Math/floor (/ (max 0 @*player-agility*) 15))))]
      (when-not (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map (fn [m] (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

(defn orc-battle []
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (println "You have been killed. Game Over."))
  (when (monsters-dead)
    (println "Congratulations! You have vanquished all of your foes.")))
