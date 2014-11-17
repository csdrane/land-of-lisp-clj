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

(defn randval [n]
  (inc (rand-int (max 1 n))))

(defn make-monster []
  {:health (atom (randval 10))})

(defn make-orc []
  (merge (make-monster) {:monster-type "Orc",
                         :club-level (randval 8)}))

(defn make-hydra []
  (merge (make-monster) {:monster-type "Hydra"}))

(defn make-slime-mold []
  (merge (make-monster) {:monster-type "Slime-Mold",
                         :sliminess (randval 5)}))

(defn make-brigand []
  (merge (make-monster) {:monster-type "Brigand"}))

(swap! *monster-builders* conj make-orc)
(swap! *monster-builders* conj make-hydra)
(swap! *monster-builders* conj make-slime-mold)
(swap! *monster-builders* conj make-brigand)

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
  (str "A wicked orc with a level " (:club-level m) " club."))

(defmethod monster-show "Hydra" [m]
  (str "A malicious hydra with " (monster-health m) " heads."))

(defmethod monster-show "Slime-Mold" [m]
  (str "A slime mold with a sliminess of " (:sliminess m) "."))

(defmethod monster-show :default [m]
  (str "A fierce " (monster-type m) "."))

(defmulti monster-hit (fn [m _] (monster-type m)))
(defmethod monster-hit "Hydra" [m x]
  (swap! (:health m) #(- % x))
  (if (monster-dead m)
    (println "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
    (println "You lop off" x "of the hydra's heads!")))

(defmethod monster-hit :default [m x]
  (swap! (:health m) #(- % x))
  (if (monster-dead m)
    (println "You killed the" (monster-type m) "! ")
    (println "You hit the" (monster-type m) ", knocking off" x "health points! ")))

(defmulti monster-attack (fn [m] (monster-type m)))
(defmethod monster-attack "Orc" [m]
  (let [x (randval (:club-level m))]
    (println "An orc swings his club at you and knocks off" x "of your health points. ")
    (swap! *player-health* #(- % x))))

(defmethod monster-attack "Hydra" [m]
  (let [x (randval (bit-shift-right (monster-health m) 1))]
    (println "A hydra attacks you with" x "of its heads! It also grows back one more head!")
    (swap! (:health m) inc)
    (swap! *player-health* #(- % x))))

(defmethod monster-attack "Slime-Mold" [m]
  (let [x (randval (:sliminess m))]
    (println "A slime mold wraps around your legs and decreases your agility by"
             x "!")
    (swap! *player-agility* #(- % x))
    (when (zero? (randval 2))
      (println "It also squirts in your face, taking away a health point!")
      (swap! *player-health* dec))))

(defmethod monster-attack "Brigand" [m]
  (let [x (max @*player-health* @*player-agility* @*player-strength*)]
    (cond
     (= x @*player-health*) 
     (do 
       (println "A brigand hits you with his slingshot, taking off 2 health points!")
       (swap! *player-health* #(- % 2)))
     (= x @*player-agility*) 
     (do 
       (println "A brigand catches your leg with his whip, taking off 2 agility points!")
       (swap! *player-agility* #(- % 2)))
     (= x @*player-strength*)
     (do 
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
          (vec (map (fn [_] ((nth @*monster-builders* (rand-int (count @*monster-builders*)))))
                    (range *monster-num*)))))

(defn show-monsters []
  (fresh-line)
  (println "Your foes:")
  #_(println @*monsters*)
  (let [x (atom 0)
        sb (new StringBuilder)]
; without doall, printlns won't evaluate due to map's laziness
    (doall (map (fn [m] 
                  (let [m (@*monsters* @x)] 
                    (.append sb \newline)
                    (.append sb (str "    " (swap! x inc) ". "))
                    (if (monster-dead m)
                      (.append sb "** dead **")
                      (do (.append sb (str "(Health= " (monster-health m) ") " ))
                          (.append sb (monster-show m)))))) 
                @*monsters*))
    (println (.toString sb))))

(defn random-monster []
  (let [m (@*monsters* (rand-int (count @*monsters*)))]
    (if (monster-dead m)
      (random-monster)
      m)))

(defn pick-monster []
  (fresh-line)
  (println "Monster #: ")
  (let [x (Integer/parseInt (read-line))]
    (if-not (and (>= x 1) (<= x *monster-num*))
      (do (println "That is not a valid monster number.")
          (pick-monster))
      (let [m (@*monsters* (dec x))]
        (if (monster-dead m)
          (do (println "That monster is already dead.")
              (pick-monster))
          m)))))

(defn player-attack []
  (fresh-line)
  (println "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read-line)
    "s" (monster-hit (pick-monster)
                     (+ 2 (randval (bit-shift-right @*player-strength* 1))))
    "d" (let [x (randval (Math/floor (/ @*player-strength* 6)))]
          (println "Your double swing has a strength of " x)
          (fresh-line)
          (monster-hit (pick-monster) x)
          (when-not (monsters-dead)
            (monster-hit (pick-monster) x)))
    (dotimes [x (inc (randval (Math/floor (/ @*player-strength* 3))))]
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
    ; again, need do-loop or else side effects won't be executed by map.
    (doall (map (fn [m] (or (monster-dead m) (monster-attack m)))
          @*monsters*))
    (game-loop)))

(defn orc-battle []
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (println "You have been killed. Game Over."))
  (when (monsters-dead)
    (println "Congratulations! You have vanquished all of your foes.")))
