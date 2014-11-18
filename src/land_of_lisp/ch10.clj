;; Land of Lisp, ch. 10
;; Translated into Clojure from Common Lisp
;; http://landoflisp.com/evolution.lisp

(ns land-of-lisp.ch10
  (:use[clojure.pprint :only (fresh-line pprint)]))

(def *width* 100)
(def *height* 30)
(def *jungle* '(45 10 10 10))
(def *plant-energy* 80)
(def *reproduction-energy* 200)
(def ^:dynamic *plants* (atom #{}))
(def ^:dynamic *animals* (atom []))

(defn make-animal [{x :x y :y energy :energy dir :dir genes :genes}]
  (swap! *animals* conj (atom {:x x :y y :energy energy :dir dir :genes genes})))

(make-animal {:x (bit-shift-right *width* 1) 
              :y (bit-shift-right *height* 1)
              :energy 1000
              :dir 0
              :genes (vec (map (fn [_] (inc (rand-int 10))) (range 8)))})

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

(defn turn [animal]
  (let [x (rand-int (apply + (:genes @animal)))]
    (letfn [(angle [genes x]
              (let [xnu (- x (first genes))]
                (if (< xnu 0)
                  0
                  (inc (angle (rest genes) xnu)))))]
      (swap! animal merge {:dir
                           (mod (+ (:dir @animal) 
                                   (angle (:genes @animal) x))
                                8)}))))

(defn eat [animal]
  (let [pos (list (:x @animal) (:y @animal))]
    (when (@*plants* pos)
      (swap! animal update-in [:energy] #(+ % *plant-energy*))
      (swap! *plants* disj pos))))

(defn reproduce [animal]
  (let [e (:energy @animal)]
    (when (>= e *reproduction-energy*)
      (swap! animal #(update-in % [:energy] bit-shift-right 1))
      (let [animal-nu @animal
            genes (:genes @animal)
            mutation (rand-int 8)
            mutated-genes (assoc genes mutation (max 1 (+ (nth genes mutation) (rand-int 3) -1)))]
        (swap! *animals* conj (atom (merge animal-nu {:genes mutated-genes})))))))

(defn update-world []
  (swap! *animals* (fn [animals] (vec (filter #(> (:energy @%) 0) animals))))
  (doall (map (fn [animal] 
                (turn animal)
                (move animal)
                (eat animal)
                (reproduce animal))
              @*animals*))
  (add-plants))

(defn draw-world [] 
  (let [sb (new StringBuilder)] 
    (loop [y 0]
      (when (<= y *height*) 
        (.append sb "|") 
        (loop [x 0] 
          (when (<= x *width*) 
            (.append sb (cond 
                         (some (fn [animal]
                                 (and (= (:x @animal) x)
                                      (= (:y @animal) y))) 
                               @*animals*)
                         \M
                         (@*plants* (list x y)) \*
                         :else \space))
            (recur (inc x))))
        :continue
        (.append sb (str  "|" \newline))
        (recur (inc y))))
    (println (.toString sb))))

(defn evolution []
  (draw-world)
  (fresh-line)
  (let [s (read-line)]
    (cond 
     (= s "quit") ()
     :else (let [x (try (Integer/parseInt s) (catch Exception e 1))]
             (if x
               (dotimes [i x]
                 (update-world)
                 (if (zero? (mod i 1000))
                   (println \.))
                 (update-world)))
             (evolution)))))

