(require '[clojure.set :as set])
(def *congestion-city-nodes* nil) 
(def *congestion-city-edges* nil)
(def *visited-nodes* nil)
(def *node-num* 30)
(def *edge-num* 45)
(def *worm-num* 3)
(def *cop-odds* 15)

(defn random-node []
  (inc (rand-int *node-num*)))

(defn edge-pair [a b] 
  (if (not= a b)
    (list (list a b) (list b a))))

(defn make-edge-list []
  (concat
    (loop [n *edge-num*
           coll nil] 
      (if-not (= n 0)
        (recur (dec n) (concat (edge-pair (random-node) (random-node)) coll))
        coll))))

(defn direct-edges [node edge-list]
  (filter #(= (first %)  node) edge-list))

(defn get-connected [node edge-list]
  (let [visited (atom #{})]
    (letfn [(traverse [node]
              (if-not (@visited node)
                (doall
                 (map (fn [edge] 
                        (swap! visited conj node)
                        (traverse ((comp first rest) edge)))
                      (direct-edges node edge-list)))))]
      (traverse (first node)))
    @visited))

;; Haven't tested. I think this is right?
(defn find-islands [nodes edge-list] 
  (let [islands (atom #{})]
    (letfn [(find-island [nodes]
              (let [connected (get-connected (first nodes) edge-list)
                    unconnected (set/difference nodes connected)] 
                (swap! islands conj connected)
                (when unconnected
                  (find-island unconnected))))] 
      (find-island nodes))
    @islands))

;; Haven't tested. I think this is right?
(defn connect-with-bridges [islands] 
  (when (rest islands)
    (concat (edge-pair (ffirst islands) ((comp ffirst rest) islands)) 
            (connect-with-bridges (rest islands)))))

;; Haven't tested. I think this is right?
(defn connect-all-islands [nodes edge-list]
  (concat (connect-with-bridges (find-islands nodes edge-list)) edge-list))
