(ns land-of-lisp.ch7
  (:require [clojure.string :as str])
  (:use [clojure.java.io :only (writer)]
        [clojure.java.shell :only (sh)]
        [clojure.pprint :only (fresh-line)]))

(def *wizard-nodes* '((living-room (you are in the living-room.
                                        a wizard is snoring loudly on the couch.))
                      (garden (you are in a beautiful garden.
                                   there is a well in front of you.))
                      (attic (you are in the attic.
                                  there is a giant welding torch in the corner.))))

(def *wizard-edges* '((living-room (garden west door)  
                                (attic upstairs ladder))
                               (garden (living-room east door))
                               (attic (living-room downstairs ladder))))

(defn dot-name [exp]
  (str/replace exp #"[^A-Za-z0-9]" "_"))

(def *max-label-length* 30)

(defn dot-label [exp]
  (if-not  (empty? exp)
    (let [s (print-str exp)]
      (if (> (count s) *max-label-length*)
        (str (subs s 0 (- *max-label-length* 3)) "...")
        s))
    ""))

(defn nodes->dot [nodes]
  (apply str (map (fn [node]
                    (fresh-line)
                    (print-str (dot-name (first node))
                               "[label=\""
                               (dot-label node)
                               "\"];"))
                  nodes)))

(defn edges->dot [edges]
  (mapcat identity (map (fn [node]
          (map (fn [edge]
                 (fresh-line)
                 (print-str (dot-name (first node))
                            "->"
                            (dot-name (first edge))
                            "[label=\""
                            (dot-label (rest edge))
                            "\"];"))
               (rest node)))
        edges)))

(defn graph->dot [nodes edges]
  (print-str "digraph{"
             (apply str (nodes->dot nodes))
             (apply str (edges->dot edges))
             "}"))

(defn uedges->dot [edges]
  (mapcat identity 
          (letfn [(maplist 
                    ([s] (maplist identity s))
                    ([f s] (when-let [s (seq s)]
                             (lazy-seq (cons (f s)
                                             (maplist f (next s)))))))] 
            (maplist (fn [lst] 
                       (map (fn [edge]
                              (if-not (some #(= % (first edge)) (map first (rest lst)))
                                (do (fresh-line)
                                    (print-str (dot-name ((comp first first) lst))
                                               "--"
                                               (dot-name (first edge))
                                               "[label=\""
                                               (dot-label (rest edge))
                                               "\"];"))))
                            ((comp rest first) lst)))
                     edges))))

(defn ugraph->dot [nodes edges]
  (print-str "graph{"
             (apply str (nodes->dot nodes))
             (apply str (uedges->dot edges))
             "}"))

(defn dot->png [fname thunk]
  (with-open  [f (writer (str fname ".dot"))]
    (.write f thunk))
  (sh "dot" "-Tpng" "-O " (str fname ".dot")))

(defn graph->png [fname nodes edges]
  (dot->png fname
            (graph->dot nodes edges)))

(defn ugraph->png [fname nodes edges]
  (dot->png fname
            (ugraph->dot nodes edges)))

(defn run []
 (ugraph->png "wizard" *wizard-nodes* *wizard-edges*))
