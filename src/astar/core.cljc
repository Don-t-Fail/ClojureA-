(ns astar.core
  (:require #?(:clj  [clojure.data.priority-map :refer [priority-map-keyfn]]
               :cljs [tailrecursion.priority-map :refer [priority-map-keyfn]])))


(def graph {:a [:b :c]
            :b [:a :d :e]
            :c [:a :d :e]
            :d [:b :c :f]
            :e [:b :c :f]
            :f [:e :d]})

(defn dist [from to]
  (let [d {:a {:b 4 :c 2}
           :b {:a 4 :d 1 :e 1}
           :c {:a 2 :d 3 :e 5}
           :d {:b 1 :c 3 :f 3}
           :e {:b 1 :c 5 :f 2}
           :f {:d 3 :e 2}}]
    (get-in d [from to])))

(def h {:a 0
        :b 0
        :c 0
        :d 0
        :e 0
        :f 0})



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private generate-route [node came-from]
  (loop [route '()
         node node]
    (if (came-from node)
      (recur (cons node route) (came-from node))
      route)))

(defn route
  "Finds the shortest route from start to goal in a graph.
  Graph is a function (eg. a map) from nodes to a collection of adjacent nodes.
  Dist is a function from two nodes to the distance (as a number) from the first node to the second.
  H is a function from a node to the heuristic distance from that node to the goal. It should never overestimate the distance.
  Start and goal are two nodes.
  Returns a list of nodes on the route, excluding the start node and including the goal node. If a route can't be found, returns nil."
  [graph dist h start goal]
  (loop [visited {}
         queue (priority-map-keyfn first start [0 0 nil])]
    (when (seq queue)
      (let [[current [_ current-score previous]] (peek queue)
            visited (assoc visited current previous)]
        (if (= current goal)
          (generate-route goal visited)
          (recur visited (reduce (fn [queue node]
                                   (let [score (+ current-score (dist current node))]
                                     (if (and (not (contains? visited node))
                                              (or (not (contains? queue node))
                                                  (< score (get-in queue [node 1]))))
                                       (assoc queue node [(+ score (h node)) score current])
                                       queue)))
                                 (pop queue)
                                 (graph current))))))))
