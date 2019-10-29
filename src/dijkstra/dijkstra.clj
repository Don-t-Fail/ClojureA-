(require '[clojure.string :as str])

(def cityroads nil)
(def citymap nil)

(def citymap
  {:alpha {:charlie 37, :beta 12}
   :beta {:alpha 12, :delta 35}
   :charlie {:alpha 37, :echo 51, :delta 42}
   :delta {:beta 35, :charlie 42, :foxtrot 17}
   :echo {:charlie 51, :foxtrot 33}
   :foxtrot {:echo 33, :delta 17}}
  )

(defn insert [city1 city2 road distance]
  (if
    (or
      (not (contains? ((keyword city1) citymap) (keyword city2)))
      (> (((keyword city1) citymap) (keyword city2)) (read-string distance))
      )
    (do
      (def citymap (update citymap (keyword city1) (fnil conj {}) (assoc {} (keyword city2) (read-string distance))))
      (def citymap (update citymap (keyword city2) (fnil conj {}) (assoc {} (keyword city1) (read-string distance))))
      (def cityroads (update cityroads (keyword city1 city2) #(str road %)))
      (def cityroads (update cityroads (keyword city2 city1) #(str road %)))
      )
    )
  )

(defn dijkstra [world start]
  (and (start world)
       ((fn [closed open]
          (let [open-routes
                ((fn [closed-route links closed open]
                   (if
                     (empty? links) open
                                    (let [current-link (first links)
                                          current-node (first current-link)
                                          current-open (first (current-node open))
                                          current-route {(conj (first closed-route) current-link), (+ (last closed-route) (last current-link))}
                                          shortest-open-route (or (nil? current-open) (< (last (first current-route)) (last current-open)))
                                          can-add-route (and (nil? (first (current-node closed))) shortest-open-route)
                                          open-updated (cond can-add-route (assoc open current-node current-route) :else open)
                                          ]
                                      (recur closed-route (rest links) closed open-updated)
                                      )
                                    )
                   ) (first (last (last closed))) ((first (last closed)) world) closed open)
                abort (if (empty? open-routes) true false)
                shortest-open
                ((fn [open shortest]
                   (cond
                     (empty? open) shortest
                     :else
                     (let [potential (first open)
                           shortest-changes (< (last (last (last potential))) (last (last (last shortest))))
                           shortest-updated (cond shortest-changes potential :else shortest)
                           ]
                       (recur (rest open) shortest-updated)
                       )
                     )
                   ) (rest open-routes) (first open-routes))
                closed-updated (conj closed shortest-open)]
            (if
              (or (= (count closed-updated) (count world)) abort) closed-updated
                                                                  (recur closed-updated (dissoc open-routes (first shortest-open)))
                                                                  )
            )
          ) {start {{start 0}, 0}} {}
        )
       )
  )

(defn output [route]
  (if-not (nil? (second route))
    (do
      ((fnil str "") (output (dissoc route (first (first route)))) (format "%-21s%-21s%-11s%5s\n" (name (first (first route))) (name (first (second route))) ((keyword (name (first (first route))) (name (first (second route)))) cityroads) (second (second route))))
      )
    )
  )

(defn findroute [start end]
  (time
    (map
      (fn [[route total]]
        (printf "%-21s%-21s%-11s%5s\n" "From" "To" "Route" "Miles")
        (printf "%-21s%-21s%-11s%5s\n" "--------------------" "--------------------" "----------" "-----")
        (print (output route))
        (printf "%58s\n" "-----")
        (printf "%53s%5s\n" "Total" total)
        )
      ((keyword end) (dijkstra citymap (keyword start)))
      )
    )
  )

; !!!!! RUN THIS FIRST TO LOAD DATA !!!!!!
;(csvtomap csv)

;(findroute start end)
;(findroute "Santa Barbara" "Las Vegas")

