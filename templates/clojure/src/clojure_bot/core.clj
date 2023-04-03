(ns clojure-bot.core
  (:require [clojure-bot.api :as api]
            [clojure.math :as math]
            [clojure.string :as str])
  (:gen-class))

(def initial-state {})

(defn generate-game-state
  [initial-state map-data server-state]
  (let [{:keys [player id]} server-state]
    (assoc initial-state
      :level (assoc map-data :exit (get-in server-state [:map :exit]))
      :alive? true
      :id id
      :player player)))

(defn process-map
  [tiles]
  (into {}
    (map-indexed
      (fn [row-index row]
        [row-index
         (into {}
           (map-indexed
             (fn [tile-index character]
               [tile-index
                (case character
                  \x {:x tile-index :y row-index :weight 999}
                  \_ {:x tile-index :y row-index :weight 1}
                  {:x tile-index :y row-index :weight 0})])
             row))])
      tiles)))

(defn heuristic
  "Calculates manhattan distance (diff x + diff y) between nodes start & end"
  [start end]
  (let [{start-x :x start-y :y} start
        {end-x :x end-y :y} end]
    (+ (abs (- start-x end-x))
        (abs (- start-y end-y)))))

(defn map-node
  [position map-data]
  (let [{:keys [x y]} position]
    #_(prn :-> x y (get-in map-data [y x]))
    (get-in map-data [y x])))

(defn calculate-weight
  [current from end]
  #_(println current from end)
  (assoc current :route-weight (+ (:weight current) (:weight from) (:distance current) (heuristic current end))))

(defn same-node?
  [{start-x :x start-y :y :as _current} {end-x :x end-y :y :as _end}]
  (and (= start-x end-x)
    (= start-y end-y)))

(defn unvisited-neighbors
  [current visited open end map-data]
  (let [neighbors [(-> current (update :x inc))
                   #_(-> current (update :x inc) (update :y inc))
                   #_(-> current (update :x inc) (update :y dec))
                   (-> current (update :x dec))
                   #_(-> current (update :x dec) (update :y inc))
                   #_(-> current (update :x dec) (update :y dec))
                   (-> current (update :y inc))
                   (-> current (update :y dec))]
        visited-removed (into []
                          (remove #(let [{:keys [y x]} %]
                                     (some? (get-in visited [y x])))
                            neighbors))
        with-properties (mapv
                          #(-> %
                             (map-node map-data)
                             (assoc :distance (-> current :distance inc))
                             (calculate-weight current end)
                             (assoc :from (select-keys current [:x :y])))
                          visited-removed)]
    (into []
      (concat
        (filter
          (fn [node]
            (nil?
              (some
                #(when (same-node? node %)
                   %)
                with-properties)))
          open)
        with-properties))))

(defn construct-route
  [end visited]
  (reverse
    (loop [route-end end
           route []]
      (let [{{:keys [x y]} :from} route-end]
        (if-not (some? (:from route-end))
          route
          (recur
            (get-in visited [y x])
            (conj route route-end)))))))

(defn print-level-route
  ([level route]
   (print-level-route level route []))
  ([level route open-nodes]
   (let [level (loop [level level
                      open-nodes open-nodes]
                 (if (empty? open-nodes)
                         level
                         (let [current-step (first open-nodes)
                               {:keys [x y]} current-step]
                           (recur
                             (assoc level y (str (subs (get level y) 0 x) "0" (subs (get level y) (inc x))))
                             (rest open-nodes)))))
         drawn-route (loop [level level
                            route route]
                       (if (empty? route)
                         level
                         (let [current-step (first route)
                               {:keys [x y]} current-step]
                           (recur
                             (assoc level y (str (subs (get level y) 0 x) "#" (subs (get level y) (inc x))))
                             (rest route)))))]
     (doall (map println drawn-route)))))

(defn find-route
  [start end map-data]
  (let [open-nodes (unvisited-neighbors start {} [] end map-data)
        sorted-nodes (sort-by :route-weight < open-nodes)
        visited (assoc-in {} [(:y start) (:x start)] (assoc start :from nil))
        [next-node & sorted-nodes] sorted-nodes]
    (loop [open-nodes sorted-nodes
             visited visited
             current-node next-node

             steps 0]
        (let [open-nodes (unvisited-neighbors current-node visited open-nodes end map-data)
              sorted-nodes (sort-by :route-weight < open-nodes)
              {:keys [y x]} current-node]
          #_(let [_ (print-level-route tiles (construct-route current-node visited))])
          (if (or (same-node? current-node end)
                (empty? sorted-nodes))
            (construct-route current-node visited)
            (recur
              (rest sorted-nodes)
              (assoc-in visited [y x] current-node)
              (first sorted-nodes)
              (inc steps)))))))



(defn load-level
  "Processes level for pathfinding usage
  Level becomes a {y {x value} hash-map
  0, 0 is at the top of the map"
  [{:keys [tiles] :as map-data}]
  (let [processed-map (process-map tiles)]
    processed-map))

(defn process-targets
  [items]
  (let [potions (filter #(= (:type %) "POTION") items)
        purchase-candidates (filter #(not= (:type %) "POTION") items)]
    {:potions potions
     :purchase-candidates purchase-candidates}))

(defn discounted-price
  [price discount-percent]
  (- price (* price (/ discount-percent 100))))

(defn suitable-target
  [items {:keys [money current-item]}]
  (let [affordable-items (remove #(let [{:keys [price discountPercent]} %]
                                    (> (discounted-price price discountPercent)
                                      money))
                           items)]
    (first
      (sort-by :discountPercent > (conj affordable-items current-item)))))

(defn decide-next-move
  "Depending on the current player state, decide the next move. Pick an item, move towards item,
  move towards potion or move towards exit"
  [{:keys [items player level] :as server-state}]
  (let [{:keys [health position]} player
        low-health? (< health 30)
        {:keys [potions purchase-candidates]} (process-targets items)
        best-target (suitable-target purchase-candidates player)
        move-to (cond
                  ;; no currently affordable items and there are no potions on map -> move to exit
                  (and (nil? best-target)
                    (empty? potions)) (-> level :exit)

                  ;; no currently affordable items and there are potions -> grab a potion
                  (and (nil? best-target)
                    (seq potions)) (first potions)

                  ;; low health but there potions -> grab a potion
                  (and low-health?
                    (seq potions)) (first potions)

                  ;; low health and no potions -> move towards exit
                  (and low-health?
                    (empty? potions)) (-> level :exit)

                  ;; get the best target
                  :else
                  best-target)
        route-to-target (find-route
                          (get-in level [(:y position) (:x position)])
                          move-to
                          level)]))

(defn algo
  []
  (let [bot-name (gensym "shodan-") ;; generate unique bot name
        current-map (load-level (api/game-map))
        initial-server-state (api/register bot-name)
        game-state (generate-game-state
                     initial-state
                     current-map
                     initial-server-state)]
    (loop [state game-state]
      (let [{:keys [alive? last-move]} state
            current-server-state (api/game-state)
            next-move (decide-next-move current-server-state)
            new-state []]
        (if-not alive?
          {}
          (recur new-state))))))

(defn -main
  [& args]
  ;; game-info contains the game map
  (let [game-info (api/register "My cool Clojure bot")]
    (while true
      (Thread/sleep 1000)
      ;; You probably want to get the current game-state from the server before you do your move
      (api/move (:id game-info) (rand-nth ["LEFT" "RIGHT"])))))
