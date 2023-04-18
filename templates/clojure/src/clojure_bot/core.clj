(ns clojure-bot.core
  (:require [clojure-bot.api :as api]
            [clojure-bot.log :as log]
            [clojure.math :as math]
            [clojure.string :as str])
  (:gen-class))

(def initial-state {})

(defn generate-game-state
  [initial-state initial-fsm map-data server-state]
  (let [{:keys [player id]} server-state]
    (log/log :generate-game-state player id)
    (assoc initial-state
      :level (assoc map-data :exit (get-in server-state [:map :exit]))
      :player {:server player
               :local (assoc player
                        :behavior initial-fsm
                        :id id
                        :alive? true)})))

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
                {:x tile-index
                 :y row-index
                 :weight (case character
                           \x 999
                           \_ 1
                           0)}])
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
  (log/log :map-node position (get-in map-data [(:y position) (:x position)]))
  (let [{:keys [x y]} position]
    (get-in map-data [y x])))

(defn calculate-weight
  [current from end]
  #_(try)
  #_(println current from end)
  (assoc current :route-weight (+ (:weight current) (:weight from) (:distance current) (heuristic current end)))
  #_(catch Exception e (log/log :calculate-weight-exception current from end)))

(defn same-node?
  [{start-x :x start-y :y :as _current} {end-x :x end-y :y :as _end}]
  (and (= start-x end-x)
    (= start-y end-y)))

(defn unvisited-neighbors
  [current visited open end map-data]
  #_(try)
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
                             (assoc :distance (inc (get current :distance 0)))
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
        with-properties)))
  #_(catch Exception e (log/log :unvisited-neighbors current end)))

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
  (log/log :find-route start end)
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

(defn affordable-items
  [items money]
  (log/log :-> money)
  (remove #(let [{:keys [price discountPercent]} %]
             (> (discounted-price price discountPercent)
               money))
    items))

(defn suitable-target
  [items {:keys [money current-item] :as _player}]
  (let [affordable-items (affordable-items items money)]
    (log/log :suitable-target #_#_affordable-items current-item (keep identity (conj affordable-items current-item)))
    (first
      (sort-by :discountPercent > (keep identity (conj affordable-items current-item))))))

(def move-matrix {[0 1] "DOWN"
                  [0 -1] "UP"
                  [1 0] "RIGHT"
                  [-1 0] "LEFT"})

(defn get-move-based-on-route
  [route position]
  (let [{px :x py :y} position
        {rx :x ry :y} (first route)]
    (get move-matrix [(- rx px) (- ry py)])))

(defn get-next-move
  [{:keys [player level]
    :as game-state} target]
  (log/log :get-next-move player target)
  (let [position-local (get-in player [:local :position])
        position-server (get-in player [:server :position])
        _ (log/log :get-next-move position-local position-server)
        route-to-target (find-route
                          (get-in level [(:y position-local) (:x position-local)])
                          (:position target)
                          level)
        next-move (get-move-based-on-route route-to-target position-local)
        ;; if position at server is different from local, we have moved and can send a next move
        queue? true #_(or
                 (not= (:x position-local) (:x position-server))
                 (not= (:y position-local) (:y position-server)))]
    (cond-> game-state
      true (assoc-in [:player :local :current-target] target)
      queue? (assoc-in [:player :local :position] position-server)
      queue? (assoc-in [:action-queue] next-move))))

(defn move-to-closest-affordable-item
  [game-state]
  (let [player (get-in game-state [:player :local])
        {:keys [purchase-candidates]} (process-targets (get-in game-state [:items]))
        best-target (suitable-target purchase-candidates player)]
    (log/log :move-to-closest-affordable-item best-target)
    (get-next-move game-state best-target)))

(defn move-to-closest-potion
  [{:keys [player]
    :as game-state}]
  (let [{:keys [potions]} (process-targets (get-in game-state [:items]))
        position-local (get-in player [:local :position])
        potions-by-distance (map #(assoc % :estimated-distance (heuristic position-local (:position %))) potions)
        best-target (first (sort-by :estimated-distance potions-by-distance))]
    (log/log :move-to-closest-potion :best-target best-target)
    (log/log :move-to-closest-potion :potions-by-distance potions-by-distance)
    (log/log :move-to-closest-potion :position-local position-local)
    (get-next-move game-state best-target)))

(defn move-to-exit
  [{:keys [level] :as game-state}]
  (let [best-target (get-in level [:exit])]
    (get-next-move game-state best-target)))

(defn pick-item
  [game-state]
  (assoc-in game-state [:action-queue] "PICK"))

;; DATA ORIENTED DESCRIPTION
;; TODO: Dispatch could be an effect

(def effects {::no-op identity
              ::move-to-closest-affordable-item move-to-closest-affordable-item
              ::move-to-closest-potion move-to-closest-potion
              ::move-to-exit move-to-exit
              ::pick-item pick-item
              ::initialize (fn [game-state]
                             (assoc-in game-state [:player :local :initialized?] true))
              ::dead (fn [game-state]
                       (assoc-in game-state [:player :local :alive?] false))})

(def evaluations {::low-health (fn [game-state]
                                 (let [health (get-in game-state [:player :server :health])]
                                   (log/log :low-health? health)
                                   (when health
                                     (< health 70))))
                  ::no-potions (fn [game-state]
                                 (let [{:keys [potions]} (process-targets
                                                           (get-in game-state [:items]))]
                                   (empty? potions)))
                  ::on-item (fn [game-state]
                              (let [{target-position :position} (get-in game-state [:player :local :current-target])
                                    server-position (get-in game-state [:player :server :position])]

                                (log/log :on-item? server-position (same-node? server-position target-position))
                                (same-node? target-position server-position)))
                  ::enough-money (fn [game-state]
                                   (let [{:keys [price discountPercent]} (get-in game-state [:player :local :current-target])
                                         money (get-in game-state [:player :server :money])]
                                     (log/log :enough-money? money price discountPercent)
                                     (when (and price discountPercent)
                                       (> money (discounted-price price discountPercent)))))
                  ::affordable-items (fn [game-state]
                                       (let [{:keys [purchase-candidates]} (process-targets
                                                                             (get-in game-state [:items]))]
                                         (and
                                           (seq purchase-candidates)
                                           (seq (affordable-items purchase-candidates (get-in game-state [:player :server :money]))))))
                  ::no-affordable-items (fn [game-state]
                                          (let [{:keys [purchase-candidates]} (process-targets
                                                                                (get-in game-state [:items]))]
                                            (and
                                              (seq purchase-candidates)
                                              (nil? (seq (affordable-items purchase-candidates (get-in game-state [:player :server :money])))))))
                  ::on-health (fn [game-state]
                                (let [current-target (get-in game-state [:player :local :current-target :position])
                                      server-position (get-in game-state [:player :server :position])]
                                  (log/log :on-health? server-position current-target)
                                  (same-node? current-target server-position)))
                  ::item-picked (fn [game-state]
                                  (let [items (get-in game-state [:items])
                                        current-target (get-in game-state [:player :local :current-target :position])]
                                    (nil? (some #(when (same-node? current-target (:position %)) %) items))))
                  ::dead (fn [game-state]
                           (let [server-instance (get-in game-state [:player :server])
                                 alive? (get-in game-state [:player :local :alive?])]
                             (log/log :dead? server-instance (nil? server-instance))
                             (and
                               alive?
                               (nil? server-instance))))
                  ::initialized (fn [game-state]
                                  (log/log :initialized? (get-in game-state [:player :local :initialized?]))
                                  (true? (get-in game-state [:player :local :initialized?])))
                  ::instance-found (fn [game-state]
                                     (some? (get-in game-state [:player :server])))
                  ::potions-available (fn [game-state]
                                        (let [{:keys [potions]} (process-targets
                                                                  (get-in game-state [:items]))]
                                          (seq potions)))})

(def bot-logic-state
  {:pre {:transitions [{:when [::dead ::initialized]
                        :switch :dead}]}
   :current {:state :waiting-for-init
             :effect ::no-op}
   :last {:state nil}
   :states {:dead {:effect ::dead
                   :transitions []}
            :waiting-for-init {:effect ::no-op
                               :transitions [{:when [::instance-found]
                                              :switch :idle
                                              :post-effect ::initialize}]}
            :idle {:effect ::no-op
                   :transitions [{:when [::low-health ::no-potions]
                                  :switch :move-to-exit}
                                 {:when [::low-health ::potions-available]
                                  :switch :move-to-health}
                                 {:when [::affordable-items]
                                  :switch :move-to-item}]}
            :move-to-item {:effect ::move-to-closest-affordable-item
                           :transitions [{:when [::low-health ::potions-available]
                                          :switch :move-to-health}
                                         {:when [::low-health ::no-potions]
                                          :switch :move-to-exit}
                                         {:when [::on-item ::enough-money]
                                          :switch :pick-item}]}
            :move-to-exit {:effect ::move-to-exit
                           :transitions [{:when [::affordable-items]
                                          :switch :move-to-item}]}
            :move-to-health {:effect ::move-to-closest-potion
                             :transitions [{:when [::on-health]
                                            :switch :pick-item}]}
            :pick-item {:effect ::pick-item
                        :transitions [{:when [::item-picked ::no-affordable-items]
                                       :switch :move-to-exit}
                                      {:when [::item-picked ::affordable-items]
                                       :switch :idle}
                                      {:when [::item-picked ::potions-available]
                                       :switch :idle}]}}})

(defn when-with-juxt
  "TODO: some kind of indication if a evaluation is not found"
  [fn-ids]
  (let [fns (map #(comp boolean (get evaluations %)) fn-ids)]
    (apply juxt fns)))

(defn juxtapose
  [transitions]
  (mapv #(update % :when when-with-juxt)
    transitions))

(defn transitions-with-juxtapositions
  "Creates juxtapositions function for each state transition

  Takes evaluations per the keywords given in transitions :when -keyword
  and applies juxt"
  [[state-id state-properties]]
  [state-id (update state-properties
              :transitions
              juxtapose)])

(defn update-fsm-states
  [states]
  (into {}
    (map transitions-with-juxtapositions)
    states))

(defn process-fsm
  [fsm-initial]
  (-> fsm-initial
    (update-in [:pre :transitions] juxtapose)
    (update :states update-fsm-states)))

(defn update-entity-behaviors
  "Takes a FSM and a signal (a state object)

  Runs through transitions for current FSM state on the signal and returns a new FSM state"
  [fsm state]
  #_(log/log :update-entity-behaviors fsm)
  (let [{:keys [current states pre]} fsm
        current-state-id (:state current)
        _ (log/log :update-entity-behaviors current-state-id)
        pre-transitions (get-in pre [:transitions])
        transitions (into [] (concat pre-transitions (get-in states [current-state-id :transitions])))
        transition (loop [{when-fn :when :as transition} (first transitions)
                          transitions (rest transitions)
                          state state]
                     #_(let [])
                     (log/log :transitioning transition)
                     (if (or (nil? transition)
                           (every? true? (when-fn state)))
                       transition
                       (recur (first transitions) (rest transitions) state)))]
    (if (some? transition)
      (do
        (log/log :update-entity-behaviors-> current-state-id :-> (:switch transition))
        (-> fsm
          (assoc-in [:current :state] (:switch transition))
          (assoc-in [:current :effect] (get-in fsm [:states (:switch transition) :effect]))
          (assoc-in [:current :post-effect] (:post-effect transition))))
      fsm)))

(defn apply-behavior
  [state]
  (let [effect-id (get-in state [:player :local :behavior :current :effect])
        effect (get effects effect-id )]
    (log/log :apply-behavior effect-id)
    (effect state)))

(defn apply-post-effect
  [state]
  (let [effect-id (get-in state [:player :local :behavior :current :post-effect])]
    (if effect-id
      (let [effect (get effects effect-id)]
        (log/log :apply-post-effect effect-id)
        (-> state
          (effect)
          (update-in [:player :local :behavior :current] dissoc :post-effect)))
      state)))

(defn decide-next-move
  "Depending on the current player state, decide the next move. Pick an item, move towards item,
  move towards potion or move towards exit"
  [state]
  (-> state
    (update-in [:player :local :behavior] update-entity-behaviors state)
    (apply-post-effect)))

(defn update-game-state-with-latest-server-state
  [state {:keys [players items] :as _server-state}]
  (let [local-name (get-in state [:player :local :name])
        server-instance (some #(when (= (:name %) local-name) %) players)]
    (log/log :update-game-state-with-latest-server-state players local-name server-instance)
    (-> state
      (assoc :items items)
      (assoc-in [:player :server] server-instance) )))

(defn dispatch-move
  "Fires a side-effect into the API if necessary"
  [{:keys [action-queue] :as state}]
  (log/log :dispatch-move action-queue)
  (let [id (get-in state [:player :local :id])]
    (or
      (when action-queue
        (api/move id action-queue)
        (dissoc state :action-queue))
      state)))

(defn algo
  []
  (let [bot-name (str (gensym "shodan-")) ;; generate unique bot name
        current-map (load-level (api/game-map))
        initial-server-state (api/register bot-name)
        logic-fsm (process-fsm bot-logic-state)
        game-state (generate-game-state
                     initial-state
                     logic-fsm
                     current-map
                     initial-server-state)]
    (loop [state game-state]
      (let [{:keys [player]} state
            {{:keys [alive?]} :local} player
            current-server-state (api/game-state)
            new-state (-> state
                        (update-game-state-with-latest-server-state current-server-state)
                        (decide-next-move)
                        (apply-behavior)
                        (dispatch-move))]
        (if-not alive?
          {}
          (recur new-state))))))

(comment )

(defn -main
  [& args]
  ;; game-info contains the game map
  (let [game-info (api/register "My cool Clojure bot")]
    (while true
      (Thread/sleep 1000)
      ;; You probably want to get the current game-state from the server before you do your move
      (api/move (:id game-info) (rand-nth ["LEFT" "RIGHT"])))))
