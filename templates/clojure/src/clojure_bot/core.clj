(ns clojure-bot.core
  (:require [clojure-bot.api :as api]
            [clojure-bot.log :as log]
            [clojure-bot.behavior :as behavior]
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
  "Reads the characters from the string-based map and translates them to weight values"
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

(defn load-level
  "Processes level for pathfinding usage
  Level becomes a {y {x value} hash-map
  0, 0 is at the top of the map"
  [{:keys [tiles] :as _map-data}]
  (let [processed-map (process-map tiles)]
    processed-map))

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
        logic-fsm (behavior/process-fsm behavior/bot-logic-state)
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
                        (behavior/decide-next-move)
                        (behavior/apply-behavior)
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
