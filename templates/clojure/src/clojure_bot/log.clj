(ns clojure-bot.log)

(def enabled-logs #{:generate-game-state
                    :update-entity-behaviors->
;                    :apply-post-effect
                    ;:get-next-move
                    ;:on-health?
                    ;:dispatch-move
;                    :low-health?
;                    :transitioning
;                    :move-to-closest-potion
                                        ;                    :update-game-state-with-latest-server-state
                    :on-item?
                    :enough-money?
                    })

(defn log
  [logger & params]
  (when (or (logger enabled-logs) (:all enabled-logs))
    (apply prn logger params)))
