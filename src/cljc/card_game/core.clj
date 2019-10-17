(ns card-game.core
  (:require
   [plumbing.graph :as graph]
   [plumbing.core :refer [fnk]]))

(defn deterministic-shuffle
  [^Long seed, ^java.util.Collection coll]
  (let [al (java.util.ArrayList. coll)
        rng (java.util.Random. seed)]
    (java.util.Collections/shuffle al rng)
    (clojure.lang.RT/vector (.toArray al))))

(def player-move?
  #{:event.type/discard
    :event.type/skip
    :event.type/not-player-card
    :event.type/not-player-turn})

(def valid-player-move?
  #{:event.type/discard
    :event.type/skip})

;; validation & rules

(defn current-player-move
  [state move]
  (when (not= (:current-player state)
              (:player/id move))
    (assoc move :event/type :event.type/not-player-turn)))

(defn player-card
  [state move]
  (let [player-hand? (get (:hands state) (:player/id move))]
    (when-not (player-hand? (:card/number move))
      (assoc move :event/type :event.type/not-player-card))))

(defn check
  [state criterias move]
  (or
   (reduce (fn [m check-fn]
             (or m (check-fn state move)))
           nil
           criterias)
   move))

;; API

(defn card-distribution
  [{:game/keys [id
                players-count
                round
                numbers] :as game}]
  (let [shuffle (->> (range 1 numbers)
                     (deterministic-shuffle id)
                     (take (* players-count round)))]
    (update game :game/events into
            (mapv
             (fn [card-number played-id]
               {:player/id played-id
                :card/number card-number
                :event/type :event.type/card-received})
             shuffle
             (cycle (range players-count))))))

(defn play
  [{:game/keys [state] :as game} player-id card-number]
  (update game :game/events conj
          (check state
                 [current-player-move
                  player-card]
                 {:player/id player-id
                  :card/number card-number
                  :event/type :event.type/discard})))

(defn skip
  [{:game/keys [state] :as game} player-id]
  (update game :game/events conj
          (check state
                 [current-player-move]
                 {:player/id player-id
                  :event/type :event.type/skip})))

;; Aggregators

(def state-reducer
  {:hands
   (fnk [game/players-count game/events]
     (reduce
      (fn [hands event]
        (case (:event/type event)
          :event.type/card-received
          (update hands (:player/id event) conj (:card/number event))

          :event.type/discard
          (update hands (:player/id event) disj (:card/number event))

          hands))
      (into {} (for [player-id (range players-count)]
                 [player-id (sorted-set)]))
      events))

   :discarded-sequence
   (fnk [game/events]
     (->> (filter #(= :event.type/discard (:event/type %)) events)
          (mapv :card/number)))

   :lost-lives
   (fnk [discarded-sequence]
     (:lives
      (reduce
       (fn [{:keys [lives previous] :as state} current]
         (if (> current previous)
           (assoc state :previous current)
           (-> (assoc state :previous current)
               (update :lives inc))))
       {:lives 0
        :previous 0}
       discarded-sequence)))

   :last-valid-player-move
   (fnk [game/events]
     (when-some [last-event (first
                             (filter
                              (comp valid-player-move? :event/type)
                              (rseq events)))]
       last-event))

   :current-player
   (fnk [game/players-count last-valid-player-move]
     (if last-valid-player-move
       (let [player-id (:player/id last-valid-player-move)]
         (if (>= (inc player-id) players-count)
           0
           (inc player-id)))
       0))

   :end-game?
   (fnk [hands]
     (every? empty? (vals hands)))})

(def compute-state* (graph/compile state-reducer))
(defn compute-state
  [game]
  (assoc game :game/state
         (into {} (compute-state* game))))

(comment

  (def s
    (-> #:game{:id 1000
               :players-count 3
               :round 2
               :numbers 100
               :events []}
        (card-distribution)
        (compute-state)))

  (-> s
      (skip 0) (compute-state)
      (play 0 45) (compute-state)
      (skip 1) (compute-state)
      (play 2 19) (compute-state)
      (skip 0) (compute-state)
      (skip 1) (compute-state)
      (skip 2) (compute-state)
      (play 0 45) (compute-state)
      (skip 1) (compute-state)
      (skip 2) (compute-state)
      (play 0 79) (compute-state)
      (play 1 37) (compute-state)
      (play 2 7) (compute-state)
      (skip 0) (compute-state)
      (play 1 26) (compute-state))

  )
