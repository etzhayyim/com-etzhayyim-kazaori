(ns kazaori.murakumo
  "Pure cljc actor boundary generated from manifest migration scaffold."
  (:require [clojure.string :as str]))

(def actor-did
  "did:web:kazaori.etzhayyim.com")

(def common-gates
  [:council-charter-attestation
   :no-platform-held-key-baseline
   :no-probing-baseline
   :murakumo-only-inference-baseline
   :did-primary-baseline
   :append-only-gate-baseline
   :kotoba-only-substrate-baseline])

(defn collection
  [name]
  (str "com.etzhayyim.kazaori." name))

(def cell-specs {
  :emergency_declaration {:legacy-cell "emergency-declaration"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "emergency_declaration")]
     :required-gates common-gates
     :trigger "manifest cell emergency_declaration"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :damage_assessment {:legacy-cell "damage-assessment"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "damage_assessment")]
     :required-gates common-gates
     :trigger "manifest cell damage_assessment"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :emergency_water_supply {:legacy-cell "emergency-water-supply"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "emergency_water_supply")]
     :required-gates common-gates
     :trigger "manifest cell emergency_water_supply"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :emergency_food_supply {:legacy-cell "emergency-food-supply"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "emergency_food_supply")]
     :required-gates common-gates
     :trigger "manifest cell emergency_food_supply"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :mass_evacuation {:legacy-cell "mass-evacuation"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "mass_evacuation")]
     :required-gates common-gates
     :trigger "manifest cell mass_evacuation"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :medical_surge {:legacy-cell "medical-surge"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "medical_surge")]
     :required-gates common-gates
     :trigger "manifest cell medical_surge"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
})

(defn safe-rkey
  [s]
  (let [clean (-> (str s)
                  (str/replace #"^did:web:" "")
                  (str/replace #"[^A-Za-z0-9._~-]" "-"))]
    (if (str/blank? clean) "unknown" clean)))

(defn gate-value
  [attestations gate]
  (or (get attestations gate)
      (get attestations (name gate))
      (when (set? attestations) (attestations gate))
      (when (set? attestations) (attestations (name gate)))))

(defn missing-gates
  [spec attestations]
  (->> (:required-gates spec)
       (remove #(boolean (gate-value attestations %)))
       vec))

(defn put-record-effect
  [collection rkey record]
  {:op :mst/put-record
   :actor actor-did
   :collection collection
   :rkey rkey
   :record record})

(defn records-for
  [spec {:keys [records record computed-at request-id]
         :as input}]
  (let [input-records (cond
                        (map? records) records
                        (some? record) {0 record}
                        :else {})
        base {:actorDid actor-did
              :computedAt computed-at
              :legacyCell (:legacy-cell spec)
              :phase (:phase spec)
              :requestId request-id
              :actorBoundary "cljc-migration-scaffold"
              :scaffold true
              :constitutionalStatus "attested-plan"}]
    (map-indexed
     (fn [idx coll]
       (let [record* (merge {:$type coll}
                            base
                            (or (get input-records coll)
                                (get input-records idx)
                                {}))
             rkey (safe-rkey (or (:rkey record*)
                                 (get record* "rkey")
                                 (:tid record*)
                                 request-id
                                 (str (:legacy-cell spec) "-" idx)))]
         {:collection coll
          :record record*
          :rkey rkey}))
     (:collections spec))))

(defn cell-plan
  [cell-key {:keys [attestations] :as input}]
  (let [spec (get cell-specs cell-key)]
    (when-not spec
      (throw (ex-info "unknown cell" {:cell cell-key})))
    (let [missing (missing-gates spec attestations)]
      (merge
       {:cell cell-key
        :legacy-cell (:legacy-cell spec)
        :actor actor-did
        :phase (:phase spec)
        :murakumo-node (:murakumo-node spec)
        :trigger (:trigger spec)
        :ceiling (:ceiling spec)
        :required-gates (:required-gates spec)
        :missing-gates missing}
       (if (seq missing)
         {:status :blocked
          :effects []}
         (let [planned-records (records-for spec input)]
           {:status :ready
            :records (vec planned-records)
            :effects (mapv (fn [{:keys [collection record rkey]}]
                             (put-record-effect collection rkey record))
                           planned-records)}))))))

(defn all-cell-plans
  [input]
  (into {}
        (map (fn [cell-key] [cell-key (cell-plan cell-key input)]))
        (keys cell-specs)))
