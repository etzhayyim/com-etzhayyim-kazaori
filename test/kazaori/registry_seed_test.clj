(ns kazaori.registry-seed-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def seed-path "registry/agencies.seed.edn")
(def seed (edn/read-string (slurp seed-path)))
(def agencies (get seed "agencies"))
(def allowed-kinds
  #{"disaster-management-agency" "early-warning-system"
    "official-alert-channel" "civilian-relief-coordination"
    "intl-disaster-body"})
(def timestamp-pattern #"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?Z$")

(deftest registry-shape
  (is (map? seed))
  (is (seq agencies))
  (is (pos-int? (get seed "freshnessWindowDays"))))

(deftest agency-identities-are-complete-and-unique
  (let [ids (map #(get % "agencyId") agencies)]
    (is (every? seq ids))
    (is (= (count ids) (count (set ids))))))

(deftest entries-ship-unverified
  (doseq [agency agencies]
    (is (= "unverified-seed" (get agency "verificationStatus"))
        (get agency "agencyId"))))

(deftest sources-and-verification-times-are-present
  (doseq [agency agencies]
    (testing (get agency "agencyId")
      (is (str/starts-with? (get agency "accessUrl" "") "http"))
      (is (str/starts-with? (get agency "provenance" "") "http"))
      (is (re-matches timestamp-pattern (get agency "lastVerified" ""))))))

(deftest registry-is-worldwide
  (is (every? #(seq (get % "jurisdiction")) agencies))
  (is (<= 12 (count (set (map #(get % "jurisdiction") agencies))))))

(deftest agency-taxonomy-is-closed
  (doseq [agency agencies]
    (is (contains? allowed-kinds (get agency "agencyKind"))
        (get agency "agencyId"))))

(deftest notes-reassert-the-civilian-observational-boundary
  (doseq [agency agencies]
    (let [notes (get agency "notes" "")]
      (is (str/includes? notes "CIVILIAN-ONLY") (get agency "agencyId"))
      (is (str/includes? notes "OBSERVATIONAL") (get agency "agencyId"))))
  (let [text (slurp seed-path)]
    (is (str/includes? text "CIVILIAN-ONLY"))
    (is (str/includes? text "ADR-2605263200"))))
