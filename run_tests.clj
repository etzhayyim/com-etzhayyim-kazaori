(require '[clojure.test :as t])

(doseq [ns-sym '[kazaori.methods.test-emergency
                  kazaori.methods.test-charter-gates
                  kazaori.registry-seed-test
                  kazaori.murakumo-test
                  kazaori.repository-contract-test]]
  (require ns-sym))

(let [result (apply t/run-tests
                    '[kazaori.methods.test-emergency
                      kazaori.methods.test-charter-gates
                      kazaori.registry-seed-test
                      kazaori.murakumo-test
                      kazaori.repository-contract-test])]
  (System/exit (if (zero? (+ (:fail result) (:error result))) 0 1)))
