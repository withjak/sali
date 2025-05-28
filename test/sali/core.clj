(ns sali.core
  (:require [clojure.test :refer :all]
            [sali.core :as s]))

(deftest function-as-schema-test
  (is (= (s/validate 1 number?)
         true))
  (is (= (s/validate :1 number?)
         [{:data :1 :message "Validation failed" :path []}])))

(deftest empty-collection-test
  (is (= (s/validate {} [:map])
         true))
  (is (= (s/validate [] [:vector])
         true))
  (is (= (s/validate #{} [:set])
         true))

  (is (= (s/validate 1 [:map])
         [{:data 1 :message "Expected map." :path []}]))
  (is (= (s/validate 1 [:vector])
         [{:data 1 :message "Expected vector." :path []}]))
  (is (= (s/validate 1 [:set])
         [{:data 1 :message "Expected set." :path []}])))

(deftest map-test
  ; empty map
  (is (= (s/validate 1 [:map])
         [{:data 1 :message "Expected map." :path []}]))

  ; map with keys
  (is (= (s/validate {:a 1} [:map [:a number?]])
         true))
  (is (= (s/validate {:a 1} [:map [:a {} number?]])
         true))
  (is (= (s/validate {:a 1} [:map [:a {:required true} number?]])
         true))
  (is (= (s/validate {:a 1} [:map [:a {:required false} number?]])
         true))
  (is (= (s/validate {} [:map [:a {:required false} number?]])
         true))

  ; negatives
  (let [error '({:data :sali.core/not-found :message ":a is required." :path []})]
    (is (= (s/validate {} [:map [:a number?]])
           error))
    (is (= (s/validate {} [:map [:a {} number?]])
           error))
    (is (= (s/validate {} [:map [:a {:required true} number?]])
           error)))

  (is (= (s/validate {:a :1} [:map [:a {:required false} number?]])
         '({:data :1 :message "Validation failed" :path [:a]})))

  ; map opts
  (is (= (s/validate {:a 1} [:map {}
                             [:a number?]])
         true))
  (is (= (s/validate {:a 1} [:map {:required true}
                             [:a number?]])
         true))
  (is (= (s/validate {:a 1} [:map {:required false}
                             [:a number?]])
         true))
  (is (= (s/validate {} [:map {:required false}
                         [:a number?]])
         true))

  ; key opts overwrite map opts
  (is (= (s/validate {} [:map {:required false}
                         [:a {:required true} number?]])
         '({:data :sali.core/not-found :message ":a is required." :path []})))

  ; dependent key
  (let [req? (fn [_key a]
               (pos? a))
        schema [:map
                [:a number?]
                [:b {:required [:a req?]} number?]]]
    ; b is required
    (is (= (s/validate {:a 1 :b 2} schema)
           true))
    ; b is required
    (is (= (s/validate {:a 1} schema)
           '({:data :sali.core/not-found :message ":b is required." :path []})))
    ; b is not required, but present and matches schema
    (is (= (s/validate {:a 1 :b 2} schema)
           true))
    ; b is not required, but present and do not match schema
    (is (= (s/validate {:a 1 :b :keyword} schema)
           '({:data :keyword :message "Validation failed" :path [:b]})))))

(deftest vector-test
  ; empty vector
  (is (= (s/validate [] [:vector])
         true))

  (is (= (s/validate [1 2] [:vector number?])
         true))
  (is (= (s/validate [1 :2 :3] [:vector number? keyword?])
         true))
  (is (= (s/validate [1 :2 "3"] [:vector number? keyword? string?])
         true))
  (is (= (s/validate [1 :2] [:vector number? keyword? string?])
         true))

  ; negatives
  (is (= (s/validate [1 :2] [:vector number?])
         '({:data :2 :message "Validation failed" :path [1]})))
  (is (= (s/validate [1 :2 3] [:vector number? keyword?])
         '({:data 3 :message "Validation failed" :path [2]})))

  ; opts
  (is (= (s/validate [1 2 3] [:vector {:f #(= (count %) 3)} number?])
         true))
  (is (= (s/validate [1 2] [:vector {:f #(= (count %) 3)} number?])
         '({:data [1 2] :message "Opts {:f ...} did not satisfy" :path []}))))

(deftest clojure-set-test
  (is (= (s/validate #{1 2} [:set number?])
         true))
  (is (= (s/validate #{1 :2} [:set number?])
         '({:data :2 :message "Validation failed" :path []})))

  (is (= (s/validate #{1 2 3} [:set {:f #(= (count %) 3)} number?])
         true))
  (is (= (s/validate #{1 2} [:set {:f #(= (count %) 3)} number?])
         '({:data #{1 2} :message "Opts {:f ...} did not satisfy" :path []}))))

(deftest and-test
  ; i think this should be false
  (is (= (s/validate 1 [:and])
         true))

  (is (= (s/validate 1 [:and number? odd?])
         true))

  (is (= (s/validate 2 [:and number? odd?])
         '({:data 2 :message "Validation failed" :path []})))

  (is (= (s/validate :2 [:and number? odd?])
         '({:data :2 :message "Validation failed" :path []}
           {:data :2 :message "Predicate exception. Exception: Argument must be an integer: :2" :path []}))))

(deftest or-test
  ; i think this should be false
  (is (= (s/validate 1 [:or])
         true))

  (is (= (s/validate 1 [:or number? keyword?])
         true))
  (is (= (s/validate :1 [:or number? keyword?])
         true))
  (is (= (s/validate "1" [:or number? keyword?])
         '({:data "1" :message "Validation failed" :path []}
           {:data "1" :message "Validation failed" :path []}))))

(deftest schema-validation-test
  (is (= (s/validate (fn [_]) fn?)
         true))

  (is (= (s/validate [1 2] [:vector])
         true))
  (is (= (s/validate {:a 1 :b 2} [:map])
         true))

  (is (= (s/validate [] [:vector])
         true)))

(deftest resolve-relative-path-test
  (let [base-path [:a :b 0 2 :love]
        paths [:my-love
               [:my-love]
               [:. 3]
               [:... :hmm]]]
    (is (= (mapv #(s/resolve-relative-path base-path %) paths)
           [[:a :b 0 2 :my-love]
            [:a :b 0 2 :my-love]
            [:a :b 0 3]
            [:a :hmm]]))))
