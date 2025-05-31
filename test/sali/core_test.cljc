(ns sali.core-test
  (:require [clojure.test :refer :all]
            [sali.core :as s]))

; [cljd.test :refer :all]

(deftest function-as-schema-test
  (is (= (s/validate number? 1)
         true))
  (is (= (->> (s/validate number? :1) (map #(dissoc % :type)))
         [{:data :1 :message "Validation failed" :path []}])))


(deftest map-test
  ; empty map
  (is (= (->> (s/validate [:map identity] 1)
              (map #(dissoc % :type)))
         [{:data 1 :message "Expected map." :path []}]))

  ; map with keys
  (is (= (s/validate [:map [:a number?]] {:a 1})
         true))
  (is (= (s/validate [:map [:a {} number?]] {:a 1})
         true))
  (is (= (s/validate [:map [:a {:required true} number?]] {:a 1})
         true))
  (is (= (s/validate [:map [:a {:required false} number?]] {:a 1})
         true))
  (is (= (s/validate [:map [:a {:required false} number?]] {})
         true))

  ; negatives
  (let [error '({:data :sali.core/not-found :message ":a is required." :path []})]
    (is (= (s/validate [:map [:a number?]] {})
           error))
    (is (= (s/validate [:map [:a {} number?]] {})
           error))
    (is (= (s/validate [:map [:a {:required true} number?]] {})
           error)))

  (is (= (->> (s/validate [:map [:a {:required false} number?]] {:a :1})
              (map #(dissoc % :type)))
         '({:data :1 :message "Validation failed" :path [:a]})))

  ; map opts
  (is (= (s/validate [:map {}
                      [:a number?]]
                     {:a 1})
         true))
  (is (= (s/validate [:map {:required true}
                      [:a number?]]
                     {:a 1})
         true))
  (is (= (s/validate [:map {:required false}
                      [:a number?]]
                     {:a 1})
         true))
  (is (= (s/validate [:map {:required false}
                      [:a number?]]
                     {})
         true))

  ; key opts overwrite map opts
  (is (= (s/validate [:map {:required false}
                      [:a {:required true} number?]]
                     {})
         '({:data :sali.core/not-found :message ":a is required." :path []})))

  ; dependent key
  (let [req? (fn [_key a]
               (pos? a))
        schema [:map
                [:a number?]
                [:b {:required {:depends-on :a :fn req?}} number?]]]
    ; b is required
    (is (= (s/validate schema {:a 1 :b 2})
           true))
    ; b is required
    (is (= (s/validate schema {:a 1})
           '({:data :sali.core/not-found :message ":b is required." :path []})))
    ; b is not required, but present and matches schema
    (is (= (s/validate schema {:a 1 :b 2})
           true))
    ; b is not required, but present and do not match schema
    (is (= (->> (s/validate schema {:a 1 :b :keyword})
                (map #(dissoc % :type)))
           '({:data :keyword :message "Validation failed" :path [:b]})))))

(deftest vector-test
  ; empty vector
  (is (= (s/validate vector? [])
         true))

  (is (= (->> (s/validate [:vector] [:abc])
              (map #(dissoc % :type)))
         '({:data [:abc] :message "schema must have a body." :path []})))

  (is (= (s/validate [:vector number?] [1 2])
         true))
  (is (= (s/validate [:vector number? keyword?] [1 :2 :3])
         true))
  (is (= (s/validate [:vector number? keyword? string?] [1 :2 "3"])
         true))
  (is (= (s/validate [:vector number? keyword? string?] [1 :2])
         true))

  ; negatives
  (is (= (->> (s/validate [:vector number?] [1 :2])
              (map #(dissoc % :type)))
         '({:data :2 :message "Validation failed" :path [1]})))
  (is (= (->> (s/validate [:vector number? keyword?] [1 :2 3])
              (map #(dissoc % :type)))
         '({:data 3 :message "Validation failed" :path [2]})))

  ; opts
  (is (= (s/validate [:vector {:f #(= (count %) 3)} number?] [1 2 3])
         true))
  (is (= (->> (s/validate [:vector {:fn #(= (count %) 3)} number?] [1 2])
              (map #(dissoc % :type)))
         '({:data [1 2] :message "Opts {:fn ...} did not satisfy" :path []}))))

(deftest clojure-set-test
  (is (= (s/validate [:set number?] #{1 2})
         true))
  (is (= (->> (s/validate [:set number?] #{1 :2})
              (map #(dissoc % :type)))
         '({:data :2 :message "Validation failed" :path []})))

  (is (= (s/validate [:set {:fn #(= (count %) 3)} number?] #{1 2 3})
         true))
  (is (= (->> (s/validate [:set {:fn #(= (count %) 3)} number?] #{1 2})
              (map #(dissoc % :type)))
         '({:data #{1 2} :message "Opts {:fn ...} did not satisfy" :path []}))))

(deftest and-test
  ; i think this should be false
  (is (= (->> (s/validate [:and] 1)
              (map #(dissoc % :type)))
         [{:data 1 :message "schema must have a body." :path []}]))

  (is (= (s/validate [:and number? odd?] 1)
         true))

  (is (= (->> (s/validate [:and number? odd?] 2)
              (map #(dissoc % :type)))
         '({:data 2 :message "Validation failed" :path []})))

  (is (= (->> (s/validate [:and number? odd?] :2)
              (map #(dissoc % :type)))
         '({:data :2 :message "Validation failed" :path []}
           {:data :2 :message "Predicate exception. Exception: Argument must be an integer: :2" :path []}))))

(deftest or-test
  ; i think this should be false
  (is (= (->> (s/validate [:or] 1)
              (map #(dissoc % :type)))
         '({:data    1
            :message "schema must have a body."
            :path    []})))

  (is (= (s/validate [:or number? keyword?] 1)
         true))
  (is (= (s/validate [:or number? keyword?] :1)
         true))
  (is (= (->> (s/validate [:or number? keyword?] "1")
              (map #(dissoc % :type)))
         '({:data "1" :message "Validation failed" :path []}
           {:data "1" :message "Validation failed" :path []}))))

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

(comment

  (defn foo [& _])
  (defn is=
    [thing]
    (fn [value]
      (= thing value)))

  (defn count=
    [c]
    (fn [collection]
      (= c (count collection))))

  (def required
    [:or
     [:and map? empty?]
     [:map [:required boolean?]]
     [:map [:required
            [:map
             [:depends-on keyword?]
             [:fn fn?]]]]
     [:map [:required
            [:map
             [:depends-on [:vector
                           [:or
                            keyword?
                            [:vector [:or
                                      keyword?
                                      string?
                                      integer?]]]]]
             [:fn fn?]]]]])

  (declare schema-schema)

  (do
    (def schema-schema
      [:or
       fn?

       ; Note: here schema-schema will be repeated as many times as needed
       ; [:vector string?]
       ; [:vector string? keyword? ...]
       [:vector
        (is= :vector)
        schema-schema]
       ; [:vector {} string? ...]
       ; [:vector {:fn #(every? pos? %)} number? ...]
       [:vector
        (is= :vector)
        [:or
         [:and map? empty?]
         [:map [:fn fn?]]]
        schema-schema]


       ; [:map
       ;    [:age number?]
       ;    [:name string?]
       ;    ...]
       [:vector
        (is= :map)
        [:vector {:fn (count= 2)}
         keyword? schema-schema]]
       ; [:map
       ;    [:age {} number?]
       ;    [:name {} string?]
       ;    ...]
       ; [:map
       ;    [:age {:required true} number?]
       ;    [:name {:required false} string?]
       ;    ...]
       ; [:map
       ;    [:age {:required {:depends-on :some-key :fn some-fn}} number?]
       ;    ...]
       ; [:map
       ;    [:age {:required {:depends-on [:some-key [:.. :other-key]] :fn some-fn}} number?]
       ;    ...]
       [:vector
        (is= :map)
        [:vector {:fn (count= 3)}
         keyword? required schema-schema]]

       ; [:map {}
       ;    [:age number?]
       ;    [:name string?]
       ;    ...]
       [:vector
        (is= :map)
        required
        [:vector {:fn (count= 2)}
         keyword? schema-schema]]
       ; [:map {:required true}
       ;    [:age {:required true} number?]
       ;    [:name {:required false} string?]
       ;    ...]
       ; [:map {:required true}
       ;    [:age {:required {:depends-on :some-key :fn some-fn}} number?]
       ;    ...]
       ; [:map {:required true}
       ;    [:age {:required {:depends-on [:some-key [:.. :other-key]] :fn some-fn}} number?]
       ;    ...]
       [:vector
        (is= :map)
        required
        [:vector {:fn (count= 3)}
         keyword? required schema-schema]]

       ])

    (map
      #(s/validate schema-schema %)

      [(fn [_])
       [:vector foo]
       [:vector {} foo]
       [:vector {:fn foo} foo]
       [:vector {:fn foo} foo foo]

       [:map [:k foo]]
       [:map [:k {} foo]]
       [:map [:k {:required true} foo]]
       [:map [:k {:required false} foo]]
       [:map [:k {:required {:depends-on :relative-path :fn foo}}
              foo]]
       [:map [:k {:required {:depends-on [:relative-path
                                          [:.. :other "relative" :path :at 0]]
                             :fn foo}}
              foo]]
       ; currently not possible to describe this as schema. basically not doable say [keyword? keyword? ... fn? ]
       ]))


  ()
  )
