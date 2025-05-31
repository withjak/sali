(ns sali.core)

(defn type-of
  [data]
  #?(:clj  (type data)
     :cljd (.-runtimeType data)))

(defn resolve-relative-path [base-path dep]
  {:pre [(or (keyword? dep) (vector? dep))]}
  (if (vector? dep)
    (let [up? (= (-> dep first name set)
                 #{\.})
          up (if up?
               (count (-> dep first name))
               0)
          ks (if up? (rest dep) dep)
          end (- (count base-path) (inc up))]
      (-> (subvec base-path 0 end)
          (into ks)))

    (let [end (dec (count base-path))
          end (if (neg? end)
                0
                end)]
      (-> (subvec base-path 0 end)
          (conj dep)))))


(declare ^:private validate)

(defn- ^:private process-field
  [key opts-map schema data path context]
  (let [value (get data key ::not-found)
        required (let [{:keys [required]} opts-map]
                   (if (map? required)
                     (let [f (:fn required)
                           depends-on (let [d (:depends-on required)]
                                        (if (keyword? d)
                                          [d]
                                          d))
                           dep-values (->> depends-on
                                           (map #(resolve-relative-path path %))
                                           (map #(get-in context %)))]
                       (apply f key dep-values))
                     (or (nil? required)
                         (true? required))))]
    (if (= value ::not-found)
      (if required
        {:data ::not-found :path path :message (str key " is required.")}
        true)
      (validate schema value (conj path key) context))))

(defn validate
  "schema can be a function, a vector.
   - function : a function of one argument.
   - vector : [type options body]
              or
              [type body]"
  ([schema data]
   (validate schema data [] data))
  ([schema data path]
   (validate schema data path data))
  ([schema data path context]
   ; (println schema path)
   (let [result (cond
                  (fn? schema) (try
                                 (if (schema data)
                                   true
                                   {:data    data :type (type-of data) :path path
                                    :message "Validation failed" :f schema})
                                 (catch Exception e
                                   {:data data :type (type-of data) :path path :message (str "Predicate exception. Exception: " (ex-message e))}))

                  (vector? schema) (let [schema-type (first schema)
                                         ; as of now we only use options in [:map opts ...]
                                         options? (map? (second schema))
                                         schema-opts (if options?
                                                       (second schema)
                                                       {})
                                         schema-body (if options?
                                                       (drop 2 schema)
                                                       (rest schema))]
                                     (if-not (pos? (count schema-body))
                                       {:data data :type (type-of data) :path path :message "schema must have a body."}
                                       (case schema-type
                                         :map (if-not (map? data)
                                                {:data data :type (type-of data) :path path :message "Expected map."}
                                                (->> (reduce
                                                       (fn [acc field]
                                                         (let [[key ops key-schema] (if (= 3 (count field))
                                                                                      field
                                                                                      [(first field) {} (last field)])
                                                               rv (process-field
                                                                    key (merge schema-opts ops) key-schema
                                                                    data path context)]
                                                           (conj acc rv)))
                                                       [] schema-body)
                                                     (remove true?)))

                                         :vector (if-not (vector? data)
                                                   {:data data :type (type-of data) :path path :message "Expected vector."}
                                                   (let [cdata (count data)
                                                         cschema (count schema-body)
                                                         schema-body (cond
                                                                       (= cdata cschema) schema-body
                                                                       (< cdata cschema) (take cdata schema-body)
                                                                       :else (concat
                                                                               schema-body
                                                                               (repeat (- cdata cschema) (last schema-body))))
                                                         errors (->> (mapv
                                                                       (fn [idx value s]
                                                                         (validate s value (conj path idx) context))
                                                                       (range) data schema-body)
                                                                     (remove true?))
                                                         f (get schema-opts :fn (fn [_] true))]
                                                     (if-let [f-errors (not (f data))]
                                                       (conj errors {:path    path
                                                                     :data    data
                                                                     :type    (type-of data)
                                                                     :message "Opts {:fn ...} did not satisfy"})
                                                       errors)))

                                         :set (if-not (set? data)
                                                {:data data :type (type-of data) :path path :message "Expected set."}
                                                (let [s (first schema-body)
                                                      errors (->> data
                                                                  (map #(validate s % path context))
                                                                  (remove true?))
                                                      f (get schema-opts :fn (fn [_] true))]
                                                  (if-let [f-errors (not (f data))]
                                                    (conj errors {:path    path
                                                                  :type    (type-of data)
                                                                  :data    data
                                                                  :message "Opts {:fn ...} did not satisfy"})
                                                    errors)))

                                         :and (->> (map #(validate % data path context) schema-body)
                                                   (remove true?))

                                         :or (let [rv (mapv #(validate % data path context) schema-body)]
                                               (if (some true? rv)
                                                 []
                                                 (remove true? rv)))

                                         (throw (Exception. (str "Unknown schema type: "
                                                                 (pr-str schema-type)
                                                                 (when (seq path)
                                                                   (str " at path " (last path)))))))))

                  :else (throw (Exception. (str "Invalid schema format: "
                                                (pr-str schema)
                                                (when (seq path)
                                                  (str " at " (last path)))))))]

     ; (prn data schema path :=> result)
     (cond
       (map? result) [(dissoc result :f)]
       (true? result) true
       (empty? result) true
       (seq result) (flatten result)
       :else (throw (Exception. (apply str "got " result [data schema path])))))))


(comment
  (def types #{:bungalow, :flat, :showroom, :office, :plot, :agri-land})
  (def property-for #{:rent :sell :lease})
  (def area-units #{:sq-ft :sq-m})

  (validate
    [:map
     [:id uuid?]
     [:type #(types %)]
     [:contact [:map
                [:name [:and string? #(< (count %) 30)]]
                [:number string?]]]
     [:area [:map
             [:unit #(area-units %)]
             [:value number?]]]
     [:price-negotiable boolean?]
     [:location [:map
                 [:country string?
                  :city string?
                  :longitude number?
                  :latitude number?
                  :address string?]]]
     [:notes [:and string? #(< (count %) 10000)]]
     [:images [:vector string?]]
     [:docs [:vector
             [:map
              [:name string?]
              [:type #(= :pdf %)]
              [:url string?]]]]
     [:property-for [:set #(property-for %)]]
     [:property-fields [:map
                        [:flat {:required {:depends-on :type
                                           :fn         (fn [key type]
                                                         (= type key))}}
                         [:map [:bhk number?]]]]]
     [:price [:map {:required {:depends-on :property-for
                               :fn         (fn [key property-for]
                                             (property-for key))}}
              [:rent number?]
              [:sell number?]
              [:lease number?]]]]

    {:id               1
     :type             "flat"
     :property-for     #{::sell :rent}
     :property-fields  {:flat {:bhk 2}}
     :contact          {:name   "Akshay Patel"
                        :number "9876543210"}
     :price            {:rent 100
                        :sell 10}
     :price-negotiable false
     :area             {:value 1000 :unit :sq-ft}
     :location         {:city      "Anytown"
                        :country   "USA"
                        :longitude 123.456
                        :latitude  78.910
                        :address   "123 Main St"}
     :notes            "some text"
     :images           ["https://images.unsplash.com/photo-1580587771525-78b9dba3b914"]
     :docs             [{:name "Property plan"
                         :type :pdf
                         :url  "https://example.com/property-title-deed.pdf"}]})
  )
