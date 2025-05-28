(ns sali.core)

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
                   (if (vector? required)
                     (let [f (last required)
                           depends-on (butlast required)
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
      (validate value schema (conj path key) context))))

(defn validate
  "schema can be a function, a vector.
   - function : a function of one argument.
   - vector : [type options body]
              or
              [type body]"
  ([data schema]
   (validate data schema [] data))
  ([data schema path]
   (validate data schema path data))
  ([data schema path context]
   (let [result (cond
                  (or (fn? schema)) (try
                                      (if (schema data)
                                        true
                                        {:data data :path path :message "Validation failed" :f schema})
                                      (catch Exception e
                                        {:data data :path path :message (str "Predicate exception. Exception: " (ex-message e))}))

                  (vector? schema) (let [schema-type (first schema)
                                         ; as of now we only use options in [:map opts ...]
                                         options? (map? (second schema))
                                         schema-opts (if options?
                                                       (second schema)
                                                       {})
                                         schema-body (if options?
                                                       (drop 2 schema)
                                                       (rest schema))]
                                     (case schema-type
                                       :map (if-not (map? data)
                                              {:data data :path path :message "Expected map."}
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
                                                 {:data data :path path :message "Expected vector."}
                                                 (let [cdata (count data)
                                                       cschema (count schema-body)
                                                       schema-body (if (zero? cschema)
                                                                     [(fn [_] true)]
                                                                     schema-body)
                                                       schema-body (cond
                                                                     (= cdata cschema) schema-body
                                                                     (< cdata cschema) (take cdata schema-body)
                                                                     :else (concat
                                                                             schema-body
                                                                             (repeat (- cdata cschema) (last schema-body))))
                                                       errors (->> (mapv
                                                                     (fn [idx value s]
                                                                       (validate value s (conj path idx) context))
                                                                     (range) data schema-body)
                                                                   (remove true?))
                                                       f (get schema-opts :f (fn [_] true))]
                                                   (if-let [f-errors (not (f data))]
                                                     (conj errors {:path    path
                                                                   :data    data
                                                                   :message "Opts {:f ...} did not satisfy"})
                                                     errors)))

                                       :set (if-not (set? data)
                                              {:data data :path path :message "Expected set."}
                                              (let [s (first schema-body)
                                                    errors (->> data
                                                                (map #(validate % s path context))
                                                                (remove true?))
                                                    f (get schema-opts :f (fn [_] true))]
                                                (if-let [f-errors (not (f data))]
                                                  (conj errors {:path    path
                                                                :data    data
                                                                :message "Opts {:f ...} did not satisfy"})
                                                  errors)))

                                       :and (->> (map #(validate data % path context) schema-body)
                                                 (remove true?))

                                       :or (let [rv (mapv #(validate data % path context) schema-body)]
                                             (if (some true? rv)
                                               []
                                               (remove true? rv)))

                                       {:type    :schema-error
                                        :message (str "Unknown schema type: "
                                                      (pr-str schema-type)
                                                      (when (seq path)
                                                        (str " at path " (last path))))}))

                  :else {:type    :schema-error
                         :message (str "Invalid schema format: "
                                       (pr-str schema)
                                       (when (seq path)
                                         (str " at " (last path))))})]
     ; (prn data schema path :=> result)
     (prn (type result) result)
     (cond
       (map? result) [(dissoc result :f)]
       (true? result) true
       (empty? result) true
       (seq result) (flatten result)
       :else (throw (Exception. (apply str "got " result [data schema path]))))
     )))

(comment
  (declare a)

  (def a
    (or [] [a]))

  (defn check-a
    [a]
    (if (= a [])
      true
      (check-a (first a))))

  (check-a [[[]]]))


(comment
  (def types #{:bungalow, :flat, :showroom, :office, :plot, :agri-land})
  (def property-for #{:rent :sell :lease})
  (def area-units #{:sq-ft :sq-m})

  (validate
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
                         :url  "https://example.com/property-title-deed.pdf"}]}

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
                        [:flat {:required [:type (fn [key type]
                                                   (= type key))]}
                         [:map [:bhk number?]]]]]
     [:price [:map {:required [:property-for (fn [key property-for]
                                               (property-for key))]}
              [:rent number?]
              [:sell number?]
              [:lease number?]]]])
  )
