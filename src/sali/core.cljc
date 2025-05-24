(ns sali.core)

(defn resolve-relative-path [base-path dep]
  {:pre [(or (keyword? dep) (vector? dep))]}
  (if (vector? dep)
    (let [up? (= (-> dep first name)
                 #{\.})
          up (if up?
               (count (-> dep first name))
               0)
          ks (if up? (rest dep) dep)
          end (- (count base-path) (inc up))]
      (-> (subvec base-path 0 end)
          (into ks)))
    (-> (subvec base-path 0 (dec (count base-path)))
        (conj dep))))


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
                     (condp = required
                       true true
                       nil true
                       false)))]
    (if (= value ::not-found)
      (when required
        {:data    ::not-found
         :message (str (name key) " is required.")
         :path    path})
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
   (println "validate -> " [data schema path context])
   (cond
     (fn? schema) (try
                    (when-not (schema data)
                      [{:data    data
                        :message "Validation failed"
                        :path    path}])
                    (catch Exception e
                      (str "Predicate exception. Exception: " (ex-message e))))

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
                                 {:data data :message "Expected map." :path path}
                                 (let [errors (reduce
                                               (fn [acc field]
                                                 (let [[key ops key-schema] (if (= 3 (count field))
                                                                              field
                                                                              [(first field) {} (last field)])
                                                       errors (process-field
                                                               key (merge schema-opts ops) key-schema
                                                               data path context)]
                                                   (if (seq errors)
                                                     (conj acc errors)
                                                     acc)))
                                               [] schema-body)]
                                   (when (seq errors)
                                     (flatten errors))))

                          :vector (if-not (vector? data)
                                    {:data data :message "Expected vector." :path path}
                                    (let [s (first schema-body)
                                          errors (->> data
                                                      (map-indexed (fn [idx item]
                                                                     (validate item s (conj path idx) context)))
                                                      (remove nil?))]
                                      (when (seq errors)
                                        (flatten errors))))

                          :set (if-not (set? data)
                                 {:data data :message "Validation set" :path path}
                                 (let [s (first schema-body)
                                       errors (->> data
                                                   (map #(validate % s path context))
                                                   (remove nil?))]
                                   (when (seq errors)
                                     (flatten errors))))

                          :and (->> (map #(validate data % path context) schema-body)
                                    (remove nil?)
                                    seq)
                          :or (let [rv (map #(validate data % path context) schema-body)]
                                (if (some nil? rv)
                                  nil
                                  (flatten (remove nil? rv))))

                          [(str "Unknown schema type: "
                                schema-type
                                (when (seq path)
                                  (str " at " (last path))))]))

     :else (str "Invalid schema format: "
                (pr-str schema)
                (when (seq path)
                  (str " at " (last path)))))))


;; --- Example Usage ---
(comment 
  
  (def property-types #{"Bungalow", "Flat", "Showroom", "Office", "Plot", "Agri Land"})
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
                [:country string?                          ; any constraints on valid city name should be handled at input form
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
             [:lease number?]]]]))
