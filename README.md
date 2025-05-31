# sali
Tiny schema validation library for clojure/clojuredart

# Features
- Only does validation
- Works with clojuredart/clojure

# Usage 
The syntax is inspired from malli.

```clojure
(require '[sali.core :as sali])

; (validate schema data)
(sali/validate number? 1) ;=> true
(sali/validate [:map [:a number?]] {:a 1})

; options can be passed for each key in the map
; keys :a :b :c are required, :d is optional
(sali/validate
  [:map
   [:a {:required true} number?]
   [:b {} number?]
   [:c number?]
   [:d {:required false} number?]]
  {:a 1 :b 2 :c 3 :d 4})

; map itself can take options
; key options = (merge map-options key-options)
; keys :a :b are optional 
; key :c is required
(sali/validate
  [:map {:required false}
   [:a number?]
   [:b {} number?]
   [:c {:required true} number?]]
  {:a 1 :b 2 :c 3})

; all elements will be tested for number?
(sali/validate [:vector number?] [1 2])
; first element will be tested for number?
; second element (and 3rd and so on) if present will be tested for keyword?
(sali/validate [:vector number? keyword?] [1 :2 :3])

; All elements of a set must satisfy the same schema
(sali/validate [:set number?] #{1 2})

; and
(sali/validate
  [:vector [:and number? even?]]
  [2 4])

; or 
(sali/validate [:vector [:or number? keyword?]] [1 :love])

; vector and set also take opts
(sali/validate
  [:set
   {:fn (fn [the-set]
          (= (count the-set) 5))}
   number?]
  #{1 2 3 4 5})

; when key depends on presence or value of other key(s)
(sali/validate
  [:map
   [:price #(#{:low :high} %)]
   [:money #(#{:i-have-it :i-do-not-have-it} %)]

   [:buy {:required {:depends-on [:price :money] ; relative path to :price and :money key
                     :fn         (fn [_key p m]
                                   ; _key is :buy
                                   (and (= p :low)
                                        (= m :i-have-it)))}}
    boolean?]]
  {:price :low
   :money :i-have-it
   :buy   true})
```
Check tests for more examples.

# Tests
Clojure
```bash 
# Run your specific test namespace
clj -M:clj-test -e "(require 'sali.core-test) (clojure.test/run-tests 'sali.core-test)"
```

