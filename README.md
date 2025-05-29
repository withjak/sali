# sali
Tiny schema validation library for clojure/clojurescript/clojuredart

# Features
- Only does validation
- Works with clojuredart/clojure/clojuresctipt

# Usage 
The syntax is inspired from malli.

```clojure
(require '[sali.core :as sali])

; For now data comes first :(
(sali/validate 1 number?)
(sali/validate {:a 1} [:map [:a number?}]])

; options can be passed for each key in the map
; keys :a :b :c are required
(sali/validate 
    {:a 1 :b 2 :c 3 :d 4} 
    [:map 
        [:a {:required true} number?}]
        [:b {} number?}]
        [:c number?}]
        [:d {:required false} number?}]])

; map itself can take options
; key options = (merge map-options key-options)
; keys :a :b :c are optional 
; key :d is required
(sali/validate 
    {:a 1 :b 2 :c 3 } 
    [:map {:required false}
        [:a number?}]
        [:b {} number?}]
        [:c {:required true} number?}]])

; something extra
(sali/validate {} map?)
(sali/validate {} [:map]) ; if confusing please ignore

; for now, schema must apply to all the elements of the vector
; vector do not take options
(sali/validate [1 2] [:vector number?])

; similarly for set
(sali/validate #{1 2} [:set number?])

; and
(sali/validate [2 4] [:vector [:and number? 
                                    #(zero? (mod % 2))]])

; or 
(sali/validate [1 :love] [:vector [:or number? 
                                       keyword?]])


; when key depends on presence or value of other key(s)
(sali/validate 
    {:price :low 
     :money :i-have-it 
     :buy true}
    [:map 
        [:price #(#{:low :high} %)]
        [:money #(#{:i-have-it :i-do-not-have-it} %)]

        [:buy {:required [:price ; relative path to :path key
                          :money
                          (fn [_key p m])
                            ; _key is :buy
                            (and (= p :low)
                                 (= m :i-have-it))]} 
              boolean?]])

```

# Tests
Clojure
```bash 
# Run your specific test namespace
clj -M:clj-test -e "(require 'sali.core-test) (clojure.test/run-tests 'sali.core-test)"
```

