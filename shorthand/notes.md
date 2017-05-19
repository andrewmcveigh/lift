given a list of args and return, find which elements are:
 a) dependent, and if there are two or more
 b) if they are dependent on each other, then
 c) pull the respective f from env by op
 d) write into :fn spec

we know that (fa :a) and (fr :ret) must be equal because they use n
:fn (let [fa (:f env-a)
          fr (:f env-r)]
      (fn [x] (= (-> x :args :a fa)
                 (-> x :ret fr))))


;; [n, int? ** vect? n int?]
;; [2 [3 4]]
;; (s/explain ::type (.-v (first (value-seq '(vect? 4 nat-int?)))))

;; data DPair : (a : Type) -> (P : a -> Type) -> Type where
;;     MkDPair : {P : a -> Type} -> (x : a) -> P x -> DPair a P
;; (n : Nat ** Vect n Int)
;; (2, [3 4])
;; DPair Nat (\n -> Vect n Int)
;; MkPair 2 [3 4]
;; (int? -> int?) /= ((int? -> int?))
;; (int? -> int? -> int?) /= (int? -> (int? -> int?))
;; int? -> {:x int?}

;;: TODO:
;;: * Varargs functions
;;:   (fdef +, int? -> (s/+ int?) -> int?)
;;: * Each type of type can be represented by deftype
;;: * Sum type shorthand expands to s/or (with/out destructuring)
;;;   - But how would that work? We can write s/or in a spec without it
;;;     being a "type" in the registry
;;: * What is the difference between a spec and a product?
;;;   - Is there one? - I don't think there is
;;;   - So what do we call it?
;;;   - cplx

;; (to-spec (parse-spec (s/conform ::re-type '(vect? (+ n m) int?))))

;; (parse-spec
;;  '[:tsig
;;    {:op vect?,
;;     :args
;;     [[:expr {:op + :args [[:expr-var n] [:expr-var m]]}]
;;      [:pred int?]]}])

;; (to-spec (parse-type-sig '(vect? n int? -> vect? n int?)))
