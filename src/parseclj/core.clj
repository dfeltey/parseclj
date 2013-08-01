(ns parseclj.core)



;; Basic symbol combinator for parsing a single symbol
;; if it encounters something that is not a sequence
;; it will just fail and return an empty sequence of 
;; parses
(defn pSym [a] 
  (fn [inp]
    (cond
      (empty? inp) []
      (= (first inp) a) [[(first inp) (rest inp)]]
      :else [])))




;; The parser that always succeeds and returns the 
;; value a as its result
(defn pReturn [a]
  (fn [inp] [[a inp]]))

;; the corresponding failing parser
(defn pFail []
  (fn [inp] []))


;; traditional Haskell-ish sequencing combinator
(defn <*> [p1 p2]
  (fn [inp]
      (for [[v1 ss1] (p1 inp)
	    [v2 ss2] (p2 ss1)]
	      (try
		   [(v1 v2) ss2]
		   (catch Exception _ [(partial v1 v2) ss2])))))
                ;; the lack of automatic currying makes this part challengin
                ;; need to somehow reflect on the number of arguments required
                ;; or just always use partial....



(def pLettera (pSym \a))

(def pString_aa
     (<*>
	(<*>
	    (pReturn cons)
	    pLettera)
	(<*>
	    (pReturn (fn [x] [x]))
	    pLettera)))