(ns parseclj.core)


;; helper function to hide the exception handling
;; that seems to be needed in the definition of <*>
(defn curry [f x]
  (try
     (f x)
     (catch Exception _ (partial f x))))



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
;; need to generalize it to take more than 2 argumnents
;; otherwise syntax will get messy very quickly
;; the challenge then becomes handling parsers that ignore results...
;; maybe macros can fix that issue...?
(defn <*> [p1 p2]
  (fn [inp]
      (for [[v1 ss1] (p1 inp)
	    [v2 ss2] (p2 ss1)]
		   [(curry v1 v2) ss2])))
                   ;; curry does exception handling to deal with not providing
                   ;; v1 with all of its arguments immediately


(defn <|> [p1 p2 & ps]
  (fn [inp]
    (let [result (concat (p1 inp) (p2 inp))]
      (cond
        (empty? ps) result
        :else (reduce <*> result ps)))))
    
    







;; simple tests, need to move these out of this file
(def pLettera (pSym \a))

(def pString_aa
     (<*>
	(<*>
	    (pReturn cons)
	    pLettera)
	(<*>
	    (pReturn (fn [x] [x]))
	    pLettera)))