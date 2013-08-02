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
;; dealing with more than two args like this seems
;; sort of a hack, but should work most of the time...
;; need to figure out how this works with
;; parsing that ignore their results....
;;
;; I think the idea for handling parsers with an ignored return value
;; might be an explicit ignore combinator that will parse the same content
;; as its argument, but essentiall ignore the result so syntax would be roughly
;
; (<*> (ignore pIF)
;       p1
;       (ignore pThen)
;       p2
;       (ignore pElse)
;      p3
;        )
;
;; so this parses an if p1 then p2 else p3 type of statement
;; but ignores the parses for recognizing the keywords, or at
;; least ignores their values, and just passes along the remaining 
;; stream to be parsed
(defn <*> [p1 p2 & ps]
  (let [result (fn [inp]
                 (for [[v1 ss1] (p1 inp)
	                     [v2 ss2] (p2 ss1)]
		               [(curry v1 v2) ss2]))]
    (reduce <*> result ps)))


(defn <|> [p1 p2 & ps]
    (let [result (fn [inp] (concat (p1 inp) (p2 inp)))]
      (reduce <|> result ps)))
    
    






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple tests, need to move these out of this file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def a|b
  (<|> (pSym \a)
       (pSym \b)))

(def a|b|c 
  (<|> (pSym \a)
       (pSym \b)
       (pSym \c)))



(def pLettera (pSym \a))

(def pString_aa
     (<*>
	(<*>
	    (pReturn cons)
	    pLettera)
	(<*>
	    (pReturn (fn [x] [x]))
	    pLettera)))