(ns parseclj.core)

;; Macro to make parsers lazy
;; and avoid evalutation of the 
;; parser body, allowing more 
;; general recursion
(defmacro delay-parser [parser]
  `(fn [ & args#] 
    (apply ~parser args#)))

;; macro to actually define lazy parser
(defmacro define-parser [parser body]
  `(def ~parser (delay-parser ~body)))


;; a record type for Parsers
(defrecord Parser [run])

; (defn run [p inp]
;   (if (= (type p) Parser) ((:run p) inp)
;     ((p) inp) ))


;; helper function to hide the exception handling
;; that seems to be needed in the definition of <*>
(defn curry [f x]
  (try
     (f x)
     (catch Exception _ (partial f x))))

(def ccons 
  (fn [x]
    (fn [y]
      (cons x y))))

;; it might be necessary to wrap parsers in a record
;; as a level of indirection in order to avoid the 
;; infinite recursion problem...



;; Basic symbol combinator for parsing a single symbol
;; if it encounters something that is not a sequence
;; it will just fail and return an empty sequence of 
;; parses
(defn pSym [a] 
  (Parser.
    (fn [inp]
      (cond
        (empty? inp) []
        (= (first inp) a) [[(first inp) (rest inp)]]
        :else []))))




;; The parser that always succeeds and returns the 
;; value a as its result
(defn pReturn [a]
  (Parser. (fn [inp] [[a inp]])))

;; the corresponding failing parser
(defn pFail []
  (Parser. (fn [inp] [])))


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
;; stream to be parsed , 
;; I don't think that can be done without using macros
;; ignore will always need two arguments to know which side is supposed 
;; to be ignored
(defn <*> [p1 p2 & ps]
  (Parser. (let [result (fn [inp]
                 (for [[v1 ss1] ((:run p1) inp)
	                     [v2 ss2] ((:run p2) ss1)]
		               [(curry v1 v2) ss2]))]
    (reduce <*> result ps))))


(defn <|> [p1 p2 & ps]
  (Parser.
    (let [result (fn [inp] (concat ((:run p1) inp) ((:run p2) inp)))]
      (reduce <|> result ps))))



; (defn ignore [p]
;   (fn [inp]
;     (for [[v ss] (p inp)]
;       [(fn [x] x) ss])))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple tests, need to move these out of this file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parens_ []
  (<*> 
    (pReturn (fn [a b c d] (max (+ 1 b) d)))
    (pSym \()
    (Parser. (:run (parens_)))
    (pSym \))
    (<|> (Parser. (:run (parens_))) (pReturn 0))))

;(def parens (parens_))

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

;; because of the structure of cons this
;; has to associate to the right, not sure
;; if this is how it has to work for other 
;; functions as well??
;
;; leaving this note in for now as documentation
;; but this is how Haskell actually handles it 
;; as well

(def abcd
  (<*> 
    (<*> (pReturn cons) (pSym \a))
    (<*> (<*> (pReturn cons) (pSym \b))
         (<*> (pReturn (fn [x] [x])) (pSym \d)))))













