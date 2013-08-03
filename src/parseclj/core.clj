(ns parseclj.core)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Notes: 
; - need to make decisions about memoization, and how this plays in to the
;   tutorial paper
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ToDo:
; - More combinators
; - parsers that ignore their results
; - Dealing with left/right recursion
; - Precedence
; - some sort of monadic or applicative interface
; - documentation
; - generic code cleanup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




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
;; 
;; might need to remove this after adding the lazy stuff
;
;; I need to think more about the interplay between this and the lazy stuff
; (defrecord Parser [run])


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
;; stream to be parsed , 
;; I don't think that can be done without using macros
;; ignore will always need two arguments to know which side is supposed 
;; to be ignored
(defn <*> [p1 p2 & ps]
  (let [result (fn [inp]
                 (for [[v1 ss1] (p1 inp)
	                     [v2 ss2] (p2 ss1)]
		               [(curry v1 v2) ss2]))]
    (reduce <*> result ps)))


(defn <|> [p1 p2 & ps]
    (let [result (fn [inp] (concat (p1 inp) (p2 inp)))]
      (reduce <|> result ps)))


(defn <$> [f p & ps]
  (let [result (<*> (pReturn f) p)]
    (reduce <*> result ps)))





; (defn ignore [p]
;   (fn [inp]
;     (for [[v ss] (p inp)]
;       [(fn [x] x) ss])))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple tests, need to move these out of this file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def lparen \()
(def rparen \))

(define-parser parens 
  (<|>
    (<*> 
      (<*>
        (<*> 
          (<*> (pReturn (fn [a b c d] (max (+ 1 b) d))) (pSym lparen))
          parens)
        (pSym rparen))
     parens)
    (pReturn 0)))

(define-parser parens1 
  (<|>
    (<*> 
      (pReturn (fn [a b c d] (max (+ 1 b) d)))
      (pSym lparen)
      parens1
      (pSym rparen)
      parens1)
    (pReturn 0)))

(define-parser parens2 
  (<|>
    (<$> 
      (fn [a b c d] (max (+ 1 b) d))
      (pSym lparen)
      parens2
      (pSym rparen)
      parens2)
    (pReturn 0)))



;(def parens (parens_))

(define-parser a|b
  (<|> (pSym \a)
       (pSym \b)))

(define-parser a|b|c 
  (<|> (pSym \a)
       (pSym \b)
       (pSym \c)))

(define-parser pLettera (pSym \a))

(define-parser pString_aa
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

(define-parser abcd
  (<*> 
    (<*> (pReturn cons) (pSym \a))
    (<*> (<*> (pReturn cons) (pSym \b))
         (<*> (pReturn (fn [x] [x])) (pSym \d)))))













