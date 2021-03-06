(ns parseclj.core)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Notes: 
; - need to make decisions about memoization, and how this plays in to the
;   tutorial paper
;
; - I think ignoring parse values can be added in by adding an Ignore record
;   and an ignore function that wraps a parser in an Ignore record, this then 
;   allows <*> and <$> combinators to check if they need to ignore an argument
;   and apply the correct rules for the ignorant combinator...
;
;   similarly it seems easy enough to wrap everything in a Parser record as it 
;   was before adding the macros, the only change seems to be making sure to 
;   wrap the parser result returned by the macro in a Parser record as well as 
;   the result of combinators.
;
;   It may be possible to attach error messages to particular parsers in a 
;   similar way, via a wrapping record, just need to figure out how all of 
;   the different wrappings will play around with one another, possibly add
;   ignore and error fields to the parser record, which ignore and error 
;   functions will set when called, this seems like a decent first solution
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ToDo:
; - Make Parser record private with :run :ignore :error keys(?) then (DONE?)
;   export functions run ignore error that deal with these things 
; - More combinators
; - parsers that ignore their results
;   - can probably deal with this and errors with a record and
;     and somewhat specializing the <*> and <$> definitions
; - Dealing with left/right recursion
; - Precedence
; - some sort of monadic or applicative interface
; - documentation
; - generic code cleanup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Parser type, also allows annotating a parser with whether its result
; should be ignored, 
(defrecord Parser [ignore run])

; declare this parser's result should be ignored
(defn ignore [p] (Parser. true (:run p)))

; easily build default parser records
(defn parser [p] (Parser. false p))

; easily run parsers with (run parser input)
(defn run [p inp] ((:run p) inp))


;; Macro to make parsers lazy
;; and avoid evalutation of the 
;; parser body, allowing more 
;; general recursion
(defmacro delay-parser [parser]
  `(parser (fn [ & args#] 
    (apply (:run ~parser) args#))))

;; macro to actually define lazy parser
;;
;; I'd like to fix this to allow the parser to possibly take arguments
(defmacro define-parser [parser body]
  `(def ~parser (delay-parser ~body)))




;; helper function to hide the exception handling
;; that seems to be needed in the definition of <*>
(defn curry [f x]
  (try
     (f x)
     (catch Exception _ (partial f x))))


;; it might be necessary to wrap parsers in a record
;; as a level of indirection in order to avoid the 
;; infinite recursion problem...



;; Basic symbol combinator for parsing a single symbol
;; if it encounters something that is not a sequence
;; it will just fail and return an empty sequence of 
;; parses
(defn pSym [a] 
    (parser (fn [inp]
      (cond
        (empty? inp) []
        (= (first inp) a) [[(first inp) (rest inp)]]
        :else []))))




;; The parser that always succeeds and returns the 
;; value a as its result
(defn pReturn [a]
  (parser (fn [inp] [[a inp]])))

;; the corresponding failing parser
(defn pFail []
  (parser (fn [inp] [])))


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
  (let [result (parser (fn [inp]
                 (for [[v1 ss1] ((:run p1) inp)
	                     [v2 ss2] ((:run p2) ss1)]
		               (if (:ignore p2)
                    [v1 ss2] 
                    [(curry v1 v2) ss2]))))]
    (reduce <*> result ps)))


(defn <|> [p1 p2 & ps]
    (let [result (parser (fn [inp] (concat ((:run p1) inp) ((:run p2) inp))))]
      (reduce <|> result ps)))


(defn <$> [f p & ps]
  (let [result (<*> (pReturn f) p)]
    (reduce <*> result ps)))



; I don't know if this one will work without fixing the define-parser macro
; need to make a decision about whether this should work only on strings
; or try to dispatch on the type of the collection/sequence
(defn pSyms [l] 
  (cond
    (empty? l) (pReturn (empty l))
    :else (<$> cons
               (pSym (first l))
               (pSyms (rest l)))))


;; temporary solution
(defn pString [l] 
  (cond
    (empty? l) (pReturn (empty l))
    :else (<$> (fn [x] (fn [y] (apply str (cons x y))))
               (pSym (first l))
               (pString (rest l)))))
;
; user=> (run (<$> cons (pString "hello " ) (pString "world")) "hello world")
; ([("hello " \w \o \r \l \d) ()])
;
; this appears to be the behavior of 
; user=> (cons "hello"  "world")
; ("hello" \w \o \r \l \d)
;
; user=> (def conc (fn [x] (fn [y] (concat x y))))
; #'user/conc
; user=> (run (<$> conc (pString "hello " ) (pString "world")) "hello world")
; ([(\h \e \l \l \o \space \w \o \r \l \d) ()])
;
; need to figure out how to fix this...

(defn prnv [s e] (do (prn s) e))

(defn pToList [p]
  (parser (fn [inp]
    (for [[v ss] ((:run p) inp)]
      [(list v) ss]))))

(defn pSeq [p & ps]
  (cond 
    (empty? ps) (parser (fn [inp] 
                  (for [[v ss] ((:run p) inp)]
                    [(list v) ss])))
    :else (<$> cons p (apply pSeq ps))))




    
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

(defn pParens [p]
  (<$>
    (fn [x] x)
    (ignore (pSym lparen))
    p
    (ignore (pSym rparen))
    ))

(def minc (fn [x] (fn [y] (max (+ x 1) y))))

(define-parser parens3
  (<|>
    (<$>
     minc
     (pParens parens3)
     parens3)
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













