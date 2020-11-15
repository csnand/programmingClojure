(ns progammingclj.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; def bindings
(def visitors (atom #{}))

(defn hello
  "put username into visitors and return greeting message"
  [username]
  (swap! visitors conj username)
  (str "Hello " username)
  )
(defrecord Book [title author])

(defn greeting
  "return greeting message.
  the default username is World"
  ([] (greeting "World"))
  ([username] (str "Hello " username))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variadic parameter using &
(defn date-chaperon-info
  [person-1 person-2 & chaperons]
  (println person-1 " and " person-2
           " went out with "
           (count chaperons) " chaperons." )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require is to load Clojure namespace
;; while import is only for Java classes.
;; (import '(java.io InputStream File))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anonymous function
(require '[clojure.string :as cstr])
(filter (fn [w] (> (count w) 2)) (cstr/split "a find day" #"\W+"))
;; equivalent to
(filter #(> (count %) 2) (cstr/split "a find day" #"\W+"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; using let bindings and closures
(defn indexable-words [text]
  (let [indexable-words? (fn [w] (> (count w) 2))]
    (filter indexable-words? (cstr/split text #"\W+"))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function composition using anonymous functions
;; also a practice of functional programming which
;; treats function as first-class citizen
(defn make-greeter [greeting-prefix]
  (fn [username] (str greeting-prefix ", " username))
  )

(def hello-greeting (make-greeter "Hello"))
(def aloha-greeting (make-greeter "Aloha"))

;; make-greeter can be used directly
;; ((make-greeter "Howdy") "pardner")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; def binds a symbol to a value that stored in
;; Clojure vars
(def testdefvar 42)
;; var is a special form that returns a var itself
;; equivalent to reader macro #'
(= (var testdefvar) #'testdefvar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; destructing in clojure
(def test-person {:first-name "Harry" :last-name "Potter"})

;; in this scenario, all you need is the :first-name
(defn greet-person-1 [person]
  (println "Hello," (:first-name person)))

;; here comes another equivalent function
;; defined using destructing
(defn greet-person-2 [{fname :first-name}]
  (println "Hello," fname))

;; another usage of destructure in let binding
(let [[x y] [1 2 3]]
  [x y])
;; using _ as wildcard here
;; but actually _ is bound twice
(let [[_ _ z] [1 2 3]]
  z)

;; binding both part and the entire collection
;; using :as clause in side a destructing expression
(let [[x y :as coords] [1 2 3]]
  [x y coords])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; meta data in Clojure
(defn ^{:tag String} shout [^{:tag String} s]
  (clojure.string/upper-case s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flow controls
;; if - using do to introduce side effects
(defn is-small? [number]
  (if (< number 100)
    "yes"
    (do
      (println "Saw a big number" number)
      "no")
    )
  )
;; loop/recur
;; (loop [bindings*] exprs*)
;; loop sets a recursion point
;; which can be targeted by recur form
;; (recur exprs*)
(defn test-loop-recur []
  (loop [result []
         x 5]
    (if (zero? x) result
        (recur (conj result x) (dec x)))
    )
  )

;; recur can be used to go back to the top of function
;; in this case, the entire body of function acts as
;; an implicit loop
(defn countdown
  ([x] (countdown [] x))
  ([result x] (if (zero? x) result
      (recur (conj result x) (dec x))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find all path from origin(0, 0) to a
;; given point(x, y)
(defn find-all-paths
  "recursion version: find all path from origin(0, 0) to a given point(x, y)"
  [x y]
  (if (or (zero? x) (zero? y)) 1
      (+ (find-all-paths (- x 1) y) (find-all-paths x (- y 1))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert imperative to functional
;; with the indexOfAny function from
;; StringUtils.java as an example
;; public static int indexOfAny(String str, char[] searchChars) {
;;     if (isEmpty(str) || ArrayUtils.isEmpty(searchChars)) {
;;         return -1;
;;     }
;;     for (int i = 0; i < str.length(); i++) {
;;         char ch = str.charAt(i);
;;         for (int j = 0; j < searchChars.length; j++) {
;;             if (searchChars[j] == ch) {
;;                 return i;
;;             }
;;         }
;;     }
;;     return -1;
;; }

;; it can be rewritten as follow
;; vector here used as a function that return
;; an empty vector
(defn indexed
  "take any collection as parameter and return a
  sequence of pair of the form [idx elt]"
  [coll]
  (map-indexed vector coll))

;; for here is not loop but sequence comprehension
;; which means yield the idx
;; when (predicate element) is true
(defn index-filter [pred coll]
  (when pred
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))
;; now the indexOfAny function above
;; can be constructed as follow
(defn index-of-any [pred coll]
  (first (index-filter pred coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; now let talk about sequences
;; literally everything is a sequence
;; which is immutable
;; and has three core capabilities
;; (first aseq)
;; (rest aseq)
;; (cons elt aseq) ; cons is short from construct

;; seq functions works on all Clojure data structures
;; (seq coll) will return a seq on any seq-able collections
;; (= (next aseq) (seq (rest aseq))) => true
(def testlist '(1 2 3))
(first  testlist)
(rest   testlist)
(cons 0 testlist)
(= (next testlist) (seq (rest testlist)))

(def testvector [1 2 3])
(first  testvector)
(rest   testvector)
(cons 0 testvector)
(= (next testvector) (seq (rest testvector)))

;; also work on maps sets
;; thses two structures are unordered
;; as opposed to
;; (sorted-map & kv-pairs) (sorted-set & elements)
;; which is sorted by their natural orders
(first {:fname "test" :lname "test"}) ; map
(first #{:the :quick :brown :fox})    ; set

;; other capabilities of seq are
;; (conj coll element & elements)
;; (into to-coll from-coll)
;; both work in an efficient insertion manner
;; depending the underlying data structures

;; add elts to a collection
(conj testlist 5 6 7)
;; adds all elts from one coll to another coll
(into testvector testlist)

;; Most Clojure sequences are lazy
;; all sequences are immutable
;; seq functions always return seq
;; regardless of their input

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THE sequence library
;; functions can be categorised into four groups =>
;; functions => create / filter / transform seqs
;; and seq predicates

;; creating seqs
;; (range start? end? steps?) ; end exclusive
(range 0 9) ; start .. end
(range 0 101 2) ; start .. end .. steps
(range 0 -1 -0.25) ; neg steps
(range 1/2 4 1) ; ratios which means 1/2 a step till 4

;; (repeat n elt)
(repeat 10 "a")
(repeat 10 5)

;; (iterate f elt) ; apply function f to element and continues forever
;; (iterate inc 1) ; infinite seq from 1 to infinitum
;; lazy evaluation happens here
(take 10 (iterate inc 1))
(take 10 (repeat 1))

;; (cycle coll) ; repeat a collection infinitely
(take 12 (cycle (range 0 3)))

;; (interleave & coll)
;; equivalent to zip function in haskell
;; stop when one coll is exausted
(interleave (iterate inc 1) "abcde")

;; (interpose separator coll)
;; return A seq with all elements segregated by separator
(apply str (interpose "," ["a", "b", "c", "d"]))

;; equivalent to performance-optimised version
;; (join separator seq)
(require '[clojure.string :refer [join]])
(join \, ["a", "b", "c", "d"])








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
