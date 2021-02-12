(ns programmingclj.core)

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
  (str "Hello " username))
(defrecord Book [title author])

(defn greeting
  "return greeting message.
  the default username is World"
  ([] (greeting "World"))
  ([username] (str "Hello " username)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variadic parameter using &
(defn date-chaperon-info
  [person-1 person-2 & chaperons]
  (println person-1 " and " person-2
           " went out with "
           (count chaperons) " chaperons." ))
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
    (filter indexable-words? (cstr/split text #"\W+"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function composition using anonymous functions
;; also a practice of functional programming which
;; treats function as first-class citizen
(defn make-greeter [greeting-prefix]
  (fn [username] (str greeting-prefix ", " username)))

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
  (cstr/upper-case s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flow controls
;; if - using do to introduce side effects
(defn is-small? [number]
  (if (< number 100)
    "yes"
    (do
      (println "Saw a big number" number)
      "no")))
;; loop/recur
;; (loop [bindings*] exprs*)
;; loop sets a recursion point
;; which can be targeted by recur form
;; (recur exprs*)
(defn test-loop-recur []
  (loop [result []
         x 5]
    (if (zero? x) result
        (recur (conj result x) (dec x)))))

;; recur can be used to go back to the top of function
;; in this case, the entire body of function acts as
;; an implicit loop
(defn countdown
  ([x] (countdown [] x))
  ([result x] (if (zero? x) result
      (recur (conj result x) (dec x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find all path from origin(0, 0) to a
;; given point(x, y)
(defn find-all-paths
  "recursion version: find all path from origin(0, 0) to a given point(x, y)"
  [x y]
  (if (or (zero? x) (zero? y)) 1
      (+ (find-all-paths (- x 1) y) (find-all-paths x (- y 1)))))

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
(cstr/join \, ["a", "b", "c", "d"])

;; filtering seqs - lazy evaluation applied here
;; (filter     pred coll) ; see example above
;; (take-while pred coll)
;; (drop-while pred coll)
;; (split-at   index coll)
;; (split-with pred coll)
(take 10 (filter even? (iterate inc 1)))
(take 10 (filter odd? (iterate inc 1)))

(split-at 5 (range 10))
(split-with #(<= % 10) (range 0 20 2))

(take-while #(<= % 10) (range 0 20 2))
(drop-while #(<= % 10) (range 0 20 2))

;; seq pred
;; (every?     pred coll)
;; (not-every? pred coll)
;; (not-any?   pred coll)
(every? even? [2 4 6 8])
(every? odd? [1 3 5])

;; (some pred coll) ; partial true
;; but is not a predicate
;; returns first result for non-true match
(some odd? [1 3 5])
(some identity [nil false 1 true])

;; transforming seqs
;; (map f coll)
(map #(format "<p>%s</p>" %) ["the" "quick" "brown" "fox"])
(map #(format "<%s>%s</%s>" %1 %2 %1)
     ["h1" "h2" "h3" "h1" "h5"]
     ["the" "quick" "brown" "fox"])

;; (reduce f coll)
;; total-up a seq
(reduce + (range 1 101))
(reduce * (range 1 11))

;; (sort    comp? coll)
;; (sort-by comp? coll)
(sort (reverse (range 11)))
(sort-by #(.toString %) [42 7 11 1])
(sort-by :grade < [{:grade 100} {:grade 2} {:grade 97}])

;; list comprehension
;; Clojure generalises list comprehension
;; to sequence comprehension
;; (for [binding-form coll-expr filter-expr? ...] expr)
;; as appeared in a previous example
(defn index-filter-example [pred coll]
  (when pred
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))

;; rewritten using list comprehension
;; for elt in coll such that return expr
(for [word ["the" "quick" "brown" "fox"]]
  (format "<p>%s<p/>" word))

;; for every number in coll yield :when even?
(take 20 (for [n (iterate inc 1)
               :when (even? n)] n))

;; for every number in coll yield :while even?
(for [n (conj (filter even? (take 20 (iterate inc 1))) 3)
      :while (even? n)] n)

;; for is quite handy when performing
;; cartesian product with several bindings
;; for would iterates rightmost binding first
;; then all the way to the left
;; the following example enumerates all
;; possible positions in a chess board
(for [file "ABCDEFGH"
      rank "12345678"]
  (format "%c%c" file rank))

;; using (doall coll) ; yield result
;;       (dorun coll) ; return nil
;;           which means it only pass elts
;;           without saving it to memory
;; to force evaluation
;; the following binding
;; wont yield result until
;; explicitly called
(def testx (for [i (range 3)]
             (do (println i) i)))
;; (doall testx)
;; (dorun testx)

;; seq-ing on regular expressions
;; (re-seq regexp string)
(map cstr/upper-case (re-seq #"\w+" "the quick brown fox"))
(for [word (re-seq #"\w+" "the quick brown fox")]
  (format "%s" (cstr/upper-case word)))

;; seq over the file system
(import 'java.io.File)
(defn get-file-name []
  (map #(.getName %) (.listFiles (File. "."))))

;; depth-first walk
(defn count-files []
  (count (file-seq (File. "."))))

;; filter recent files
(defn min-to-ms [mins] (* mins 1000 60))
(defn recent-modified? [file]
  (> (.lastModified file)
     (- (System/currentTimeMillis) (min-to-ms 30))))
;; (filter recent-modified? (file-seq (File. ".")))

;; seq-ing the stream
(require '[clojure.java.io :refer [reader]])
;; read current file and take first 4 lines
;; leave reader open
(take 4 (line-seq (reader "src/programmingclj/core.clj")))

;; wrap reader in with-open bindings
;; to close reader correctly when the
;; body is complete
(with-open [rdr (reader "src/programmingclj/core.clj")]
  (count (line-seq rdr)))

(with-open [rdr (reader "src/programmingclj/core.clj")]
  (count (filter #(re-find #"\S" %) (line-seq rdr))))

;; count loc in current project
(defn non-blank?   [line] (not (cstr/blank? line)))
(defn non-comment? [line] (not (cstr/starts-with? ";" line)))
(defn clj-src? [file] (.endsWith (.toString file) ".clj"))
(defn clj-loc [base-file]
  (reduce +
          (for [file (file-seq base-file) :when (clj-src? file)]
            (with-open [rdr (reader file)]
              (count (filter (and non-blank? non-comment?)
                             (line-seq rdr)))))))
(println (clj-loc (File. ".")))

;; structure-specific functions
;; peek pop for lists (first elt)
(peek '(1 2 3))
(pop  '(1 2 3))

;; .. for vectors (LIFO)
(peek [1 2 3])
(pop  [1 2 3])

;; get assoc for vectors
;; vectors themselves are functions
;; therefore function syntax applies here
(= (get [1 2 3] 2) ([1 2 3] 2))
(assoc [1 2 3 4] 4 :five)

;; (subvec avec start end?)
(subvec [1 2 3 4 5 6] 3)

;; (keys map) (vals map) for maps
;; which return all keys or all vals
(def testmap {:one "1" :two "2"})
(keys testmap)
(vals testmap)

;; (get map key value-if not-found?)
;; default value-not-found is nil
;; maps are also functions
;; :keywords are also functions
;; therefore :keywords can be used in reverse order
(= (testmap :one) (:one testmap))
(= (get testmap :three) (testmap :three))
(get testmap :three :not-found)

;; (contains? map key)
(contains? testmap :one)

;; add, remove and select kv pair in maps
(dissoc (assoc testmap :three 3) :three)
(select-keys testmap [:one :two])

;; merge multiple maps
;; keys in the rightmost map
;; will be the final result if conflict
(merge testmap {:one "one"})

;; (merge-with merge-fn & maps)
;; results will be depend on merge-fn function
;; if there are conflicts
(merge-with concat testmap {:one "one"})

;; functions on sets
;; based on set theory (maths)
;; also part of relational algebra
(require '[clojure.set :as cset])
(def testset-1 #{"1" "2" "three"})
(def testset-2 #{"one" "two" "three"})

(cset/union        testset-1 testset-2)
(cset/difference   testset-1 testset-2)
(cset/intersection testset-1 testset-2)
(cset/select #(= 1 (count %)) testset-1)

;; the six relational primitives are
;; set union  set difference
;; rename     selection
;; projection cross product

;; therefore a simple relational database
;; can be implemented using maps and sets
(def compositions
  #{
    {:name "The Art of the Fugue" :composer "J. S. Bach"}
    {:name "Musical Offering"     :composer "J. S. Bach"}
    {:name "Requiem"              :composer "W. A. Mozart"}
    {:name "Requiem"              :composer "Giuseppe Verdi"}})
(def composers
  #{
    {:composer "J. S. Bach"     :country "Germany"}
    {:composer "W. A. Mozart"   :country "Austria"}
    {:composer "Giuseppe Verdi" :country "Italy"}})
(def nations
  #{
    {:nation "Germany" :language "German"}
    {:nation "Austria" :language "German"}
    {:nation "Italy"   :language "Italian"}})

;; rename key :name to :title
(def titled-compositions (cset/rename compositions {:name :title}))

;; (select pred relation)
;; returns a map where pred is true
;; analogous to WHERE clause in SQL SELECT
(cset/select #(= (:title %) "Requiem") titled-compositions)

;; (project relation keys)
;; project returns only the part of
;; maps that match a set of keys
;; similar to SQL SELECT that
;; specifies a subset of columns
(cset/project compositions [:name ])

;; cartesian product
;; aka cross product
;; can be done using list comprehension
;; or (join relation-1 relation-2 keymap?)
;; that returns some subset of
;; full cross product
(cset/join compositions composers)

;; if key names in the two relation dont match
;; keymap can be used to map key names in relation-1
;; to key names in relation-2
;; the following example maps
;; :country to :nation
(cset/join composers nations {:country :nation})

;; SELECT country FROM composers
;;    INNER JOIN country ON
;;     (SELECT * FROM compositions WHERE name == "Requiem")
(cset/project
 (cset/join composers (cset/select #(= (:name %) "Requiem") compositions))
 [:country])

;; functional programming in Clojure
;; tail call optimisations (TCOs)
;; which avoid both avoid stack and heap overflow
;; letfn macro is quite useful here
;; (letfn fnspecs & body) ; fnspecs ==> [(fname [params*] exprs)+]

;; this version would fail even for small n
(defn stackoverflow-fibo [n]
  (cond (= n 0) 0
        (= n 1) 0
        :else (+ (stackoverflow-fibo (- n 1))
                 (stackoverflow-fibo (- n 2)))))

;; still failed for large numbers
(defn tail-fibo [n]
  (letfn [(fib [current-num next-num n]
            (if (zero? n)
              current-num
              (fib next-num (+ current-num next-num) (dec n))))]
    (fib 0N 1N n)))

;; Solutions in Clojure
;; 1. explicit self-recursion with recur
;; 2. lazy sequences
;; 3. explicit mutual recursion with trampoline

;; self-recursion with recur
;; works with large numbers
;; but only return one number each time
(defn recur-fibo [n]
  (letfn [(fib [current-num next-num n]
            (if (zero? n)
              current-num
              (recur next-num (+ current-num next-num) (dec n))))]
    (fib 0N 1N n)))

;; lazy sequences with (lazt-seq & body)
;; lazy-seq invokes its body only when needed
;; then it caches the result for subsequent calls
;; the key point is to wrap recursive part in
;; (lazy-seq) to replace
;; recursion with laziness
(defn lazy-seq-fibo
  ([] (concat [0 1] (lazy-seq-fibo 0N 1N)))
  ([a b]
   (let [n (+ a b)]
     (lazy-seq
      (cons n (lazy-seq-fibo b n))))))

;; but most of the time
;; (lazy-seq) doesnt need to be used directly
;; instead existing sequence library functions
;; that return lazy-sequences can be reused

;; iterate begins with first pair of fibo numbers
;; and generate a sequence of pairs the indicate
;; the current number and the next
;; then the fibo sequence can be obtained by
;; simply take the first number of each pair
(defn iterate-fibo []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; How many times in sequence does the heads come up twice?
;; (partition size step? coll)
;; breaks collection into chunks of size

;; (comp f & fs)
;; compose two or more functions
(def ^{:doc "Count items matching a filter"}
  count-if (comp count filter))

(= (count-if odd? [1 2 3 4 5]) 3)

(defn count-runs
  "Count runs of length n where pred is true in coll"
  [n pred coll]
  (count-if #(every? pred %) (partition n 1 coll)))

;; currying and partial application
;; there is no real currying in clojure
;; but partial will do the trick most of the time

;; (partial f & partial-args)
;; performs a partial application of a function
(def ^{:doc "Count runs of length 2 that are both heads"}
  count-heads-pairs (partial count-runs 2 #(= % :h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recursion revisited
;; mutual recursion
;; A mutual recursion occurs when the recursion bounces
;; between tow or more functions.

;; because my-odd? and my-even? both call the other
;; therefore both vars need to be created before the
;; definition of functions. (def or decalre)
;; declare macro can be used to create both vars without
;; initial bindings

;; recur cannot be applied for mutual recursion
;; stackoverflow will occur for large n
(declare my-odd-1? my-even-1?)
(defn my-odd? [n]
  (if (= n 0)
    false
    (my-even-1? (dec n))))
(defn my-even? [n]
  (if (= n 0)
    true
    (my-odd-1? (dec n))))

;; converting to self-recursion
(defn parity [n]
  (loop [n n par 0]
    (if (= n 0)
      par
      (recur (dec n) (- 1 par)))))
(defn my-odd-2? [n]
  (= 0 (parity n)))
(defn my-even-2? [n]
  (= 1 (parity n)))

;; trampolining mutual recursion
;; (trampoline f & partial-args)
;; if the return value is not a function
;; trampoline acts like calling function directly
;; if a function is returned, then trampoline will
;; call that function recursively.

;; tail recursion version of fib can be rewritten
;; using trampoline by prepending # to any recursive calls
;; example only. Don't write code like this.
(defn trampoline-fibo [n]
  (let [fib (fn fib [f-2 f-1 current]
              (let [f (+ f-2 f-1)]
                (if (= n current)
                  f
                  #(fib f-1 f (inc current)))))]
    (cond
      (= n 0) 0
      (= n 1) 1
      :else (fib 0N 1 2))))

;; trampoline does a recur, therefore can handle large input
;; (= 875N (rem (trampoline trampoline-fibo 1000000) 1000))
;; recur is preferred for self-recursion
;; while trampoline for mutual recursion

;; mutual recursive my-odd and my-even
;; can be rewritten using trampoline as follow
(declare trampoline-odd? trampoline-even?)
(defn trampoline-odd? [n]
  (if (= n 0)
    false
    #(trampoline-even? (dec n))))
(defn trampoline-even? [n]
  (if (= n 0)
    true
    #(trampoline-odd? (dec n))))
;; In practice, many Clojure programmers
;; never encounter a case requiring trampoline at all.

;; replacing recursion with laziness
;; with the following example
;; wallingford's scheme implementation of replace-symbol
;; overly-literal port, do not use
;; will cause stackoverflow for deeply nested structures
(declare replace-symbol replace-symbol-expression)
(defn replace-symbol [coll oldsym newsym]
  (if (empty? coll)
    ()
    (cons (replace-symbol-expression
           (first coll) oldsym newsym)
          (replace-symbol
           (rest coll) oldsym newsym))))
(defn replace-symbol-expression [symbol-expr oldsym newsym]
  (if (symbol? symbol-expr)
    (if (= symbol-expr oldsym)
      newsym
      symbol-expr)
    (replace-symbol symbol-expr oldsym newsym)))

;; helper function to create deeply nested data structure
(defn deeply-nested [n]
  ;; bind n to n
  ;; bind '(bottom) to result
  (loop [n n
         result '(bottom)]
    (if (= n 0)
      result
      (recur (dec n) (list result)))))

;; stackoverflow occurs here
;; (replace-symbol (deeply-nested 10000) 'bottom 'deepest)

;; the following version replaced recursion with lazy-seq
(defn- coll-or-scalar [x & _] (if (coll? x) :collection :scalar))
(defmulti replace-symbol coll-or-scalar)
(defmethod replace-symbol :collection [coll oldsym newsym]
  (lazy-seq
   (when (seq coll)
     (cons (replace-symbol (first coll) oldsym newsym)
           (replace-symbol (rest coll) oldsym newsym)))))
(defmethod replace-symbol :scalar [obj oldsym newsym]
  (if (= obj oldsym) newsym obj))

;; Shortcutting Recursion with Memoization

;; the male and famale sequence are defined as follows
;; F(0) = 1; M(0) = 0
;; F(n) = n - M(F(n-1)), n > 0
;; M(n) = n - F(M(n-1)), n > 0

;; do not use these directly
;; as it performs terribly for large value of n
(declare m f)
(defn m [n]
  (if (zero? n)
    0
    (- n (f (m (dec n))))))
(defn f [n]
  (if (zero? n)
    1
    (- n (m (f (dec n))))))

;; Memoization trades space for time
;; by caching the results of past calculations.
;; When you call a memoized function,
;; it first checks your input against a
;; map of previous inputs and their outputs.
;; If it finds the input in the map, it can return the
;; output immediately, without having to perform the calculation again.
(def m (memoize m))
(def f (memoize f))

;; memoization alone is not enough as stackoverflow may still
;; occur for large n before the cache can be built.

;; The final trick is to guarantee that the cache is built from the ground up by
;; exposing sequences, instead of functions.
(def m-seq (map m (iterate inc 0)))
(def f-seq (map f (iterate inc 0)))


;; eager transformation
(defn square [x] (* x x))
(defn sum-square-seq [n]
  (vec (map square (range n))))

;; the above version will
;; first calculate range
;; then apply map
;; finally return vector
;; which means three times as much sequence overhead

;; we can perform intermediate transformation on the input collection
;; and put the result directly into the output vector
;; using into with map transducer

;; a transducer is a function that captures the essence of a
;; collection transformation without tying it to the form of
;; either the input collection or the output collection.

;; (into output-collection transformation input-collection)
(defn sum-square [n]
  (into [] (map square) (range n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optimising performance

;; apply multiple transformation within into
;; in a single pass over the input

;; find all the predicate functions in the namespaces we've loaded
;; predicate functions usualy ending in ?
;; then find their public vars
;; convert them to friendly names
;; using sequence, transformation can be chained together with ->>
(defn preds-seq []
  (->> (all-ns)
       (map ns-publics)
       (mapcat vals)
       (filter #(cstr/ends-with? % "?"))
       (map #(str (.-sym %)))
       vec))

;; transducers are composed in a stack-like fashion
;; transducer can be composed in a similarly pipelined fashion using comp.
(defn preds []
  (into []
        (comp (map ns-publics)
              (mapcat vals)
              (filter #(cstr/ends-with? % "?"))
              (map #(str (.-sym %))))
        (all-ns)))

;; the sequence implementation creates four intermediate sequences
;; the transducer implementation composes the four transformation into a
;; single combined transformation ans applies it during
;; a single traversal of the input sequence

;; removing the intermediate sequences can result in a
;; significant reduction in memory usage

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; managing external resources
;; file is not closed and not clear for gc  when to close it.
;; as this is lazy approach
(defn non-blank-lines-seq-lazy [file-name]
  (let [reader (clojure.java.io/reader file-name)]
    (filter non-blank? (line-seq reader))))

;; for small files, it can be transformed to eagerly processing
;; but it will eventually throw OutOfMemoryError
(defn non-blank-lines-eager [file-name]
  (with-open [reader (clojure.java.io/reader file-name)]
    (into []
          (filter non-blank?)
          (line-seq reader))))

(defn non-blank-lines-counter-lazy [file-name]
  (count (non-blank-lines-seq-lazy file-name)))

;; An eduction is a suspended transformation (like a sequence),
;; but it processes the entire input each time its asked (usually just once)
(defn non-blank-lines-education [reader]
  (eduction (filter non-blank?) (line-seq reader)))

;; eager approach
;; The eduction processes each line and then
;; releases the associated memory, so the eduction will hold
;; only one line in memory at a time.
(defn non-blank-lines-reduce [file-name]
  (with-open [reader (clojure.java.io/reader file-name)]
    (reduce (fn [cnt el] (inc cnt)) 0 (non-blank-lines-education reader))))

;; Transducers and eductions allow us to choose exactly when an input source
;; is processed and thus know when the external resource is done and ready to
;; release. With lazy sequences, it’s much harder to know when processing is
;; “done” and it’s time to release.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; specification

;; public class Ingredient {
;;     private String name;
;;     private double quantity;
;;     private Unit unit;
;;     // ...
;;     public static Ingredient scale(Ingredient ingredient, double factor) {
;;         ingredient.setQuantity(ingredient.getQuantity() * factor);
;;         return ingredient;
;;     }
;; }

;; the above class as an example for spec
(require '[clojure.spec.alpha :as s])
(defn scale-ingredient [ingredient factor]
  (update ingredient :quatity * factor))

;; Specs describing an ingredient
(s/def ::ingredient
  (s/keys :req [::name ::quantity ::unit]))
(s/def ::name string?)
(s/def ::quantity number?)
(s/def ::unit keyword?)
;; Function spec for scale-ingredient
(s/fdef scale-ingredient
  :args (s/cat :ingredient ::ingredient :factor number?)
  :ret ::ingredient)
;; These specs give us a precise description of the shape of an ingredient map,
;; its fields, and their contents. The function spec gives us an explicit definition
;; of the arguments and return value of scale-ingredient. These specs don’t just
;; serve as documentation. The spec library uses them to provide several addi-
;; tional tools that operate on specs—data validation, explanations of invalid
;; data, generation of example data, and even automatically created generative
;; tests for functions with a spec.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the syntax
;; (spec/def name spec)
;; spec names must be fully qualified keywords.
;; Auto-resolved keywords start with :: e.g.
;; :: test-resolver in this namespace might expand
;; :programmingclj.core/test-resolver

;; predicates specs can be used to validating data
(s/def ::company-name string?)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

