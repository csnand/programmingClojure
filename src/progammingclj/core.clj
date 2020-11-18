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
  (cstr/upper-case s))

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
(take 4 (line-seq (reader "src/progammingclj/core.clj")))

;; wrap reader in with-open bindings
;; to close reader correctly when the
;; body is complete
(with-open [rdr (reader "src/progammingclj/core.clj")]
  (count (line-seq rdr)))

(with-open [rdr (reader "src/progammingclj/core.clj")]
  (count (filter #(re-find #"\S" %) (line-seq rdr))))

;; count loc in current project
(defn non-blank?   [line] (not (cstr/blank? line)))
(defn non-comment? [line] (not (cstr/starts-with? ";;" line)))
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
    {:name "Musical Offering"    :composer "J. S. Bach"}
    {:name "Requiem"             :composer "W. A. Mozart"}
    {:name "Requiem"             :composer "Giuseppe Verdi"}})
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
