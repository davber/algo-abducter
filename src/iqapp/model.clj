(ns iqapp.model
  (:require [clojure.string :as string]
            [clojure.set :as set :refer [intersection]]))

(defrecord Op [sym arities associative commutative in-kinds out-kinds active])

(def EXPR-COUNTER (atom 0))
(def EVAL-COUNTER (atom 0))

(def MAX-ITERS 1000)
(def MAX-LEVELS 3)
(def ALL-TYPES #{::num ::str})
(def CONST-PROB 0.3)
(def BINARY-PROB 0.4)
(def UNARY-PROB 0.3) ;; NOTE: this is also deduced...

(def PARAM-PROB 0.7) ;; how big chance of using a parameter rather than a constant

(def OP-SEQ [(Op. '+ #{::two ::many} true true #{::num} #{::num} true)
             (Op. '- #{::one} false false #{::num} #{::num} true)
             (Op. '* #{::two ::many} true true #{::num} #{::num} true)
             (Op. 'inc #{::one} false false #{::num} #{::num} false)
             (Op. 'dec #{::one} false false #{::num} #{::num} false)
             (Op. 'str #{::two ::many} true false #{::num ::str} #{::str} true)
             (Op. 'count #{::one} false false #{::str} #{::num} true)
             (Op. 0 #{::const} false false #{} #{::num} true)
             (Op. 1 #{::const} false false #{} #{::num} true)
             (Op. 2 #{::const} false false #{} #{::num} true)
             (Op. 3 #{::const} false false #{} #{::num} true)])

(def OPS (into {} (map (fn [op] [(.-sym op) op]) OP-SEQ)))

(defn init-counters []
  (reset! EXPR-COUNTER 0)
  (reset! EVAL-COUNTER 0))

(defn eval-expr [op args]
  (swap! EVAL-COUNTER inc)
  (eval (apply list op args)))

(defn constant? [expr]
  ((some-fn number? string?) expr))

(defn get-ops-worker [arity out-kinds]
  (map :sym (filter #(and (.-active %) ((.-arities %) arity) (not-empty (intersection (.-out-kinds %) out-kinds))) OP-SEQ)))

(def get-ops (memoize get-ops-worker))

(defn get-kinds [expr]
  (cond
   (seq? expr) (.-out-kinds (OPS (first expr)))
   (string? expr) #{::str}
   true #{::num}))

(defn get-arity-num [arity]
  (case arity
    ::const -1
    ::one 1
    ::two 2
    ::many 2))

(defn get-inputs [params]
  (subvec params 0 (dec (count params))))

(defn get-output [params]
  (nth params (dec (count params))))

(defn name-inputs [param-lists]
  (->> param-lists first get-inputs ((comp range count)) (map #(symbol (str "x" %)))))

(defn get-rand [nums constants]
  (rand-nth (if (> (rand) 0.2) nums constants)))

(defn test-fun [fun params]
  (let [inputs (get-inputs params)
        output (get-output params)
        res (apply fun inputs)]
    (or (= res output)
        (= res (str output)))))

(defn extract-args [op sorting expr]
  (if (and (seq? expr) (= (first expr) op))
    ((if sorting (partial sort-by str) identity) (rest expr))
    [expr]))

(defn partial-evaluate [expr]
  (let [op (first expr)
        args (rest expr)
        parts (group-by constant? args)
        constants (parts true)
        non-constants (parts false)
        const-expr (eval-expr op constants)
        args (sort-by str (conj non-constants const-expr))]
    (apply list op args)))

(defn simplify-iter [expr]
  (let [op (when (seq? expr) (first expr))]

    ;; We have a generic simplification involving any operation with only constants
    (cond (and op (every? constant? (rest expr)))
            (try
              (eval-expr op (rest expr))
              (catch Throwable _
                (println "Oops, could not evaluate constant expression " expr)
                expr)) ;; TODO: should perhaps include some other code here, since this will fail in runtime
          (and op (.-associative (OPS op)) (.-commutative (OPS op))
               (>= (count (filter constant? (rest expr))) 2))
            (partial-evaluate expr)
          true
            (let [fargs (when (and op (.-associative (OPS op)))
                          ((if (.-commutative (OPS op))
                            (partial sort-by str)
                            identity)
                            (apply concat (pmap (partial extract-args op true) (rest expr)))))
                  expr (if fargs (conj fargs op) expr)
                  f (when (seq? expr) (second expr))
                  s (when (and (seq? expr) (> (count expr) 2)) (nth expr 2))]
              (case op
                + (cond
                   (= f 0) s
                   (= s 0) f
                   (= f s) (list '* 2 f)
                   true expr)
                - (cond
                   (= f s) 0
                   (and (seq? f) (not s) (= (first f) '-) (= (count f) 2)) (second f)
                   (= s 0) f
                   true expr)
                * (cond
                   (= f 1) s
                   (= s 1) f
                   (= f 0) 0
                   (= s 0) 0
                   true expr)
                / (cond
                   (= s 1) f
                   (= f 0) 0
                   (= f s) 1
                   (= s 0) f ;; yeah, we skip divide by zeroes...
                   true expr)
                str expr
                expr)))))

(defn simplify-worker [expr]
  (loop [e expr]
    (let [sexpr (simplify-iter e)]
      (if (= sexpr e)
        e
        (recur sexpr)))))

(def simplify (memoize simplify-worker))

(defn get-rand-arity [kinds]
  (let [prob (rand)]
    (cond (empty? (intersection kinds #{num})) ::two ;; TODO: this forces a binary string operator
          (< prob CONST-PROB) ::const
          (> prob (- 1.0 BINARY-PROB)) ::two
          true ::one)))

(defn create-expr [nums levels & {:keys [kinds] :or {kinds #{::str ::num}}}]
  (let [arity (if (zero? levels) ::const (get-rand-arity kinds))
        ops (when (or (not= arity ::const) (>= (rand) PARAM-PROB)) (get-ops arity kinds))
        op (when-not (empty? ops) (rand-nth ops))
        expr (cond
               (not op) (rand-nth nums) ;; should check or use type here
               (= arity ::const) op
               true (let [in-kinds (.-in-kinds (OPS op))
                          arity-num (get-arity-num arity)
                          exprs (pmap (fn [_] (create-expr nums (dec levels) :kinds in-kinds)) (range arity-num))]
                      (apply list op exprs)))
        simp-expr (simplify expr)]
      simp-expr))

(defn beautify [fun-form]
  (let [args (second fun-form)
        expr (nth fun-form 2)
        beauty
          (if-let [op (when (seq? expr) (first expr))]
            (let [arg-count (when (seq? expr) (dec (count expr)))
                  f (when (> arg-count 0) (nth expr 1))
                  s (when (> arg-count 1) (nth expr 2))]
              (case op
                + (cond
                     (and (= arg-count 2) (= f 1)) `(inc ~s)
                     (and (seq? f) (= (count f) 2) (= (first f) '-) (= arg-count 2)) (list '- s (second f))
                     true expr)
                - (cond
                     (and (= arg-count 2) (= s 1)) `(dec ~f)
                     true expr)
                expr))
            expr)]
    (list 'fn args beauty)))

(defn create-random-algo [num-lists]
  (let [params (name-inputs num-lists)
        levels MAX-LEVELS]
    `(~'fn [~@params] ~(create-expr params levels))))

(defn create-algo [num-lists]
  (init-counters)
  (beautify
    (loop [used-expr #{}]
      (let [fun-form (create-random-algo num-lists)
            already-used (used-expr fun-form)
            fun (when-not already-used (eval fun-form))]
        (if already-used
          (do
            (println (pr-str fun-form) " already used...")
            (recur used-expr))
          (do
            (println "trying with: " (pr-str fun-form) "...")
            (if-let [found-fun
              (try
                (if (every? (partial test-fun fun) num-lists)
                  fun-form
                  (println "Nope, so continue"))
                (catch Throwable ex
                  (println "Error, so continue.. [" (.getMessage ex) "]")))]
              found-fun
              (do
                (swap! EXPR-COUNTER inc)
                 (when (<= @EXPR-COUNTER MAX-ITERS) (recur (conj used-expr fun-form)))))))))))

(defn get-algo [input-text]
  (let [lines (string/split-lines input-text)
        num-lists (map (fn [num-list] (map #(Integer/parseInt %) (string/split num-list #"\s+"))) lines)]
    (create-algo num-lists)))

(create-algo [[1 5 6] [10 2 4]])

@EVAL-COUNTER
@EXPR-COUNTER
