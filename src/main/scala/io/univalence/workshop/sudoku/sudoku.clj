(ns sudoku)


(do :queue

    "a queue that do not accept elements that are already in it"

    (defn qonj [q x]
      (if-not (contains? (set q) x)
        (conj q x)
        q))

    (defn queue [& xs]
      (assert (= (count xs) (count (set xs))))
      (reduce conj clojure.lang.PersistentQueue/EMPTY xs))

    (defn qoncat [q xs]
      (reduce qonj q xs))

    (defn unqons [q]
      (when-not (empty? q)
        ((juxt peek pop) q)))

    (defmethod print-method clojure.lang.PersistentQueue [q w]
      (print-method (cons 'queue (seq q)) w)))

(do :sudoku

    (def empty-grid
      (vec (repeat 9 (vec (repeat 9 '_)))))

    (defn square-positions
      "return the sequence of positions contained in the 3*3 square constaining the given position"
      [[x y]]
      (for [x (take 3 (iterate inc (* (quot x 3) 3)))
            y (take 3 (iterate inc (* (quot y 3) 3)))]
        [x y]))

    (def sudoku-constraints
      (reduce (fn [cs [x y :as pos]]
                (into cs
                      (concat
                        ;; column
                        (map (fn [n] [:!= #{pos [n (pos 1)]}]) (remove (partial = x) (range 9)))
                        ;; row
                        (map (fn [n] [:!= #{pos [(pos 0) n]}]) (remove (partial = y) (range 9)))
                        ;; square
                        (map (fn [pos2] [:!= #{pos pos2}]) (remove (partial = pos) (square-positions pos))))))
              #{}
              (for [x (range 9)
                    y (range 9)]
                [x y])))

    (defn get-domain [state pos]
      (get-in state [:vars pos]))

    (defn singleton? [dom]
      (when (= 1 (count dom))
        (first dom)))

    (defn grid->variables
      "given a sudoku grid, build the :vars map of a sudoku-state"
      [grid]
      (into {}
            (mapcat (fn [row y]
                      (map (fn [v x] [[x y]
                                      (if (= '_ v) (set (range 1 10)) (hash-set v))])
                           row (range)))
                    grid
                    (range))))

    (defn grid->state
      [grid]
      (let [vars (grid->variables grid)
            digits-positions (map first (filter (comp singleton? val) vars))]
        {:vars vars
         :queue (apply queue digits-positions)
         :constraints sudoku-constraints}))

    (defn state->grid
      [{:as state :keys [vars]}]
      (reduce (fn [grid [pos dom]]
                (assoc-in grid (reverse pos) (first dom)))
              empty-grid
              vars))

    (defn refine-domain [state pos new-dom]
      (let [current-dom (get-domain state pos)]
        (if (= new-dom current-dom)
          state
          (-> state
              (assoc-in [:vars pos] new-dom)
              (update :queue qonj pos)))))

    (defn remove-constraint [state c]
      (update state :constraints disj c))

    (defn run-constraint
      [state [verb args :as constraint]]
      ;; for now we have only one type of constraint (disequality)
      (case verb
        :!= (let [[pos1 pos2] (seq args)
                  dom1 (get-domain state pos1)
                  dom2 (get-domain state pos2)
                  val1 (singleton? dom1)
                  val2 (singleton? dom2)]
              (cond
                val1 (-> state
                         (refine-domain pos2 (disj dom2 val1))
                         (remove-constraint constraint))
                val2 (-> state
                         (refine-domain pos1 (disj dom1 val2))
                         (remove-constraint constraint))
                :else state))))

    (defn step
      [{:as state :keys [queue constraints]}]
      (when (not (empty? queue))
        (let [[pos next-queue] (unqons queue)
              runnable-constraints (filter (fn [[_ args]]
                                             (contains? (set args) pos))
                                           constraints)]
          (reduce run-constraint
                  (assoc state :queue next-queue)
                  runnable-constraints))))

    (defn step-rec
      [state]
      (if-let [next-state (step state)]
        (step-rec next-state)
        state))
    )


(do :demo

    (def grid
      '[[1 _ _ _ _ _ 9 _ _]
        [_ 4 _ _ 7 3 _ 8 _]
        [_ _ 7 6 _ _ 3 _ 5]
        [_ _ 1 4 _ _ _ _ _]
        [4 6 2 _ 8 _ 7 9 1]
        [_ _ _ _ _ 2 6 _ _]
        [7 _ 9 _ _ 6 4 _ _]
        [_ 8 _ 7 5 _ _ 6 _]
        [_ _ 4 _ _ _ _ _ 7]])

    (-> (grid->state grid)
        step-rec
        state->grid)

    ;; result :
    
    [[1 3 6 5 2 8 9 7 4]
     [9 4 5 1 7 3 2 8 6]
     [8 2 7 6 4 9 3 1 5]
     [3 9 1 4 6 7 5 2 8]
     [4 6 2 3 8 5 7 9 1]
     [5 7 8 9 1 2 6 4 3]
     [7 1 9 8 3 6 4 5 2]
     [2 8 3 7 5 4 1 6 9]
     [6 5 4 2 9 1 8 3 7]])
