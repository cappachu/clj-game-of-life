(ns game-of-life.core
  (:gen-class))



;; The game of life is a cellular automaton devised by mathematician John Conway.

;; The 'board' consists of both live (#) and dead ( ) cells. Each cell interacts with its eight neighbours (horizontal, vertical, diagonal), and its next state is dependent on the following rules:

;; 1) Any live cell with fewer than two live neighbours dies, as if caused by under-population.
;; 2) Any live cell with two or three live neighbours lives on to the next generation.
;; 3) Any live cell with more than three live neighbours dies, as if by overcrowding.
;; 4) Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

;; Write a function that accepts a board, and returns a board representing the next generation of cells.


;;=====================================================


(defn board-next-state [board]
  (let [alive? #(= % \#)
        rows (count board)
        cols (count (first board))
        lookup (fn [board row col]
                 (-> board (get row) (get col \space)))

        neighbors (fn [board row col]
                    (for [r [(- row 1) row (+ row 1)]
                          c [(- col 1) col (+ col 1)]
                          :when (or (not= r row) (not= c col))]
                      (lookup board r c)))

        cell-next-state (fn [board row col]
                          (let [cell-state (lookup board row col)
                                num-live-neighbors (count (filter #(= % \#) (neighbors board row col)))]
                            (if (= cell-state \#)
                              (if (#{2 3} num-live-neighbors) \# \space)
                              (if (= 3 num-live-neighbors) \# \space))))
        ]
    (vec
     (map clojure.string/join
          (partition rows
                     (for [r (range rows)
                           c (range cols)]
                       (cell-next-state board r c)))))))

;;=====================================================

(def board ["      "
            " ##   "
            " ##   "
            "   ## "
            "   ## "
            "      "])

(defn print-board [board]
  (dotimes [row (count board)]
    (println "[" (nth board row) "]")))

(defn print-steps [board steps]
  (print-board board)
  (println "\n")
  (if (= steps 0)
    (println "done.")
    (recur (board-next-state board) (dec steps))))


(defn -main
  [& args]
  (println "Conway's Game of Life"))
