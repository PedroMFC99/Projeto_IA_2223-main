;;;; procura.lisp
;;;; IA - 2022/ 2023
;;;; Diogo Letras - 202002529 & Pedro Cunha - 202000757
;;;; Implementa os algorithms de procura (independentemente do dominio da aplicacao)

;;; Successors
   
;;successors
(defun successors (node operators search-algorithm depth heuristic-function num-boxes-to-close) 
"Return a list of successors of a given node"
	(let* 	(
				(operator (first operators))
				(num-lines (num-lines-board (get-node-state node)))
				(num-columns (num-columns-board (get-node-state node)))
				(list-possible-lines-columns
					(cond 
						((eql operator 'insert-horizontal-arc) (reverse (combination-list (+ num-lines 1) num-columns)))
						((eql operator 'insert-vertical-arc)   (reverse (combination-list (+ num-columns 1) num-lines)))
					)
				)
			)
			(cond
				((null operators) nil)
				((and (equal 'dfs search-algorithm) (= (get-node-depth node) depth)) nil)
				(T (append
						(get-all-possible-successors node operator list-possible-lines-columns heuristic-function num-boxes-to-close)
						(successors node (rest operators) search-algorithm depth heuristic-function num-boxes-to-close)
					)
				)
			)
	)
)

;;; Aux functions of successors

;;get-all-possible-successors
(defun get-all-possible-successors (node operator possibilities heuristic-function num-boxes-to-close)
"Returns the possibilities of sucessors of a given node"
	(let* 	(
				(first-possibility (first possibilities))
				(valid-possibilities (not (null possibilities)))
				(result
					(cond
						(valid-possibilities (check-possible-successors node (list operator first-possibility) heuristic-function num-boxes-to-close))
						(T nil)
					)
				)
				(final-result
					(cond
						((null result) nil)
						(T (list result))
					)
				)
			)
		  
		(cond
			((null possibilities) nil)
			(T (append final-result (get-all-possible-successors node operator (rest possibilities) heuristic-function num-boxes-to-close)))
		)
	)
)

;;check-possible-successors
(defun check-possible-successors (node list-operator-parameters heuristic-function num-boxes-to-close)
"Auxiliary function to check the valid successors possibilities of a given node"
	(let* 		(
					(operator (first list-operator-parameters))
					(board (get-node-state node))
					(parameters (append (second list-operator-parameters) (list board)))
					(operation-result (apply operator parameters))
					(result (create-node operation-result (+ 1 (get-node-depth node))
								(cond
									((not (null heuristic-function)) (funcall heuristic-function board num-boxes-to-close))
									(T nil)
								)
								node
							)
					)
				)
				(cond
					((null operation-result) nil)
					(T result)
				)
	)
)

;;combination-list
(defun combination-list (max-lines max-columns) 
"Returns sub-lists with all possible combinations of a number of lines and a number of columns"
	(cond
		((zerop max-lines) nil)
		(T (append (num-list-combinations max-lines (number-list max-columns)) (combination-list (- max-lines 1) max-columns)))
	)
)

;; num-list-combinations
(defun num-list-combinations (num list) 
"Returns sub-lists with all combinatons of the num received and each elem of the list"
	(let 	(
				(last-element (first (last list)))
			) 
			(cond
				((null list) nil)
				(T
					(cons
						(list num last-element)
						(num-list-combinations num (reverse (rest (reverse list))))
					)
				)
			)
	)
)

;; number-list
(defun number-list (dimension &optional (default-value 1))
"Returns a list with the received size as a parameter"
  (if (= dimension 0)
		nil
		(cons default-value (number-list (- dimension 1) (+ default-value 1)))
  )
)

;;; Search Algorithm Implementation
(defun generic-search (initial-node max-depth f-solution f-successors f-algorithm operator-list f-heuristic num-boxes-to-close &aux (init-time (get-universal-time)))
"Generic search algorithm, used to search for a solution in state space. Used in combination with bf-search and df-search. It may require a depth in case of df-search"
    (let    (
                (unvisited (list initial-node))
                (visited nil)
            )
            (generic-search-helper unvisited visited f-solution f-successors f-algorithm operator-list f-heuristic num-boxes-to-close max-depth init-time)
    )
)

;;; Auxiliar Function of generic-search
(defun generic-search-helper (unvisited visited f-solution f-successors f-algorithm operator-list f-heuristic num-boxes-to-close max-depth init-time)
    (if (null unvisited)
        nil
        (let    (
                    (cur-node (first unvisited))
                )
                (if (funcall f-solution cur-node num-boxes-to-close)
                    (list cur-node (length unvisited) (length visited) (- (get-universal-time) init-time))
                    (if (existp cur-node visited f-algorithm)
                        (generic-search-helper (rest unvisited) visited f-solution f-successors f-algorithm operator-list f-heuristic num-boxes-to-close max-depth init-time)
                        (let    (
                                    (list-successors (funcall f-successors cur-node operator-list f-algorithm max-depth f-heuristic num-boxes-to-close))
                                )
                                (let    (
                                            (solution (exist-solution list-successors f-solution f-algorithm num-boxes-to-close))
                                        )
                                        (if solution
                                            (append (list solution) (list (+ (length unvisited) (length list-successors)) (length visited) (- (get-universal-time) init-time)))
                                            (let 	(
														(new-unvisited (funcall f-algorithm (rest unvisited) list-successors))
													)
                                            		(generic-search-helper new-unvisited (cons cur-node visited) f-solution f-successors f-algorithm operator-list f-heuristic num-boxes-to-close max-depth init-time)
											)
                                        )
                                )
                        )
                    )
                )
        )
    )
)

;;; Algorithms

;; Breadht-First (Procura em largura)
(defun bfs (unvisited successors)
"BF-Search"
	(append unvisited successors)
)

;; Depth-First (Procura em depth)
(defun dfs (unvisited successors)
"DF-Search"
	(append successors unvisited)
)

;; A*
(defun a-asterisc (unvisited successors)
"A-asterisc Search"
	(sort (append unvisited successors) #'< :key #'get-node-cost)	
)
 
;;; Validation functions for generic-search algorthim

;;existp
 (defun existp (node node-list algorithm)
 "Returns true if a node exists in a list of nodes. Used in df-search algoritm."
    (let* 	(
				(compared-node (existp-aux node node-list))
             	(valid (not (null compared-node)))
			)
        (cond 
            ((and (eql algorithm 'dfs) valid (= (get-node-depth node) (get-node-depth compared-node))) T) ; algorithm dfs
            ((and (eql algorithm 'bfs) valid) T)
			((and (eql algorithm 'a-asterisc) valid) T)
            (T nil)
        )
    )
)

;existp-aux 
(defun existp-aux (node node-list)
"Checks if a node exists in a list of nodes."
    (cond
        ((null node-list) nil)
        ((equal (get-node-state node) (get-node-state (first node-list)))(first node-list))
        (T (existp-aux node (rest node-list)))
    )
) 
 
;; exist-solution
(defun exist-solution (list f-solution f-algorithm num-boxes-to-close) 
"Checks if a solution exists in a list of succesors. Used in df-search algorithm."
	(cond
		((not (eql f-algorithm 'dfs)) nil)
		((null list) nil)
		((funcall f-solution (first list) num-boxes-to-close) (first list))
		(T (exist-solution (rest list) f-solution f-algorithm num-boxes-to-close))
	)
)

;;; Extra functions (used to calculate algorithim's performance)

;;penetrance
(defun penetrance (node generated-nodes)
"Returns the value of penetrance"
	(when (not (equal generated-nodes 0))
			(float (/ (get-node-depth node) generated-nodes))
	)
)

;;branching-factor 
(defun branching-factor (L t-value  &optional (margin-of-error 0.1) (bmin 1) (bmax 10e11)) 
"Returns the value of branching factor"
    (let* 	(
				(bavg (/ (+ bmin bmax) 2))
			)
			(cond 
				((< (- bmax bmin) margin-of-error) (/ (+ bmax bmin) 2))
				((> (- (f-polynomial L bavg) t-value) margin-of-error) (branching-factor L t-value margin-of-error bmin bavg))
				(T (float(branching-factor L t-value margin-of-error bavg bmax)))
			)
    )
)

;;; f-polynomial
(defun f-polynomial (polynomial x)
"Aux function used to calculate the branching factor"
    (cond 
        ((= polynomial 1) x)
        (t (+ (expt x polynomial) (f-polynomial (- polynomial 1) x)))
    )
)
