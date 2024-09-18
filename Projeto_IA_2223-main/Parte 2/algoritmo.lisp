;;;; algoritmo.lisp
;;;; IA - 2022/ 2023
;;;; Diogo Letras - 202002529 & Pedro Cunha - 202000757
;;;; Implementa o algoritmo, junto com as funcoes auxiliares

;;; Variáveis Globais
(defvar *alpha-prune* 0)
(defvar *beta-prune* 0)
(defvar *pc-play* nil) 
(defvar *analyzed-nodes* 0)

;;; Alpha-beta algorithm
;alpha-beta
(defun alpha-beta (node limit-depth piece f-evaluation &optional (alpha most-negative-fixnum) (beta most-positive-fixnum)  (init-time (get-universal-time))(max-time 5000))
"Alpha beta algorithm, used along alpha-beta-max and alpha-beta-min, depending on the depth of the node."
	(let*(	
			(piece-to-play (cond ((= (get-node-depth node) 0) piece) (T (switch-piece piece))))
			(max-min (check-player-depth node))
			(boxes-player-1	(get-boxes-player-1 node))
			(boxes-player-2 	(get-boxes-player-2 node))
			(actual-time 		(get-universal-time))
			(spent-time 		(- actual-time init-time))
		)

		(cond
			(	(or
					(= limit-depth (get-node-depth node))
				)
				(progn
					(setf *analyzed-nodes* (+ *analyzed-nodes* 1))
										(get-node-evaluation node)			
				)	
			)

			(
				(eq max-min 'MAX)
				(alpha-beta-max (successors-alphabeta node (operators) limit-depth piece f-evaluation boxes-player-1 boxes-player-2) limit-depth piece-to-play f-evaluation alpha beta init-time max-time)
			)
			(T
				(alpha-beta-min (successors-alphabeta node (operators) limit-depth piece f-evaluation boxes-player-1 boxes-player-2) limit-depth piece-to-play f-evaluation alpha beta init-time max-time)
			)
		)
	)
)

;alpha-beta-max
(defun alpha-beta-max (successors limit-depth piece f-evaluation alpha beta init-time max-time)
"Function used to discover the biggest value between the node value and value of successors"
	(cond
		((null successors) alpha)
		(T (let*((new-piece (switch-piece piece))
					(evaluation-value-node (alpha-beta (car successors) limit-depth new-piece f-evaluation alpha beta init-time max-time))
					(new-alpha (verify-biggest-successor alpha evaluation-value-node (car successors))))
			(cond
				((<= beta new-alpha) (setf *beta-prune* (+ *beta-prune* 1)) beta) ; houve corte alpha
				(T (alpha-beta-max (cdr successors) limit-depth piece f-evaluation new-alpha beta init-time max-time))
			)
		))
	)
)

;alpha-beta-min
(defun alpha-beta-min (successors limit-depth piece f-evaluation alpha beta init-time max-time) 
"Function used to discover the smallest value between the node value and value of successors"
	(cond
		((null successors) beta)
		(T (let*((new-piece (switch-piece piece))
					(evaluation-value-node (alpha-beta (car successors) limit-depth new-piece f-evaluation alpha beta init-time max-time))
					(new-beta (verify-smallest-successor beta evaluation-value-node)))
			(cond
				((<= new-beta alpha) (setf *alpha-prune* (+ *alpha-prune* 1)) alpha) ; houve corte beta
				(T (alpha-beta-min (cdr successors) limit-depth piece f-evaluation alpha new-beta init-time max-time))
			)
		))
	)
)

;verify-biggest-successor
(defun verify-biggest-successor (alpha evaluation-value sucessor) 
"Aux function of alpha-beta-max, verifies if evaluation value is bigger than beta value"
	(cond
		((> evaluation-value alpha) (setf *pc-play* sucessor) evaluation-value)
		(t alpha)
	)
)

;verify-smallest-successor
(defun verify-smallest-successor (beta evaluation-value)
"Aux function of alpha-beta-min, verifies if evaluation value is smaller than beta value"
	(cond
		((< evaluation-value beta) evaluation-value)
		(t beta)
	)
)

;funçao utiilidade
(defun evaluation-function (node piece old-evaluation closed-boxes-j1 closed-boxes-j2 old-number-boxes-j1 old-number-boxes-j2)
"Evaluation function that makes determines the score of each player."	
	(let* ((number-closed-boxes (num-closed-boxes (get-node-state node)))
			 (board (get-node-state node))
			 (winner-result (winner-p board number-closed-boxes piece closed-boxes-j1 closed-boxes-j2)))

		(cond
			(winner-result (cond ((= winner-result *player2*) 1000) (T -1000)))
			(T 
				(cond
					((> closed-boxes-j2 old-number-boxes-j2) (+ old-evaluation 25))	 
					((> closed-boxes-j1 old-number-boxes-j1) -25)
					(T old-evaluation)
				)
			)
		)
	)
)

;check-player-depth
(defun check-player-depth(node) 
"Function that verifies if the player is in MIN or MAX"
	(let ((depth (get-node-depth node)))
		(cond
			((or (evenp depth) (= depth 0)) 'MAX)
			(t 'MIN)
		)
	)
)

;check-if-box-closed
(defun check-if-box-closed (node previous-number-closed-boxes) 
"Auxiliary function of succesors-alphabeta to verify if a box is effectively closed"
	(let ((actual-closed-boxes (num-closed-boxes (get-node-state node))))
		(> actual-closed-boxes previous-number-closed-boxes)
	)
)

;;; Succesors

;successors-alphabeta
(defun successors-alphabeta (node operators depth piece evaluation-function closed-boxes-j1 closed-boxes-j2)
"Succesors in alpha-beta algorithm"
	(let* ((number-closed-boxes (num-closed-boxes (get-node-state node)))
		     (successors-result (successors node operators piece depth evaluation-function closed-boxes-j1 closed-boxes-j2))
		     (new-successors (apply 'append 
											(mapcar #'(lambda (nodex)
																	(let ((closed-box (check-if-box-closed nodex number-closed-boxes)))

																		(cond
																			((null closed-box) (list nodex))
																			(T (let* ((num-closed-boxes-player-1 (cond ((= piece *player1*) (+ closed-boxes-j1 1)) (T closed-boxes-j1)))
																						 (num-closed-boxes-player-2 (cond ((= piece *player2*) (+ closed-boxes-j2 1)) (T closed-boxes-j2)))
																						 (new-successors (successors nodex operators piece depth evaluation-function num-closed-boxes-player-1 num-closed-boxes-player-2)))
																				(cond
																					((null new-successors) (list nodex))
																					(T new-successors))))))
																)successors-result)))
		 )
		new-successors
	)
)

;successors
(defun successors (node operators piece max-depth evaluation-function closed-boxes-j1 closed-boxes-j2)
"Successors function"
	(let* ((operator (car operators))
		   (number-lines (num-lines-board (get-node-state node)))
		   (number-columns (num-columns-board (get-node-state node)))
		   (list-possible-lines-columns (cond 
											((eql operator 'insert-horizontal-arc) (reverse (combinations-list (+ number-lines 1) number-columns)))
											((eql operator 'insert-vertical-arc)   (reverse (combinations-list (+ number-columns 1) number-lines))))))
		(cond
			((null operators) nil)
			((= (get-node-depth node) max-depth) nil)
			(T (append 
				(get-all-possible-successors node operator piece list-possible-lines-columns evaluation-function closed-boxes-j1 closed-boxes-j2)
				(successors node (cdr operators) piece max-depth evaluation-function closed-boxes-j1 closed-boxes-j2)
				))
		)
	)
)

; get-all-possible-successors
(defun get-all-possible-successors (node operator piece possibilities evaluation-function closed-boxes-j1 closed-boxes-j2)
"Returns the possibilities of successors of a given node"
	(let* ((first-possiblity (car possibilities))
		   (valid-possibilities (not (null possibilities)))
		   (result (cond (valid-possibilities (check-possible-succesors node (list operator (append first-possiblity (list piece))) piece evaluation-function closed-boxes-j1 closed-boxes-j2)) (T nil)))
		   (final-result (cond ((null result) nil) (T (list result)))))
		  
		(cond
			((null possibilities) nil)
			
			(T (append final-result (get-all-possible-successors node operator piece (cdr possibilities) evaluation-function closed-boxes-j1 closed-boxes-j2)))
		)
	)
)

; check-possible-succesors
(defun check-possible-succesors (node list-operator-parameters piece evaluation-function closed-boxes-j1 closed-boxes-j2)
"Auxiliary function to check the valid successors possibilities of a given node"
	(let* ((operator (car list-operator-parameters))
			(parent-board (get-node-state node))
			(parameters (append (cadr list-operator-parameters) (list parent-board)))
			(generated-board (apply operator parameters))
			(old-evaluation (get-node-evaluation node))
			(old-number-boxes-j1 (get-boxes-player-1 node))
			(old-number-boxes-j2 (get-boxes-player-2 node)))

		(cond
			((null generated-board) nil)
			(T (let* ((number-closed-boxes-generated-board (num-closed-boxes generated-board))
						 (number-boxes-player-1 (cond ((= piece *player1*) (- number-closed-boxes-generated-board closed-boxes-j2)) (T closed-boxes-j1)))
						 (number-boxes-player-2 (cond ((= piece *player2*) (- number-closed-boxes-generated-board closed-boxes-j1)) (T closed-boxes-j2)))
						 (depth  (+ 1 (get-node-depth node)))
						 (evaluation-value (funcall evaluation-function node piece old-evaluation number-boxes-player-1 number-boxes-player-2 old-number-boxes-j1 old-number-boxes-j2)))
				
				(create-node generated-board depth evaluation-value number-boxes-player-1 number-boxes-player-2))))
	)
)

;;combinations-list
(defun combinations-list (max-lines max-columns) 
"Returns sub-lists with all possible combinations of a number of lines and a number of columns"
	(cond
		((zerop max-lines) nil)
		(T (append (number-list-combinations max-lines (number-list max-columns)) (combinations-list (- max-lines 1) max-columns)))
	)
)

;; number-list-combinations
(defun number-list-combinations (number list) 
"Returns sub-lists with all combinatons of the num received and each elem of the list"
	(let ((last-element (car (last list)))) 
		
		(cond
			((null list) nil)
			(T (cons (list number last-element) (number-list-combinations number (reverse (cdr (reverse list))))))
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