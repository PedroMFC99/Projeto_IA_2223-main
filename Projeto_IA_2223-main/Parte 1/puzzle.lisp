;;;; puzzle.lisp
;;;; IA - 2022/ 2023
;;;; Diogo Letras - 202002529 & Pedro Cunha - 202000757
;;;; Implementa a resolucao do problema, incluindo a definicao dos operadores e a heuristica, que sao especificos do dominio de aplicacao

;;test-board (ignore)
(defun test-board ()
"Returns 3x3 board (3 horizontal arcs x 3 horizontal arcs)"
  (list
    '((0 0 0) (0 0 1) (0 1 1) (0 0 1))
    '((0 0 0) (0 1 1) (1 0 1) (0 1 1))
  )
)

;;; Constructor

;; create-node
(defun create-node (state &optional (depth 0) (heuristic nil) (node-parent nil)) 
"Create a list that represents a node
A node is composed by:
- state of the board (mandatory parameter),
- depth at which the node is (by default value 0),
- heuristic of the node (by default value nil),
- parent node, (by default value nil)"
	(list state depth heuristic node-parent)
)

;;; Getters

;;get-node-state
(defun get-node-state (node) 
"Returns the state of the node, which is represented by the board"
	(first node)
)

;;get-node-depth
(defun get-node-depth (node) 
"Returns the depth at which the node is located"
	(second node)
)

;;get-node-heuristic
(defun get-node-heuristic (node) 
"Returns the heuristic of the node"
	(third node)
)

;;get-node-parent
(defun get-node-parent (node) 
"Returns the parent node of this node"
	(fourth node)
)

;;get-node-cost
(defun get-node-cost (node)
"Returns the value of the cost of the node (f) which is the sum of the value of the depth (g) and the heuristic (h)"
  ;(format t "~s" (get-node-heuristic node))
  (+ (get-node-depth node)(get-node-heuristic node))
)

;;get-horizontal-arcs
(defun get-horizontal-arcs (board)
 "Returns the list of horizontal arcs on the board"
	(first board)
)

;;get-vertical-arcs
(defun get-vertical-arcs (board) 
"Returns the list of vertical arcs on the board"
	(second board)
)

;;; Aux functions

;;insert-arc-in-position
(defun insert-arc-in-position (index list) 
"Insert an arc (represented by the value [1]) at the index of the received list"
  (if (null list)
    nil
    (if (= index 1)
      (cons 1 (rest list))
      (cons (first list) (insert-arc-in-position (- index 1) (rest list)))
    )
  )
)

;;insert-arc-in-position-final
;;arc-in-position-final
(defun insert-arc-in-position-final (line column list)
"Insert an arc (represented by the value [1]) into a list representing the set of arcs of a board."
  (if (null list)
    nil
    (if (= line 1)
      (let  (
              (new-line (insert-arc-in-position column (first list)))
            )
            (cons new-line (rest list))
      )
      (cons (first list) (insert-arc-in-position-final (- line 1) column (rest list)))
    )
	)
)

;;verify-add-arc-aux 
(defun verify-add-arc-aux (index list)
"Receives an index and a list and returns the arc in that index position"
  (if (null list)
    nil
    (if (and (= index 1) (eql (first list) 0))
      1
      (verify-add-arc-aux (- index 1) (rest list))
    )
  )
)

;;verify-add-arc
(defun verify-add-arc (line column list)
"Receives a row index, column index, and a list of horizontal arcs or vertical arcs.
Checks if the value is [1] at that position, if it is returns 0, if it is [0] returns 1"
 (if (null list)
    nil
    (if (= line 1)
      (verify-add-arc-aux column (first list))
      (verify-add-arc (- line 1) column (rest list))
    )
  )
)

;;; Operators

;;insert-horizontal-arc
(defun insert-horizontal-arc (line column board)
"Insert a horizontal arc (represented by the value [1]) into a board passed as an argument"
  (let  (
          (horizontal-arcs (get-horizontal-arcs board))
          (vertical-arcs (get-vertical-arcs board))
        )
        (when
          (and board (verify-add-arc line column horizontal-arcs))
          (cons (insert-arc-in-position-final line column horizontal-arcs) (list vertical-arcs))
        )
  )
)

;;insert-vertical-arc
(defun insert-vertical-arc (line column board) 
"Insert a vertical arc (represented by the value [1]) into a board passed as an argument"
  (let  (
          (horizontal-arcs (get-horizontal-arcs board))
          (vertical-arcs (get-vertical-arcs board))
        )
        (when
          (and board (verify-add-arc line column vertical-arcs))
          (cons horizontal-arcs (list (insert-arc-in-position-final line column vertical-arcs)))
        )
  )
)

;;operators 
(defun operators () 
"Create a list with all the operators of the problem"
	(list 'insert-horizontal-arc 'insert-vertical-arc)
)		

;num-lines-board
(defun num-lines-board (board)
"Return the number of lines of a board, that is, the number of lines between the points"
	(- (length (get-vertical-arcs board)) 1)
)

;;num-columns-board
(defun num-columns-board (board)
"Return the number of lines of a board, that is, the number of columns between the points"
	(- (length (get-horizontal-arcs board)) 1)
)

;;; Puzzle Functions - Validation of closed boxes

;;num-closed-boxes
(defun num-closed-boxes (board) 
  "Return the number of closed boxes on the board."
  (if (null board)
    nil
    (verify-box-closed (num-closed-boxes-aux (get-horizontal-arcs board)(get-headers-by-column board)))
  )
)

;;num-closed-boxes-aux
(defun num-closed-boxes-aux (harcs varcs) 
"Receive the horizontal and vertical arcs, and apply an auxiliary function in order to join all the boxes in a list."
  (if (or (null harcs) (null varcs))
    nil
    (let  (
            (boxes (num-closed-boxes-aux2 (first harcs) (second harcs) (first varcs)))
          )
          (append boxes (num-closed-boxes-aux (rest harcs) (rest varcs)))
    )
  )
)
  
;;num-closed-boxes-aux2
(defun num-closed-boxes-aux2(line1 line2 column)
"Returns a list of the 'first' of the 1st line, the 'first' of the 2nd line, and the 'first' of the rest of the column."
"Returns the list of boxes of the two lines and the two columns."
  (if (and line1 line2 column)
    (cons
      (list (first line1) (first line2) (first column) (first (rest column)))
      (num-closed-boxes-aux2 (rest line1) (rest line2) (rest column))
    )
    nil
  )
)

;;get-headers-by-column
(defun get-headers-by-column(board)
"Returns the lists of each header of each column"
	(counter 0 (num-columns-board board) 'get-headers-list-aux (get-vertical-arcs  board)) 
)

(defun get-headers-list-aux (list n)
"Receives the headers of a vertical or horizontal line, and from there creates a new list."
  (if (null list)
    nil
    (cons
      (nth n (first list))
      (get-headers-list-aux (rest list) n)
    )
  )
)

(defun counter (i dimensionMax function &rest args)
"Performs a certain action throughout a list when the index is equal to the dimension of the list."
  (if (= i dimensionMax)
    nil
    (let  (
            (arg (first args))
          )
          (cond
            ((eq function 'get-headers-list-aux)
              (cons (get-headers-list-aux arg i)
                (counter (+ i 1) dimensionMax function arg)
              )
            )
            (t (error "Oops! Unrecognized function: %s" function))
          )
    )
  )
)

;; verify-box-closed
(defun verify-box-closed (list) 
"Counts if the box is closed or not."
  (if (null list)
    0
    (let  (
            (item (first list))
            (rest (rest list))
          )
          (if (= (count-zeros-list item) 0)
            (+ 1 (verify-box-closed rest))
            (verify-box-closed rest)
          )
    )
  )
)

;; count-zeros-list
(defun count-zeros-list (list) 
"Counts the number of '0's in a list."
   (length (remove-if-not #'zerop list))
)

;; count-ones
;(defun count-ones (lst)
;  "Count the number of ones in a list of lists."
;  (if (null lst)
;      0
;      (+ (count-ones-in-sublist (car lst)) (count-ones (cdr lst)))
;  )
;)

;(defun count-ones-in-sublist (sublst)
;  "Count the number of ones in a sublist."
;  (if (null sublst)
;    0
;    (if (eq (car sublst) 1)
;      (+ 1 (count-ones-in-sublist (cdr sublst)))
;      (count-ones-in-sublist (cdr sublst))
;    )
;  )
;)



;; Solution

;; solutionp
(defun solutionp (node num-boxes-to-close)	
"If the number of boxes to close is equal to the number of closed boxes in the node, return 1.
Otherwise, it returns 0 if not."
	(= (num-closed-boxes (get-node-state node)) num-boxes-to-close)
)

;; path-solution
(defun path-solution (node &optional (solution nil)) 
"Returns the path to the solution, therefore all states from the initial state to the goal state."
	(cond
		((null (get-node-state node)) solution)
		(T (path-solution (get-node-parent node) (cons (get-node-state node) solution)))
	) 
)

;;; Heuristic functions (used for informed search algorithms)

;; original-heuristic
(defun original-heuristic (board num-boxes-to-close) 
"This heuristic was proposed by teachers, it favors boards with the most number of closed boxes."
	(- num-boxes-to-close (num-closed-boxes board))
)

;; alternative-heuristic
(defun alternative-heuristic (board num-boxes-to-close) 
"This heuristic was proposed by students."
(+ (* (original-heuristic board num-boxes-to-close) (num-lines-board board) ) 1))
;  (let* (
;          (horizontal-count (count-ones (get-horizontal-arcs board)))
;          (vertical-count (count-ones (get-vertical-arcs board)))
;        )
;        (cond
;          (format t "board: ~s~% hor: ~s~% horList: ~s~% ver~s~% verList ~s~%~%" board horizontal-count (get-horizontal-arcs board) vertical-count (get-vertical-arcs board))
;          ((or (= horizontal-count vertical-count) (= vertical-count horizontal-count))
;            (+ 1 (- num-boxes-to-close (num-closed-boxes board)) )
;          )
;          ((and (> horizontal-count vertical-count) (not (= vertical-count 0)))
;            (+ (/ horizontal-count vertical-count) (- num-boxes-to-close (num-closed-boxes board)))
;          )
;          ((and (> vertical-count horizontal-count) (not (= horizontal-count 0))) 
;            (+ (/ vertical-count horizontal-count) (- num-boxes-to-close (num-closed-boxes board)))
;          )
;        )
;  )
;)