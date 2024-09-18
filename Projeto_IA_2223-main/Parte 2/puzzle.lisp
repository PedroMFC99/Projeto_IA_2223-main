;;;; puzzle.lisp
;;;; IA - 2022/ 2023
;;;; Diogo Letras - 202002529 & Pedro Cunha - 202000757
;;;; Implementa a resolucao do problema, incluindo a definicao dos operadores, que sao especificos do dominio de aplicacao

;;test-board (ignore)
(defun test-board ()
"Returns 3x3 board (3 horizontal arcs x 3 horizontal arcs)"
  (list
    '((0 0 0) (0 0 0) (0 0 0) (0 0 0))
    '((0 0 0) (0 0 0) (0 0 0) (0 0 0))
  )
)

(defun inicial-board ()
  (list
    ;arcos horizontais
    '((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0))
    ;arcos verticais
    '((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))
  )
)

(defun final-board ()

  (list
    ;arcos horizontais
    '((1 2 1 1 0 2) (2 1 1 1 1 0) (0 2 1 1 2 0) (0 1 0 2 2 0) (1 2 0 0 0 0) (0 1 2 1 2 1))
    ;arcos verticais
    '((1 0 1 0 0) (2 1 1 2 2) (2 1 1 2 0) (1 2 2 1 1) (1 2 2 0 0) (0 1 2 1 2) (2 2 1 2 0))
  )

)

;;; Constructor

;; create-node
(defun create-node (state &optional (depth 0) (evaluation 0) (boxes-player-1 0) (boxes-player-2 0)) 
"Create a list that represents a node
A node is composed by:
- state of the board (mandatory parameter),
- depth at which the node is (by default value 0),
- evaluation of the node (by default value 0),
- boxes of player1, (by default value 0).
- boxes of player2, (by default value 0)."
	(list state depth evaluation boxes-player-1 boxes-player-2)
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

;;get-node-evaluation
(defun get-node-evaluation (node) 
"Returns the evaluation of the node"
	(third node)
)

;;get-node-parent
(defun get-boxes-player-1 (node) 
"Returns the number of boxes of player1"
	(fourth node)
)

;;get-node-parent
(defun get-boxes-player-2 (node) 
"Returns the number of boxes of player2"
	(fifth node)
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
(defun insert-arc-in-position (index list piece) 
"Insert an arc (represented by the value [1]) at the index of the received list"
  (if (null list)
    nil
    (if (= index 1)
      (cons piece (rest list))
      (cons (first list) (insert-arc-in-position (- index 1) (rest list) piece))
    )
  )
)

;;insert-arc-in-position-final
;;arc-in-position-final
(defun insert-arc-in-position-final (line column list piece)
"Insert an arc (represented by the value [1]) into a list representing the set of arcs of a board."
  (if (null list)
    nil
    (if (= line 1)
      (let  (
              (new-line (insert-arc-in-position column (first list) piece))
            )
            (cons new-line (rest list))
      )
      (cons (first list) (insert-arc-in-position-final (- line 1) column (rest list) piece))
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
(defun insert-horizontal-arc (line column piece board)
"Insert a horizontal arc (represented by the value [1]) into a board passed as an argument"
  (let  (
          (horizontal-arcs (get-horizontal-arcs board))
          (vertical-arcs (get-vertical-arcs board))
        )
        (when
          (and board (verify-add-arc line column horizontal-arcs))
          (cons (insert-arc-in-position-final line column horizontal-arcs piece) (list vertical-arcs))
        )
  )
)

;;insert-vertical-arc
(defun insert-vertical-arc (line column piece board) 
"Insert a vertical arc (represented by the value [1]) into a board passed as an argument"
  (let  (
          (horizontal-arcs (get-horizontal-arcs board))
          (vertical-arcs (get-vertical-arcs board))
        )
        (when
          (and board (verify-add-arc line column vertical-arcs))
          (cons horizontal-arcs (list (insert-arc-in-position-final line column vertical-arcs piece)))
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

;switch-piece
(defun switch-piece (piece) 
"Switches between player's pieces."
  (cond
	((= piece *player1*) *player2*)
	((= piece *player2*) *player1*)
	)
)
