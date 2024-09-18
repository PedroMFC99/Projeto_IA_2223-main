;;;; projeto.lisp
;;;; IA - 2022/ 2023
;;;; Diogo Letras - 202002529 & Pedro Cunha - 202000757
;;;; Implementa a interacao com o utilizador incluindo a escrita e leitura de ficheiros

;;; Extra Functions

;; read-keyboard
(defun read-keyboard ()
"Reads inputs from the keyboard"
	(read)
)

;; current-date
(defun current-date ()
"Returns date and time in string format"
	(multiple-value-bind (sec min hr day mon yr)
		(get-decoded-time)
		(format nil "~A-~A-~A : ~A:~A:~A" yr mon day hr min sec)
	)
)

;; elem-exists-list
(defun elem-exists-list (elem list)
"Verifies if an element exists in a given list. Returns T if the given elem exists in the list
and returns NIL if the elem does not exist"
  (if (null list)
		nil
		(if (eql elem (first list))
			T
			(elem-exists-list elem (rest list))
		)
 )
)

;;; Initialization & Menus

;;start
(defun start ()
"Initializes the program."
	(let 	(
				(path (insert-directory))
			)
			(compile-load-files path)
	)
)

;;insert-directory
(defun insert-directory ()
"Prompts the user to enter the path to a folder containing project files."
	(format t "~%Please, introduce the file's path~%")
	(format nil (read-line)
 )
)

;;compile-load-files
(defun compile-load-files (path)
"Compiles & loads the necessary files for the program to run."
  	(let 	(
				(puzzle-file (concatenate 'string path "\\puzzle.lisp"))
				(search-file (concatenate 'string path "\\procura.lisp"))
				(puzzle-compiled (concatenate 'string path "\\puzzle.64ofasl"))
				(search-compiled (concatenate 'string path "\\procura.64ofasl"))
			)
			(compile-file puzzle-file)
			(compile-file search-file)
			(load puzzle-compiled)
			(load search-compiled)
			(main-menu path)
	)
)

;; main-menu
(defun main-menu (path) 
"Draws the main menu of the application."
	(loop	
		(progn
			(format t "~%> ______________________________________________________________ ")
			(format t "~%>|     DOTS AND BOXES PUZZLE (BY PEDRO CUNHA & DIOGO LETRAS)    |")
			(format t "~%>|                                                              |")
			(format t "~%>|     1. Start a search                                        |")     
			(format t "~%>|     2. Check puzzle rules (tutorial)                         |")		
			(format t "~%>|     3. Quit                                                  |")
			(format t "~%>|                                                              |")
			(format t "~%> ______________________________________________________________ ")
			(format t "~%> option")
			(format t "~%> ")
			
			(let 	(
						(option (read-keyboard))
					)
					(cond
						((not (numberp option)) (main-menu path))		
						((and (<= option 3) (>= option 1))
							(cond
								((= option 1) (start-search path))
								((= option 2) (puzzle-rules))	
								((= option 3) (progn (format t "BYE!")) (return))
							)
						)
						(T (progn
								(format t "~%> Invalid option!")
								(format t "~%> Insert a valid option: [1, 2]")
								(fresh-line)
								(format t "~%")
							)
						)
					)
			)
		)
	)
)

;; start-search 
(defun start-search (path) 
"Asks the user for all the information necessary to start a search in the state space, namely:
the initial state (board), the search algorithm and depending on the chosen algorithm, it may be necessary to indicate the maximum depth."
	(let* 	(	 
				(node (create-node (read-boards path)))
				(num-boxes-to-close (read-num-boxes-to-close))
				(algorithm (read-algorithm))
				(depth (cond ((eql algorithm 'dfs) (read-depth)) (T 10)))
				(heuristic (cond ((not (or (eql algorithm 'dfs) (eql algorithm 'bfs))) (read-heuristic)) (T nil)))
				(init-time (get-universal-time))
				(solution (generic-search node depth 'solutionp 'successors algorithm (operators) heuristic num-boxes-to-close))			
			)
			(cond
				((null solution) (exception-stats node path))
				(T (write-stats node depth algorithm heuristic solution init-time path))
			)
	)
)

;; read-boards
(defun read-boards (path)
"Shows all possible boards. Receives the user's choice and returns his choice if it is valid."
	(progn
		(format t "~%> _________________________________________________________________ ")
		(format t "~%>|                                                                 |")
		(format t "~%>|     Choose the initial board:                                   |") 
		(format t "~%>|                                                                 |")
		(format t "~%>|     1. Board A ---> Closed boxes: 1   |   Dimensions: 3 x 3     |")
		(format t "~%>|     2. Board B ---> Closed boxes: 5   |   Dimensions: 4 x 4     |")
		(format t "~%>|     3. Board C ---> Closed boxes: 4   |   Dimensions: 4 x 4     |")
		(format t "~%>|     4. Board D ---> Closed boxes: 0   |   Dimensions: 5 x 4     |")
		(format t "~%>|     5. Board E ---> Closed boxes: 2   |   Dimensions: 6 x 6     |")
		(format t "~%>|     6. Board F ---> Closed boxes: 0   |   Dimensions: 7 x 7     |")
		(format t "~%>|     7. Board G ---> Adicionar na discussao de projeto!          |")
		(format t "~%> _________________________________________________________________")
		(format t "~%>  Chosen board: ")
		(format t "~%> ")	

		(let* 	(
					(option (read-keyboard))
			   		(valid-option (elem-exists-list option '(1 2 3 4 5 6)))
				)		;;adicionar na discussaao!
				(with-open-file (file (concatenate 'string path "\\problemas.dat") :direction :input :if-does-not-exist :error) ;;reads the board in problemas.dat file 
					(cond
						((not valid-option)
							(progn
								(format t "~%> Invalid option!")
								(format t "~%  ")
								(fresh-line)
								(read-boards path)
							)
						)
						((equal option '1) (nth 0 (read file)))
						((equal option '2) (nth 1 (read file)))
						((equal option '3) (nth 2 (read file)))
						((equal option '4) (nth 3 (read file)))
						((equal option '5) (nth 4 (read file)))
						((equal option '6) (nth 5 (read file)))
						;((equal option '7) (nth 6 (read file))) adicionar na discussao!
					)
				)
		)
	)
)

;; read-num-boxes-to-close 
(defun read-num-boxes-to-close ()
"Reads the number of boxes to close in the board."
  (progn
		(format t "~%> Number of boxes to close: ")
		(format t "~%> ")
		(let 	(
					(answer (read-keyboard))
				)
				(cond 
					((not (numberp answer)) (read-num-boxes-to-close))
					((>= answer 1) answer)
					(T (read-num-boxes-to-close))
				)
		)
	)
)

;; read-algorithm
(defun read-algorithm ()
"Receives the name of the algorithm that will be used in the search."
	(format t "~%> ______________________________________________________________ ")
	(format t "~%>|                                                              |")
  	(format t "~%>| Choose one of the following algorithms to solve the board:   |")
  	(format t "~%>|                                                              |")
  	(format t "~%>| Breadth-first Search --->  bfs                               |")
  	(format t "~%>| Depth-first Search --->    dfs                               |")
	(format t "~%>| A* Search --->             a-asterisc                        |")
	(format t "~%>|                                                              |")
	(format t "~%> ______________________________________________________________ ")
  	(format t "~%> Choosen algorithm: ")
  	(format t "~%> ")
  	(let 	(
				(answer (read-keyboard))
			)
			(cond
				((eq answer 'bfs) 'bfs)
				((eq answer 'dfs) 'dfs)
				((eq answer 'a-asterisc ) 'a-asterisc)
				(t (format t "~%> Invalid option!")
					(fresh-line)
					(read-algorithm)
				)
			)
	)
)

;;read heuristic
(defun read-heuristic ()
"Receives the correct heuristic."
	(format t "~%> ______________________________________________________________ ")
	(format t "~%>|                                                              |")
	(format t "~%>| Choose one of the following heuristic to solve the board:    |")
	(format t "~%>|                                                              |")
	(format t "~%>| Original Heuristic --->  oh                                  |")
	(format t "~%>| Alternative Heuristic --->  ah                               |")
	(format t "~%>|                                                              |")
	(format t "~%> ______________________________________________________________ ")
	(format t "~%> Choosen heuristic: ")
  	(format t "~%> ")
	(let 	(
				(answer (read-keyboard))
			)
			(cond
				((eq answer 'oh) 'original-heuristic)
				((eq answer 'ah) 'alternative-heuristic)
				(t (format t "~%> Invalid option!")
					(fresh-line)
					(read-heuristic)
				)
			)
	)
)

;; ler depth do algorithm dfs
(defun read-depth () 
"Reads the depth - used for b-f-search"
	(progn
		(format t "~%> Introduce a depth: ")
		(format t "~%> ")
		(let 	(
					(answer (read-keyboard))
				)
				(cond 
					((or (not (numberp answer)) (or (> answer 100) (<= answer 0))) 
						(progn
							(format t "~%> Invalid option! Introduce a value between [0,100]")
							(format t "~%  ")
							(fresh-line)
							(read-depth)
						)
					)
					(T answer)
				)
		)
	)
)


;; puzzle-rules
(defun puzzle-rules() 
"Draws the rules of 'dots and boxes'"
	(format t "~%> 
______________________________________________________________ Dots and Boxes Puzzle Rules ____________________________________________________________
|           The objective of the puzzle is to arrange the arcs on the board in such a way that a certain number of boxes are formed and closed.         |
|                                                                                                                                                       |
|                                           When the number of boxes to close is reached, the puzzle is solved.                                         |
|                                                                                                                                                       |
| Therefore, solving the puzzle consists of adding a sucession of dots that allows reaching a state where the number of boxes to be closed is reached.  |
|                                                                                                                                                       |
| ***Note***: A closed box is when two horizontal lines and two vertical lines are added between four adjacent dots, therefore generating a closed box' |
_______________________________________________________________________________________________________________________________________________________~%~%"
	)
)

;;; Statistics Handling

;; write-stats
(defun write-stats (initial-node max-depth algorithm heuristic solution init-time directory) 
"Writes in estatisticas.dat the statistics about the game."
	(let* 
		(
			(dimension-unvisited-list (second solution))
			(node-solution (first solution))
			(state-solution (get-node-state node-solution))
			(dimension-visited-list (+ (third solution) 1))
			(generated-nodes (- (+ dimension-unvisited-list dimension-visited-list) 1))
			(depth (get-node-depth (get-node-state solution)))
			(goal-node (get-node-state solution))
			(time (- (get-universal-time) init-time))
			(path (path-solution node-solution))
			(heuristic-value (get-node-heuristic node-solution))
		)
						
		(with-open-file (file (concatenate 'string directory "\\estatisticas.dat") 
							:direction :output
							:if-exists :append 
							:if-does-not-exist :create)

							
			;; write in estatisticas.dat file
			(format file "______________________________________________________________ Dots and Boxes Data ____________________________________________________________~%")
			(format file "| Generated in: ~s~%" (current-date))
			(format file "| Time Spent: ~s seconds~%" time)
			(format file "| Initial State: ~s~%" initial-node)
			(format file "| Final State: ~s~%" state-solution)
			(format file "| Maximum Depth: ~s~%" max-depth)
			(format file "| Algorithm: ~s~%" algorithm)
			(format file "| Heuristic: ~s~%" heuristic)
			(format file "| Depth: ~s~%" depth)
			(format file "| Generated Nodes: ~s~%" generated-nodes)
			(format file "| Expanded Nodes: ~s~%" dimension-visited-list)
			(format file "| Penetrance: ~s~%" (penetrance goal-node generated-nodes));
			(format file "| Branching Factor: ~s~%" (branching-factor depth dimension-visited-list))	
			(format file "| Heuristic Value: ~s~%" heuristic-value)
			(format file "| Num Closed Boxes: ~s~%" (num-closed-boxes (get-node-state goal-node)))
			(format file "_______________________________________________________________________________________________________________________________________________~%")
			(format file "	                                                                                                                                                ~%")
			(format file "Solution Path: ~s ~%" path)
		)

		;; draw in console
		(format t "______________________________________________________________ Dots and Boxes Data ____________________________________________________________~%")
		(format t "| Generated in: ~s~%" (current-date))
		(format t "| Time Spent: ~s seconds~%" time)
		(format t "| Initial State: ~s~%" initial-node)
		(format t "| Final State: ~s~%" state-solution)
		(format t "| Maximum Depth: ~s~%" max-depth)
		(format t "| Algorithm: ~s~%" algorithm)
		(format t "| Heuristic: ~s~%" heuristic)
		(format t "| Depth: ~s~%" depth)
		(format t "| Generated Nodes: ~s~%" generated-nodes)
		(format t "| Expanded Nodes: ~s~%" dimension-visited-list)
		(format t "| Penetrance: ~s~%" (penetrance goal-node generated-nodes));
		(format t "| Branching Factor: ~s~%" (branching-factor depth dimension-visited-list))	
		(format t "| Heuristic Value: ~s~%" heuristic-value)
		(format t "| Num Closed Boxes: ~s~%" (num-closed-boxes (get-node-state goal-node)))
		(format t "_______________________________________________________________________________________________________________________________________________~%")
		(format t "	                                                                                                                                                ~%")
		(format t "Solution Path: ~s ~%" path)
	)
)

;;exception-stats
(defun exception-stats (initial-node directory) 
"Prints in estatisticas.dat that it is not possible to calculate the statistics for the problem."
		
	(with-open-file (file (concatenate 'string directory "\\estatisticas.dat") 
		:direction :output
		:if-exists :append 
		:if-does-not-exist :create)
		(format file "~%Initial state: ~s ~%" initial-node)
		(format file "No solution possible")
		(format t "~%Initial state: ~s ~%" initial-node)
		(format t "Impossible to find a solution!")
	)
)