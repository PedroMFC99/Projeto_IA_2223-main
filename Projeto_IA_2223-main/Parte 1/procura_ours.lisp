(defun get-list-position-with-no-arcs (node maxi &optional (i 1))
;; Recebe uma lista de arcos verticais ou horizontais (get-horizontal-arc/get-vertical-arc)
;; Devolve a lista de posi��es que podem ser alterados
;; Exemplo (get-list-position-with-no-arcs (get-arcos-horizontais (tabuleiro-teste)) (length (get-arcos-horizontais (tabuleiro-teste))))
    (cond
        ((null node) nil)
        ((> i maxi) nil)
        (t 
           (append (get-position-of-row (car node) i (length (car node))) (get-list-position-with-no-arcs (cdr node) maxi (1+ i)))
        )
    )
)

(defun get-position-of-row (row &optional i maxj (j 1))
;; Recebe uma parte da lista de arcos verticais ou horizontais
;; Devolve as posi��es que podem ser alterados
    (cond
        ((> j maxj) nil)
        ((= (car row) 1) (get-position-of-row (cdr row) i maxj (+ j 1)))
        (t (cons (list i j) (get-position-of-row (cdr row) i maxj (+ j 1))))
    )
)

(defun new-node-successor (node operator) ;; Novo NÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â³ Sucessor
;   Recebe 1 no e 1 operador
;   Devolve todos os nÃ³s sucessores possiveis
;  Se o operador for 'INSERIR-ARCO-HORIZONTAL
;(new-node-successor (criar-no (tabuleiro-teste)) 'INSERIR-ARCO-HORIZONTAL)
    (cond
        ((null node) nil)
        ((equal operator 'INSERIR-ARCO-HORIZONTAL)
            (let*   (
                        (succesors )
                        (tab (car node))
                        (tabHor (car tab) )
                        (listPos (get-list-position-with-no-arcs (get-arcos-horizontais tab) (length (get-arcos-horizontais tab))))
                    )
                    (format t "succesors = ~a~% tab = ~a~% tabHor = ~a~% listPos = ~a~%~%" succesors tab tabHor listPos)
                    
                    (loop
                        (when (or (null tabHor) (null listPos)) (return))
                        ; do something with the list here
                        
                        (if (equal (car (car tabHor)) 0)
                            (setf succesors (append  (criar-no (inserir-arco-horizontal (car (car listPos)) (car (cdr (car listPos))) tab) (1+ (no-profundidade node)) (no-heuristica node) node) succesors))
                        )
                        (if (equal (car (car tabHor)) 0) 
                            (setf listPos (cdr listPos))
                        )
                        (if (equal (car tabHor) nil)
                            (setf tabHor (cdr tabHor)) ;True Statement
                            (setf tabHor (append (list(cdr (car tabHor))) (cdr tabHor))); false statement
                        )

                        
                        (format t "listPos = ~a ~%tabHor = ~a ~%succesors = ~a ~%~%" listPos tabHor succesors)
                    )
                    ;(criar-no (inserir-arco-horizontal (car (car listPos)) (car (cdr (car listPos))) tab) (1+ (no-profundidade node)) (no-heuristica node) node)
            )
        )
        ((equal operator 'INSERIR-ARCO-VERTICAL)
            (let*   (
                        (succesors )
                        (tab (car node))
                        (tabVer (car (cdr tab)) )
                        (listPos (get-list-position-with-no-arcs (get-arcos-verticais tab) (length (get-arcos-verticais tab))))
                    )
                    (format t "succesors = ~a~% tab = ~a~% tabVer = ~a~% listPos = ~a~%~%" succesors tab tabVer listPos)
                    
                    (loop
                        (when (or (null tabVer) (null listPos)) (return))
                        ; do something with the list here
                        
                        (if (equal (car (car tabVer)) 0)
                            (setf succesors (append  (criar-no (inserir-arco-vertical (car (car listPos)) (car (cdr (car listPos))) tab) (1+ (no-profundidade node)) (no-heuristica node) node) succesors))
                        )
                        (if (equal (car (car tabVer)) 0) 
                            (setf listPos (cdr listPos))
                        )
                        (if (equal (car tabVer) nil)
                            (setf tabVer (cdr tabVer)) ;True Statement
                            (setf tabVer (append (list(cdr (car tabVer))) (cdr tabVer))); false statement
                        )

                        
                        (format t "listPos = ~a ~%tabVer = ~a ~%succesors = ~a ~%~%" listPos tabVer succesors)
                    )
                    ;(criar-no (inserir-arco-horizontal (car (car listPos)) (car (cdr (car listPos))) tab) (1+ (no-profundidade node)) (no-heuristica node) node)
            )
        )
    )
        
)


(defun node-successor (node opList algorithm &optional maxDepth) ;;No Sucessor

)

(defun bfs-visited-list () ;;Fechados

)

(defun bfs-not-visited-list () ;; Abertos

)

(defun bfs (visited notVisited) ;; Algoritmo BFS
;    (cond
;        ((null notVisited) nil)
;        (t
;            (let*
;                (nodesNotVisited (cons (car notVisited) visited))
;                (sucessors (sucessors (car notVisited)))
;                (nodesVisited (append (cdr notVisited) sucessors))
;                (solution (no-solucao sucessors))
;                    (cond
;                        ((not (null solution)) solution)
;                        (t (bfs nodesNotVisited nodesVisited))
;                    )
;            )
;        )
;    )
)

;; Returns a list of the neighbors for a given node in a graph
(defun get-neighbors (node graph)
  (loop for edge in (get-edges graph)
        when (or (equal (edge-start edge) node)
                 (equal (edge-end edge) node))
        collect (if (equal (edge-start edge) node)
                    (edge-end edge)
                    (edge-start edge))))

;; Returns a boolean indicating whether a given node has been visited
(defun visited? (node visited-nodes)
  (member node visited-nodes))

;; Marks a given node as visited
(defun mark-visited (node visited-nodes)
  (push node visited-nodes))


(defun bfs (root)
    (let    (
                (queue (list root))
            )
            (labels ((bfs-helper (queue)
                (if (null queue)
                    nil
                    (let*   (
                                (node (first queue))
                                (neighbors (get-neighbors node))
                            )
                            (dolist (neighbor neighbors)
                                (unless (visited? neighbor)
                                (setf queue (append queue (list neighbor)))
                                (mark-visited neighbor))
                            )
                            (bfs-helper (rest queue))
                    )
                )))
            (bfs-helper queue)
            )
    )
)
