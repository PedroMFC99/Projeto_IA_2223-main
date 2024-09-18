(defun generic-search (initial-node max-depth f-solution f-successors f-algorithm operator-list f-heuristic num-boxes-to-close &aux (init-time (get-universal-time)))
  "Generic search algorithm, used to search for a solution in state space. Used in combination with bf-search and df-search. It may require a depth in case of df-search"
    (let    (
                (unvisited (list initial-node))
                (visited nil)
            )
            (generic-search-helper unvisited visited f-solution f-successors f-algorithm operator-list f-heuristic num-boxes-to-close max-depth init-time)
    )
)


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
                                            (let ((new-unvisited (funcall f-algorithm (rest unvisited) list-successors)))
                                            (generic-search-helper new-unvisited (cons cur-node visited) f-solution f-successors f-algorithm operator-list f-heuristic num-boxes-to-close max-depth init-time))
                                        )
                                )
                        )
                    )
                )
        )
    )
)
