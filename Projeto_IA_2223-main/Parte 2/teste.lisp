(defun quantos-impares lista
"Função recursiva que obtem os seguintes resultados: (quantos-impares ’(1 2 3)) = 2 | (quantos-impares ’(0 1 (2 ((3)4)5))) = 3 | (quantos-impares nil) = 0"
    (cond
        ((null lista) 0)
        ((atom (car lista))
            (if (oddp (car lista))
                (+ 1 (quantos-impares (cdr lista)))
                (quantos-impares (cdr lista)))
        )
        (t (+ (quantos-impares (car lista)) (quantos-impares (cdr lista))))
    )
)
