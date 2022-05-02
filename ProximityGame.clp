(defglobal 
    ?*turnos* = 0
    ?*tablero* = ""
    ?*tamano* = 0
)

(deffacts hechos-iniciales
        (inicio "_ __" 0)
)
;COMENTARIO DE PRUEBA PARA COMPROBAR LOS COMMITS

;INICIALIZA EL TABLERO (TODO VACIO)
(defrule INICIALIZAR_TABLERO
    (declare (salience 20))
    ?a<-(inicio ?o ?q)
=>
    (retract ?a) 
    (printout t "Especifica tamano del tablero" crlf)
    (bind ?*tamano* (read))

    (loop-for-count (?i 1 ?*tamano*) do
        (bind ?fila "")
        (loop-for-count(?j 1 ?*tamano*) do
           (bind ?fila (str-cat ?fila " '_00'"))
        )
        (printout t ?fila crlf)
        (bind ?*tablero* (str-cat ?*tablero* ?fila))
        
    )
    ;(printout t ?*tablero* crlf)

)


;PIDE AL USUARIO COORDENADAS Y GENERA LA FICHA
(defrule INSERTAR_FICHA
    (declare (salience 10))

=>

    (printout t "Donde desea insertar la ficha? (Numero ????)" crlf)

    (printout t "Indica la fila" crlf)
    (bind ?x (read))

    (printout t "Indica la columna" crlf)
    (bind ?y (read))
    (if (= (mod ?*turnos* 2) 0) then
        (bind ?color "R")
    
    else
        (bind ?color "A")
    )
    ;FALTA GENERAR NUMERO RANDOM (DE MOMENTO 10)
    (if (> ?x ?*tamano*) then
         (printout t "Indica una posicion correcta" crlf)
    else    
        (if (> ?y ?*tamano*) then
            (printout t "Indica una posicion correcta" crlf)
        else
            (bind ?x (- ?x 1))
            (bind ?y (- ?y 1))
            (assert (ficha ?color 10 ?x ?y))
            (+ ?*turnos* 1)

        )
    )
    
   

)


;METE LA FICHA GENERADA EN EL TABLERO
(defrule ACTUALIZAR_TABLERO
    (declare (salience 5))
    ?a<-(ficha ?color ?numero ?x ?y)

=>
    ;damos formato a la ficha
    (bind ?ficha (str-cat ?color ?numero))
    ;calculamos su posicion en el tablero
    (bind ?posicion (+ (+(* ?x ?*tamano*) ?y)1))
    ;transformamos el tablero en variable multicampo
    (bind $?tableroMulti (create$ (explode$ ?*tablero*)))
    ;insertamos la ficha en el tablero
    (bind $?tableroMulti (replace$ $?tableroMulti ?posicion ?posicion ?ficha))
    ;actualizamos el valor del tablero global
    (bind ?*tablero* (implode$ $?tableroMulti))

    ;mostramos el tablero actualizado
    (bind ?indice 1)
    (loop-for-count (?i 1 ?*tamano*) do
        (loop-for-count (?j 1 ?*tamano*) do
            (printout t (sub-string ?indice (+ ?indice 5) ?*tablero*))
            ;(printout t (subseq$ $?tableroMulti ?indice ?indice))
            (bind ?indice (+ ?indice 6))
        )
        (printout t crlf)
    )

)

