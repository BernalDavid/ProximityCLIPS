(defglobal 
    ?*turnos* = 1
    ?*tablero* = ""
    ?*tamano* = 0
    ?*jugador1* = ""
    ?*jugador2* = ""
)

(deffacts hechos-iniciales
        (estado "INICIO")
)


;INICIALIZA EL TABLERO (TODO VACIO)
(defrule INICIALIZAR_TABLERO
    (declare (salience 20))
    ?b<-(estado "INICIO")
=>
    (retract ?b)
    (assert (estado "TURNO"))
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

    (bind ?aux (div (* ?*tamano* ?*tamano*) 2))
    (loop-for-count (?i 1 ?aux) do
        (bind ?*jugador1* (str-cat ?*jugador1* ?i" "))
        (bind ?*jugador2* (str-cat ?*jugador2* ?i" "))
    )
    ;(printout t ?*tablero* crlf)

)


;PIDE AL USUARIO COORDENADAS Y GENERA LA FICHA
(defrule INSERTAR_FICHA
    (declare (salience 10))
    ?b<-(estado "TURNO")
=>
    (retract ?b)
    
    ;FALTA ESPECIFICAR EL JUGADOR 
    (if (= (mod  ?*turnos*  2) 0) then
        (printout t "Fichas disponibles:" ?*jugador2* crlf)
        (printout t "Elija numero de ficha" crlf)
        (bind ?numero (read))

        ;lista=multicampo del fichas del jugador
        (bind $?lista (create$ (explode$ ?*jugador2*)))

        (if (subset (create$ ?numero) $?lista) then
            (bind $?lista (replace$ $?lista ?numero ?numero -))
            (bind ?*jugador2* (implode$ $?lista))
        else
            (printout "Ese numero ya se ha usado, por favor elija otro" crlf)
            (assert (estado "TURNO"))
        )
    else
        (printout t "Fichas disponibles:" ?*jugador1* crlf)
        (printout t "Elija numero de ficha" crlf)
        (bind ?numero (read))

        ;lista=multicampo del fichas del jugador
        (bind $?lista (create$ (explode$ ?*jugador1*)))

        (if (subset (create$ ?numero) $?lista) then
            (bind $?lista (replace$ $?lista ?numero ?numero -))
            (bind ?*jugador1* (implode$ $?lista))
        else
            (printout "Ese numero ya se ha usado, por favor elija otro" crlf)
            (assert (estado "TURNO"))
    )

    )
    
    
    ; (printout t "Fichas disponibles:" ?*jugador1* crlf)
    ; (printout t "Elija numero de ficha" crlf)
    ; (bind ?numero (read))

    (printout t "Donde desea insertar la ficha?" crlf)
    (printout t "Indica la fila" crlf)
    (bind ?x (read))

    (printout t "Indica la columna" crlf)
    (bind ?y (read))
    (if (= (mod  ?*turnos*  2) 0) then
        (bind ?color "R")
    
    else
        (bind ?color "A")
    )
    (if (> ?x ?*tamano*) then
         (printout t "Indica una posicion correcta" crlf)
         (bind $?lista (replace$ $?lista ?numero ?numero ?numero))
         (assert (estado "TURNO"))
    else    
        (if (> ?y ?*tamano*) then
            (printout t "Indica una posicion correcta" crlf)
            (bind $?lista (replace$ $?lista ?numero ?numero ?numero))
            (assert (estado "TURNO"))
        else
            (bind ?posicion (+ (-(* ?y ?*tamano*) ?x)1))
            
            ;transformamos el tablero en variable multicampo
            (bind $?tableroMulti (create$ (explode$ ?*tablero*)))
            (bind ?comprobar (subseq$ ?tableroMulti ?posicion ?posicion))
            ;(if (subset (create$ "'_00'")?comprobar ) then
            (if (subset ?comprobar (create$ '_00')) then
                (assert (estado "ACTUALIZAR"))
                (assert (ficha ?color ?numero ?x ?y ))
            else
                (printout t "Esa posicion ya esta en uso, por favor, indica una posicion correcta" crlf)
                (bind $?lista (replace$ $?lista ?numero ?numero ?numero))
                (assert (estado "TURNO"))
            )
        )
    )
)


;METE LA FICHA GENERADA EN EL TABLERO
(defrule ACTUALIZAR_TABLERO
    (declare (salience 5))
    ?a<-(ficha ?color ?numero ?x ?y )
    ?b<-(estado "ACTUALIZAR")

=>
    (retract ?b)
    (bind ?x (- ?x 1))
    (bind ?y (- ?y 1))
    (assert (estado "TURNO"))
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
    (bind ?*turnos* (+ ?*turnos* 1))
)

