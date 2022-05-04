(deftemplate tablero
    (multislot matriz)
    (slot id)
    (slot padre)
    (slot prof)
    (slot alfa)
    (slot beta)
)

(defglobal 
    ?*turnos* = 0
    ?*tablero* = ""
    ?*tamano* = 0
    ?*id* = 0
    ?*jugador1* = ""
    ?*jugador2* = ""
    ?*score1* = 0
    ?*score2* = 0 

)

(deffacts hechos-iniciales
        (estado "INICIO")
)

;Dadas las coordenadas x e y de una ficha, devuelve su posicion 1D en el tablero
(deffunction posTablero (?x ?y)
    (bind ?pos (+ (+(* ?x ?*tamano*) ?y) 1))
    (return ?pos)
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
    
    (bind $?tableroMulti (create$ (explode$ ?*tablero*)))
    (printout t $?tableroMulti crlf)
    (assert (tablero (matriz $?tableroMulti )(id ?*id*) (padre 0) (prof 0) (alfa -999) (beta 999)))
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
        (printout t "Fichas disponibles:" ?*jugador1* crlf)
        (printout t "Elija numero de ficha" crlf)
        (bind ?numero (read))

        ;lista=multicampo del fichas del jugador
        (bind $?lista (create$ (explode$ ?*jugador1*)))

        (if (not (subset (create$ ?numero) $?lista)) then
            (printout "Ese numero ya se ha usado, por favor elija otro" crlf)
            (assert (estado "TURNO"))
        )
    else
        (printout t "Fichas disponibles:" ?*jugador2* crlf)
        (printout t "Elija numero de ficha" crlf)
        (bind ?numero (read))

        ;lista=multicampo del fichas del jugador
        (bind $?lista (create$ (explode$ ?*jugador2*)))

        (if (not (subset (create$ ?numero) $?lista)) then
            (printout "Ese numero ya se ha usado, por favor elija otro" crlf)
            (assert (estado "TURNO"))
        )

    )
    
    (printout t "Donde desea insertar la ficha?" crlf)
    (printout t "Indica la fila" crlf)
    (bind ?x (read))

    (printout t "Indica la columna" crlf)
    (bind ?y (read))

    ;Para nosotros los indices empiezan en 0 (facilita calculos)
    ;Para el usuario empiezan en 1
    (bind ?x (- ?x 1))
    (bind ?y (- ?y 1))
    (if (= (mod  ?*turnos*  2) 0) then
        (bind ?color "R")
    
    else
        (bind ?color "A")
    )

    (if (> ?x ?*tamano*) then
         (printout t "Indica una posicion correcta" crlf)
         (assert (estado "TURNO"))
    else    
        (if (> ?y ?*tamano*) then
            (printout t "Indica una posicion correcta" crlf)
            (assert (estado "TURNO"))
        else
            (bind ?posicion (+(+(* ?x ?*tamano*) ?y)1))
            
            ;transformamos el tablero en variable multicampo
            (bind $?tableroMulti (create$ (explode$ ?*tablero*)))
            (bind ?comprobar (subseq$ ?tableroMulti ?posicion ?posicion))
            ;(if (subset (create$ "'_00'")?comprobar ) then
            (if (subset ?comprobar (create$ '_00')) then
                (assert (estado "ACTUALIZAR"))
                (assert (ficha ?color ?numero ?x ?y ))
                (bind $?lista (replace$ $?lista ?numero ?numero -))
                
                (if (= (mod  ?*turnos*  2) 0) then
                    (bind ?*score1* (+ ?*score1* ?numero))
                    (bind ?*jugador1* (implode$ $?lista))
                else
                    (bind ?*score2* (+ ?*score2* ?numero))
                    (bind ?*jugador2* (implode$ $?lista))
                )
            else
                (printout t "Esa posicion ya esta en uso, por favor, indica una posicion correcta" crlf)
                (assert (estado "TURNO"))
            )
        )
    )
)


;METE LA FICHA GENERADA EN EL TABLERO
(defrule ACTUALIZAR_TABLERO
    (declare (salience 5))
    ?tab <-(tablero (matriz $?) (id ?) (padre ?) (prof ?) (alfa ?) (beta ?))
    ?a<-(ficha ?color ?numero ?x ?y )
    ?b<-(estado "ACTUALIZAR")

=>
    (retract ?b)
    ;Para que el tablero se imprima bien, añadimos un 0 si el valor es de un solo digito
    (if (< ?numero 10) then
        (bind ?numero (str-cat "0" ?numero))
    )
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


    ;------------CAMBIAR COLOR Y PUNTUACION
    ; X e Y que hay que sumar/restar para obtener los adyacentes:
    ;
    ;   - - | - = | - +
    ;   = - | = = | = +
    ;   + - | + = | + +
    ;
    (bind $?adyacentes (create$))

    ;esquina sup izq
    (if (and (= ?x 0) (= ?y 0)) then
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (+ ?y 1))))        ;adyacente der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) (+ ?y 1))))  ;adyacente inf der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) ?y)))        ;adyacente inf
    )
    ;lado izq
    (if (and (= ?y 0) (and (not(= ?x 0)) (not (= ?x (- ?*tamano* 1))))) then
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) ?y )))       ;adyacente sup
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) (+ ?y 1) ))) ;adyacente sup der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (+ ?y 1))))        ;adyacente der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) (+ ?y 1))))  ;adyacente inf der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) ?y)))        ;adyacente inf
    )
    ;esquina inf izq
    (if (and (= ?y 0) (= ?x (- ?*tamano* 1))) then
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) ?y )))       ;adyacente sup
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) (+ ?y 1) ))) ;adyacente sup der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (+ ?y 1))))        ;adyacente der
    )
    ;lado inf
    (if (and (= ?x (- ?*tamano 1) (and (not(= ?y 0)) (not (= ?y (- ?*tamano* 1))))) then
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (- ?y 1))))        ;adyacente izq
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) (- ?y 1))))  ;adyacente sup izq
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) ?y )))       ;adyacente sup
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) (+ ?y 1) ))) ;adyacente sup der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (+ ?y 1))))        ;adyacente der
    )
    ;esquina inf der
    (if (and (= ?x (- ?*tamano* 1)) (= ?y (- ?*tamano* 1))) then
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (- ?y 1))))        ;adyacente izq
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) (- ?y 1))))  ;adyacente sup izq
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) ?y )))       ;adyacente sup
    )
    ;lado der
    (if (and (= ?y (- ?*tamano* 1)) (and (not(= ?x 0)) (not (= ?x (- ?*tamano* 1))))) then
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) (- ?y 1))))  ;adyacente sup izq
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) ?y )))       ;adyacente sup
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) ?y)))        ;adyacente inf
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) (- ?y 1))))  ;adyacente inf izq
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (- ?y 1))))        ;adyacente izq
    )
    ;esquina sup der
    (if (and (= ?x 0) (= ?y (- ?*tamano* 1))) then
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (- ?y 1))))        ;adyacente izq
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) ?y)))        ;adyacente inf
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) (+ ?y 1))))  ;adyacente inf der
    )
    ;lado sup
    (if (and (= ?x 0) (and (not(= ?y 0)) (not (= ?y (- ?*tamano* 1))))) then
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (- ?y 1))))        ;adyacente izq
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (+ ?y 1))))        ;adyacente der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) (+ ?y 1))))  ;adyacente inf der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) ?y)))        ;adyacente inf
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) (- ?y 1))))  ;adyacente inf izq
    )
    ;Caso general: centro
    (if (and (and (not(= ?x 0)) (not (= ?x (- ?*tamano* 1))))  (and (not(= ?y 0)) (not (= ?y (- ?*tamano* 1))))) then
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) (- ?y 1))))  ;adyacente sup izq
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) ?y )))       ;adyacente sup
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) (+ ?y 1) ))) ;adyacente sup der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (+ ?y 1))))        ;adyacente der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) (+ ?y 1))))  ;adyacente inf der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) ?y)))        ;adyacente inf
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) (- ?y 1))))  ;adyacente inf izq
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (- ?y 1))))        ;adyacente izq
    )

    ;------------------------------------

    ;Actualizamos el hecho
    (modify ?tab (matriz ?tableroMulti))

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
    (printout t crlf)
    (printout t "Puntuacion del jugador1: " ?*score1* crlf)
    (printout t "Puntuacion del jugador2: " ?*score2* crlf)
    (printout t crlf)
    (printout t crlf)

    (if (= (mod (* ?*tamano* ?*tamano*) 2) 0) then
        (if (= ?*turnos* (* ?*tamano* ?*tamano*)) then
            (assert (estado "GANAR"))
        else
            (assert (estado "TURNO"))
        )
    else
        (if (= ?*turnos* (- (* ?*tamano* ?*tamano*) 1)) then
            (assert (estado "GANAR"))
        else
            (assert (estado "TURNO"))
        )
    )
    
)

(defrule FINAL
    ?a<-(estado "GANAR")
=>
    (if (= ?*score1* ?*score2*) then
        (printout t "Empate")
        (halt)
    else
        (if (> ?*score1* ?*score2*) then
            (printout t "El jugador1 gana.")
            (halt)
        else
            (printout t "El jugador2 gana.")
            (halt)
        )
    )
)
