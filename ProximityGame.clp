(deftemplate tablero
    (multislot matriz)
    (slot id)
    (slot padre)
    (slot prof)
    (slot alfa)
    (slot beta)
)

;Turnos pares:      Rojo jugador1
;Turnos impares:    Azul jugador2
(defglobal 
    ?*turnos* = 0
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

;Dada la info del tablero en un multicampo, la muestra por pantalla con formato de tablero
(deffunction mostrarTablero ($?tableroLocal)
    (bind ?indice 1)
    (progn$ (?elemento $?tableroLocal)
        (printout t ?elemento "")
        (if (=(mod ?indice ?*tamano*)0) then
            (printout t crlf)
        )
        (bind ?indice (+ ?indice 1))
    )
)

;Dadas las coordenadas 'x' e 'y' de una ficha en el tablero, devuelve un multicampo con sus fichas adyacentes
(deffunction obtenerAdyacentes (?x ?y)
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
    (if (and (= ?x (- ?*tamano* 1)) (and (not(= ?y 0)) (not (= ?y (- ?*tamano* 1))))) then
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
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) (- ?y 1))))  ;adyacente inf izq
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) ?y)))        ;adyacente inf
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
    (return $?adyacentes)
)

;Dados un jugador y el tablero, calcula la puntuación del jugador y la actualiza en la variable global
; (deffunction calcularPuntuacion(?jugador $?tableroLocal)
;     (bind ?puntos 0)
;     (progn$ (?i $?tableroLocal)
;         (bind ?ficha ?i)
;         (printout t "Ficha: " ?ficha crlf)
;         (if (eq (sub-string 1 1 ?ficha) ?jugador) then
;             (printout t "pts: " ?puntos crlf)
;             (bind ?puntos (+ ?puntos (eval (sub-string 2 3 ?ficha))))
;         )
;     )
;     (printout t "puntos: " ?puntos crlf)
;     (printout t "1ant: " ?*score1* crlf)
;     (if (eq (sub-string 1 1 ?ficha) "R") then (bind ?*score1* ?puntos))
;     (printout t "1des: " ?*score1* crlf)

;     (if (eq (sub-string 1 1 ?ficha) "A") then (bind ?*score2* ?puntos))
; )

; (deffunction mostrarTablero ($?tableroLocal)
;     (bind ?indice 1)
;     (progn$ (?elemento $?tableroLocal)
;         (printout t ?elemento "")
;         (if (=(mod ?indice ?*tamano*)0) then
;             (printout t crlf)
;         )
;         (bind ?indice (+ ?indice 1))
;     )
; )

;INICIALIZA EL TABLERO (TODO VACIO)
(defrule INICIALIZAR_TABLERO
    (declare (salience 20))
    ?b<-(estado "INICIO")
=>
    (retract ?b)
    (assert (estado "TURNO"))
    (printout t "Especifica tamano del tablero" crlf)
    (bind ?*tamano* (read))

    (bind $?tableroLocal (create$))

    ;Inicializar tablero local
    (loop-for-count (?i 1 (* ?*tamano* ?*tamano*)) do
        (bind $?tableroLocal (insert$ $?tableroLocal ?i "_00"))
    )
    
    ;Imprimir tablero
    (mostrarTablero $?tableroLocal)

    ;Inicializar fichas de los jugadores
    (bind ?aux (div (* ?*tamano* ?*tamano*) 2))
    (loop-for-count (?i 1 ?aux) do
        (bind ?*jugador1* (str-cat ?*jugador1* ?i" "))
        (bind ?*jugador2* (str-cat ?*jugador2* ?i" "))
    )
    
    ;Inicializar hecho de tablero
    (assert (tablero (matriz $?tableroLocal) (id ?*id*) (padre 0) (prof 0) (alfa -999) (beta 999)))
)


;PIDE AL USUARIO COORDENADAS Y GENERA LA FICHA
(defrule INSERTAR_FICHA
    (declare (salience 10))
    ?tab <-(tablero (matriz $?tableroLocal) (id ?) (padre ?) (prof ?) (alfa ?) (beta ?))
    ?b<-(estado "TURNO")
=>
    (retract ?b)

    ;comprobar si se quiere salir o no del juego
    (bind ?salir "0")
    (while (and  (not (=(str-compare ?salir "c") 0)) (not (=(str-compare ?salir "s") 0)))
        (printout t "Para continuar:    pulse la tecla 'c'" crlf)
        (printout t "Para salir:        pulse la tecla 's'" crlf)
        (bind ?salir (read))
    )
    ;continuar
    (if (=(str-compare ?salir "c")0) then
    
        (if (= (mod  ?*turnos*  2) 0) then
            (printout t "Fichas disponibles:" ?*jugador1* crlf)
            (printout t "Elija numero de ficha" crlf)
            (bind ?numero (read))

            ;lista=multicampo de fichas del jugador
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

        
        (bind ?x (- ?x 1))
        (bind ?y (- ?y 1))
        (if (= (mod  ?*turnos*  2) 0) then
            (bind ?color "R")
        else
            (bind ?color "A")
        )

        ;Para nosotros los indices empiezan en 0 (facilita calculos)
        ;Para el usuario empiezan en 1
        (if (> ?x ?*tamano*) then
            (printout t "Indica una posicion correcta" crlf)
            (assert (estado "TURNO"))
        else    
            (if (> ?y ?*tamano*) then
                (printout t "Indica una posicion correcta" crlf)
                (assert (estado "TURNO"))
            else
                (bind ?posicion (+(+(* ?x ?*tamano*) ?y)1))
            
                (bind ?comprobar (subseq$ $?tableroLocal ?posicion ?posicion))
            
                (if (subset ?comprobar (create$ "_00")) then
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
    ;salir
    else
        (if (=(str-compare ?salir "s")0) then
            (assert (estado "SALIR"))
        )
        
    )
)


;METE LA FICHA GENERADA EN EL TABLERO
(defrule ACTUALIZAR_TABLERO
    (declare (salience 5))
    ?tab <-(tablero (matriz $?tableroLocal) (id ?) (padre ?) (prof ?) (alfa ?) (beta ?))
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
    (bind ?posicion (posTablero ?x ?y ))
    
    ;insertamos la ficha en el tablero
    (bind $?tableroLocal (replace$ $?tableroLocal ?posicion ?posicion ?ficha))
    
    ;obtener las fichas adyacentes a la ficha insertada
    (bind $?adyacentes (obtenerAdyacentes ?x ?y))

    ;Hacer las actualizaciones en las adyacentes
    (progn$ (?pos $?adyacentes)
        (bind ?ficha (nth$ ?pos $?tableroLocal))
        (bind ?color (sub-string 1 1 ?ficha))
        (bind ?pts (eval (sub-string 2 4 ?ficha))) ;Crea un string con los pts y los pasa a integer

        (if (or (and (eq ?color "R") (= (mod ?*turnos* 2) 0))           ; Ficha roja en turno rojo: sumar
                (and (eq ?color "A") (= (mod ?*turnos* 2) 1))) then     ; Ficha azul en turno azul: sumar
            (bind ?pts (+ ?pts 1))
            (if (and (eq ?color "R") (= (mod ?*turnos* 2) 0)) then
                (bind ?*score1* (+ ?*score1* 1))
            else
                (bind ?*score2* (+ ?*score2* 1))
            )
        )
        
        (if (or (and (eq ?color "R") (= (mod ?*turnos* 2) 1))           ; Ficha roja en turno azul: cambiar color
                (and (eq ?color "A") (= (mod ?*turnos* 2) 0))) then     ; Ficha azul en turno rojo: cambiar color
            
            (bind ?ptsFichaActual (eval ?numero))
            (if (> ?ptsFichaActual ?pts) then 
                (if (eq ?color "R") then 
                    (bind ?color "A") 
                    (bind ?*score2* (+ ?*score2* ?pts))
                    (bind ?*score1* (- ?*score1* ?pts))
                else 
                    (bind ?color "R")
                    (bind ?*score1* (+ ?*score1* ?pts))
                    (bind ?*score2* (- ?*score2* ?pts))
                )
            )
              
        )
        ;Para que el tablero se imprima bien, añadimos un 0 si el valor es de un solo digito
        (if (< ?pts 10) then (bind ?pts (str-cat "0" ?pts)))
        (bind ?ficha (str-cat ?color ?pts))
        (bind $?tableroLocal (replace$ $?tableroLocal ?pos ?pos ?ficha))                  
    )
    
    ;Actualizamos el hecho
    (modify ?tab (matriz $?tableroLocal))
    ;Mostramos el tablero
    (mostrarTablero $?tableroLocal)
    ;Calculamos la nueva puntuacion de los jugadores
    ; (calcularPuntuacion "R" $?tableroLocal)
    ; (calcularPuntuacion "A" $?tableroLocal)
    ;Avanzamos un turno
    (bind ?*turnos* (+ ?*turnos* 1))

    ;Mostramos las puntuaciones hasta el momento
    (printout t crlf)
    (printout t "Puntuacion del jugador1: " ?*score1* crlf)
    (printout t "Puntuacion del jugador2: " ?*score2* crlf)
    (printout t crlf)
    (printout t crlf)

    ;Comprobar si la partida ha terminado o no
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

(defrule SALIR
    ?a <-(estado "SALIR")
=>
    (retract ?a)
    (printout t "SE HA SALIDO DEL JUEGO" crlf)
    (printout t "Puntuaciones obtenidas hasta el momento:" crlf)
    (printout t "Puntuacion del jugador1: " ?*score1* crlf)
    (printout t "Puntuacion del jugador2: " ?*score2* crlf)
    (halt)

)
