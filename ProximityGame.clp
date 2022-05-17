(deftemplate tablero
    (multislot matriz)
    (slot id)
    (slot padre)
    (slot prof)
    (slot alfa)
    (slot beta)
    (slot heuristico)
    (slot turno)
    (slot real)
)

;Turnos pares:      Rojo jugador1
;Turnos impares:    Azul jugador2
(defglobal 
    ?*turnos* = 1
    ?*tamano* = 0
    ?*id* = 1
    ?*jugador1* = ""
    ?*jugador2* = ""
    ?*score1* = 0
    ?*score2* = 0
    ?*iaju* = ""
    ?*AGENTE* = "RANDOM"       ;MINIMAX // RANDOM
    ;Para añadir hecho con la jugada optima
    ?*tableroOpt* = (create$)
    ?*idOpt* = 0
    ?*padreOpt* = 0
    ?*profOpt* = 0
    ?*alfaOpt* = 0
    ?*betaOpt* = 0
    ?*heurOpt* = -999   
)

(deffacts hechos-iniciales
        (estado "INICIO")
)

;******************************  FUNCIONES  ******************************
;*************************************************************************

;Dadas las coordenadas x e y de una ficha, devuelve su posicion 1D en el tablero
(deffunction posTablero (?x ?y)
    (bind ?pos (+ (+(* ?x ?*tamano*) ?y) 1))
    (return ?pos)
)

;Dada una posicion 1D, devuelve sus coordenadas x y
(deffunction posMatriz2D (?pos)
    
    (bind ?x (div ?pos ?*tamano*))
    (bind ?y (mod ?pos ?*tamano*))
    (if (= ?y 0) then (bind ?y ?*tamano*))
    (if (not(= (mod ?pos ?*tamano*) 0)) then
       (bind ?x (+ ?x 1))
    )
    
    (return (create$ ?x ?y))

)

;Dada la info del tablero en un multicampo, la muestra por pantalla con formato de tablero
(deffunction mostrarTablero ($?tableroLocal)
    (bind ?indice 1)
    (progn$ (?elemento $?tableroLocal)
        (printout t  " | " ?elemento )
        (if (=(mod ?indice ?*tamano*)0) then
            (printout t " |" crlf)
            (loop-for-count (?i 1 ?*tamano*)
                (printout t " | ---")
            )
            (printout t " |" crlf)
            
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
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) (+ ?y 1))))  ;adyacente inf der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) ?y)))        ;adyacente inf
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) (- ?y 1))))  ;adyacente inf izq
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (+ ?y 1))))        ;adyacente der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (- ?y 1))))        ;adyacente izq
    )
    ;Caso general: centro
    (if (and (and (not(= ?x 0)) (not (= ?x (- ?*tamano* 1))))  (and (not(= ?y 0)) (not (= ?y (- ?*tamano* 1))))) then
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) (+ ?y 1))))  ;adyacente inf der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) ?y)))        ;adyacente inf
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (+ ?x 1) (- ?y 1))))  ;adyacente inf izq
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (+ ?y 1))))        ;adyacente der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero ?x (- ?y 1))))        ;adyacente izq
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) (+ ?y 1) ))) ;adyacente sup der
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) ?y )))       ;adyacente sup
        (bind $?adyacentes (insert$ ?adyacentes 1 (posTablero (- ?x 1) (- ?y 1))))  ;adyacente sup izq
        
        
        
        
        
        
        
    )
    
    (return $?adyacentes)
)

;Dada la posicion (numero) la lista de adyacentes y el tablero, actualiza el valor de las fichas adyacentes en el tablero
(deffunction actualizarAdyacentes (?numero ?adyacentes $?tableroLocal)
    (printout t "adyacentes " $?adyacentes crlf)
    (progn$ (?pos $?adyacentes)
        (printout t "pos    " ?pos crlf)
        (bind ?ficha (nth$ ?pos $?tableroLocal))
        (bind ?color (sub-string 1 1 ?ficha))

        (printout t "ficha           " ?ficha crlf)
        
        (bind ?pts (eval (sub-string 2 4 ?ficha))) ;Crea un string con los pts y los pasa a integer
        
        (if (or (and (eq ?color "R") (= (mod ?*turnos* 2) 0))           ; Ficha roja en turno rojo: sumar
                (and (eq ?color "A") (= (mod ?*turnos* 2) 1))) then     ; Ficha azul en turno azul: sumar
            (bind ?pts (+ ?pts 1))
            (if (and (eq ?color "R") (= (mod ?*turnos* 2) 0)) then
                ;(bind ?*score1* (+ ?*score1* 1))
            else
                ;(bind ?*score2* (+ ?*score2* 1))
            )
        )
        (if (or (and (eq ?color "R") (= (mod ?*turnos* 2) 1))           ; Ficha roja en turno azul: cambiar color
                (and (eq ?color "A") (= (mod ?*turnos* 2) 0))) then     ; Ficha azul en turno rojo: cambiar color
                
            (bind ?ptsFichaActual (eval ?numero))
            (if (> ?ptsFichaActual ?pts) then 
                (if (eq ?color "R") then 
                    (bind ?color "A") 
                    ;(bind ?*score2* (+ ?*score2* ?pts))
                    ;(bind ?*score1* (- ?*score1* ?pts))
                else 
                    (bind ?color "R")
                    ;(bind ?*score1* (+ ?*score1* ?pts))
                    ;(bind ?*score2* (- ?*score2* ?pts))
                )
            )
             
        )
        ;Para que el tablero se imprima bien, añadimos un 0 si el valor es de un solo digito
        (if (< ?pts 10) then (bind ?ptsString (str-cat "0" ?pts))
        else (bind ?ptsString (str-cat "" ?pts)))

        (printout t "ficha  antes de str-cat  " ?ficha crlf)
        (printout t "ptsString                " ?ptsString crlf)
        (bind ?ficha (str-cat ?color ?ptsString))
        ;(printout t "ficha  despues de str-cat" ?ficha crlf)
        (bind $?tableroLocal (replace$ $?tableroLocal ?pos ?pos ?ficha))
                         
    )
    (return $?tableroLocal)
)

;Dado un tablero, devuelve las posiciones libres
(deffunction obtenerLibres ($?tableroLocal)
    (bind $?libres (create$))
    (bind ?cont 1)
    (progn$ (?i $?tableroLocal)
        (bind ?ficha (sub-string 1 1 ?i))
        (if (=(str-compare ?ficha "_")0) then
            (bind ?libres (insert$ $?libres 1 ?cont))
        )
        (bind ?cont (+ ?cont 1))
    )
    (return $?libres)
)


;Dado un tablero, calcula los puntos de cada jugador y los muestra por pantalla
(deffunction obtenerPuntos ($?tablero)
    
    (bind ?puntosRojo 0)
    (bind ?puntosAzul 0)
    
    (progn$ (?ficha $?tablero) 
        (bind ?color (sub-string 1 1 ?ficha))
        (bind ?puntos (eval (sub-string 2 3 ?ficha)))

        (if (= (str-compare ?color "R") 0) then
            (bind ?puntosRojo (+ ?puntosRojo ?puntos))
        else
            (bind ?puntosAzul (+ ?puntosAzul ?puntos))
        )
    )
    ;(printout t "PUNTOS ROJO: " ?puntosRojo crlf)
    ;(printout t "PUNTOS AZUL: " ?puntosAzul crlf)
    (bind $?puntosTablero (create$ ?puntosRojo ?puntosAzul))
    (return $?puntosTablero)
)


;Dados el numero de ficha insertada y el tablero, calcula el valor del heurístico
(deffunction obtenerHeuristico (?numero $?tablero)
    (bind ?numero (eval ?numero))

    (bind $?puntos (create$ (obtenerPuntos $?tablero)))

    (bind ?puntosRojo (nth$ 1 $?puntos))
    (bind ?puntosAzul (nth$ 2 $?puntos))

    (bind ?heuristico (div (- ?puntosAzul ?puntosRojo)?numero))
    ;(printout t (div (- ?puntosAzul ?puntosRojo)?numero) crlf)
    ;(printout t "HEURISTICO: " ?heuristico crlf)
    (return ?heuristico)

)

;Dado un tablero y las fichas disponibles del jugador, genera todos los estados siguientes posibles (prof 1)
(deffunction generarHijos (?idPadre ?tableroPadre $?fichasLibres)
    
    (bind $?posLibres (obtenerLibres $?tableroPadre))
    ;Para cada posicion libre del tablero
    (progn$ (?pos $?posLibres)

        ;para cada ficha libre del jugador
        (progn$ (?ficha $?fichasLibres)
            
            ;generamos multicampo para le nuevo tablero (copia del padre)
            (bind $?tableroNuevo $?tableroPadre)
            
            ;Inicializa la ficha a insertar (su valor en formato string)
            (bind ?fichaString (str-cat ?ficha ""))

            ;Comprobamos que NO sea una ficha ya utilizada 
            (if (not (=(str-compare ?fichaString -)0)) then
                
                ;Obtenemos el valor de la ficha en formato int
                (bind ?fichaInt (nth$ ?ficha $?fichasLibres))
                (bind ?fichaString (str-cat ?fichaString ""))
                
                ;calculamos las coordenadas en las que meter la ficha
                (bind $?coordenadas (posMatriz2D ?pos))
                ;Damos formato a la ficha
                (bind ?fichaInt (eval ?fichaString))
                (if (< ?fichaInt 10) then (bind ?fichaString (str-cat "0" ?fichaString))
                else (bind ?fichaString (str-cat "" ?fichaString)))
                
                (if (= (mod  ?*turnos*  2) 0) then 
                    (bind ?fichaString (str-cat "R" ?fichaString))
                else
                    (bind ?fichaString (str-cat "A" ?fichaString))
                )

                ;insertamos la ficha en el tablero
                (bind $?tableroNuevo (replace$ $?tableroNuevo ?pos ?pos ?fichaString))
              
                ;obtener las fichas adyacentes a la ficha insertada
                (bind ?x (nth$ 1 ?coordenadas))
                (bind ?y (nth$ 2 ?coordenadas))
                (bind $?adyacentes (obtenerAdyacentes (- ?x 1) (- ?y 1))) 
                ;Hacer las actualizaciones en las adyacentes
                
                (bind $?tableroNuevo (actualizarAdyacentes (sub-string 2 3 ?fichaString) $?adyacentes $?tableroNuevo))
                
                (bind ?*id* (+ ?*id* 1))
                (bind ?prof ?idPadre)

                ;calcular heuristico
                (bind ?heur (obtenerHeuristico (sub-string 2 3 ?fichaString) $?tableroNuevo))

                (assert (tablero (matriz $?tableroNuevo) (id ?*id*) (padre ?idPadre) (prof ?prof) (alfa -999) (beta 999) (heuristico ?heur) (turno ?*iaju*) (real "no")))
                
            )   
        )
    )
    
    (bind ?*tableroOpt* $?tableroNuevo)
    (bind ?*heurOpt* ?heur)
)


;******************************FIN FUNCIONES******************************
;*************************************************************************


;INICIALIZA EL TABLERO (TODO VACIO)
(defrule INICIALIZAR_TABLERO
    ?b<-(estado "INICIO")
=>
    (retract ?b)
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
    
    (while (and(not (=(str-compare ?*iaju* "ia")0))(not(=(str-compare ?*iaju* "ju")0)))
        (printout t "¿Quien empieza? ia/ju" crlf)
        (bind ?*iaju* (read))
    )
    
    ;Inicializar hecho de tablero
    (assert (tablero (matriz $?tableroLocal) (id ?*id*) (padre 0) (prof 0) (alfa -999) (beta 999) (heuristico 0) (turno "ju") (real "si")))
    

    (if (=(str-compare ?*iaju* "ia")0) then
        (assert (estado ?*AGENTE*))
    else
        (assert (estado "JUGADOR"))
    )

    
)


;PIDE AL USUARIO COORDENADAS Y GENERA LA FICHA
(defrule INSERTAR_FICHA
    ?tab <-(tablero (matriz $?tableroLocal) (id 1) (padre ?) (prof ?) (alfa ?) (beta ?)(turno ?))
    ?b<-(estado "JUGADOR")
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
        (printout t "Turno " ?*turnos* " de partida" crlf crlf)
        (if (= (mod  ?*turnos*  2) 0) then
        
            (printout t "Turno del jugador 1 (ROJO)" crlf)
            (printout t "Fichas disponibles:" ?*jugador1* crlf)
            (printout t "Elija numero de ficha" crlf)
            (bind ?numero (read))

            ;lista=multicampo de fichas del jugador
            (bind $?lista (create$ (explode$ ?*jugador1*)))

            (if (not (subset (create$ ?numero) $?lista)) then
                (printout "Ese numero ya se ha usado, por favor elija otro" crlf)
                (assert (estado "JUGADOR"))
            )
        else
            (printout t "Turno del jugador 2 (AZUL)" crlf)
            (printout t "Fichas disponibles:" ?*jugador2* crlf)
            (printout t "Elija numero de ficha" crlf)
            (bind ?numero (read))

            ;lista=multicampo del fichas del jugador
            (bind $?lista (create$ (explode$ ?*jugador2*)))

            (if (not (subset (create$ ?numero) $?lista)) then
                (printout "Ese numero ya se ha usado, por favor elija otro" crlf)
                (assert (estado "JUGADOR"))
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
            (assert (estado "JUGADOR"))
        else    
            (if (> ?y ?*tamano*) then
                (printout t "Indica una posicion correcta" crlf)
                (assert (estado "JUGADOR"))
            else
                (bind ?posicion (+(+(* ?x ?*tamano*) ?y)1))
            
                (bind ?comprobar (subseq$ $?tableroLocal ?posicion ?posicion))
            
                (if (subset ?comprobar (create$ "_00")) then
                    (assert (estado "ACTUALIZAR"))
                    (assert (ficha ?color ?numero ?x ?y ))
                    (bind $?lista (replace$ $?lista ?numero ?numero -))
                
                    (if (= (mod  ?*turnos*  2) 0) then
                        ;(bind ?*score1* (+ ?*score1* ?numero))
                        (bind ?*jugador1* (implode$ $?lista))
                    else
                        ;(bind ?*score2* (+ ?*score2* ?numero))
                        (bind ?*jugador2* (implode$ $?lista))
                    )

                else
                    (printout t "Esa posicion ya esta en uso, por favor, indica una posicion correcta" crlf)
                    (assert (estado "JUGADOR"))
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
    ?tab <-(tablero (matriz $?tableroLocal) (id 1) (padre ?) (prof ?) (alfa ?) (beta ?)(turno ?turno)(real "si"))
    ?a<-(ficha ?color ?numero ?x ?y )
    ?b<-(estado "ACTUALIZAR")

=>
    (retract ?a)
    (retract ?b)
    
    ;Para que el tablero se imprima bien, añadimos un 0 si el valor es de un solo digito
    (if (< ?numero 10) then
        (bind ?numero (str-cat "0" ?numero))
    )
    ;damos formato a la ficha
    (bind ?ficha (str-cat ?color ?numero))

    ;calculamos su posicion en el tablero
    (bind ?posicion (posTablero ?x ?y ))
    
    (printout t "posicion: " ?posicion crlf)
    ;insertamos la ficha en el tablero
    (bind $?tableroLocal (replace$ $?tableroLocal ?posicion ?posicion ?ficha))
    ;obtener las fichas adyacentes a la ficha insertada
    (bind $?adyacentes (obtenerAdyacentes ?x ?y))
    
    ;Hacer las actualizaciones en las adyacentes
    (bind $?tableroLocal (actualizarAdyacentes (sub-string 2 3 ?ficha) $?adyacentes $?tableroLocal))
    ;Mostramos el tablero
    (mostrarTablero $?tableroLocal)
    
    ;Comprobar si la partida ha terminado o no y el cambio de estado ju/ia
    (if (=(mod ?*tamano* 2)0) then
        (if (= ?*turnos* (* ?*tamano* ?*tamano*)) then
            (assert (estado "GANAR"))
        else
            (if (=(str-compare ?*iaju* "ju")0) then
                (assert (estado ?*AGENTE*))
                (bind ?*iaju* "ia")
            else
                (assert (estado "JUGADOR"))
                (bind ?*iaju* "ju")
            )
        )
    else
        (if (= ?*turnos* (-(* ?*tamano* ?*tamano*)1)) then
            (assert (estado "GANAR"))
        else
           (if (=(str-compare ?*iaju* "ju")0) then
                (assert (estado ?*AGENTE*))
                (bind ?*iaju* "ia")
            else
                (assert (estado "JUGADOR")) 
                (bind ?*iaju* "ju")
            )
        )
    )

    ;Avanzamos un turno
    (bind ?*turnos* (+ ?*turnos* 1))

    (bind ?heur (obtenerHeuristico (sub-string 2 3 ?ficha) $?tableroLocal))
    
    ;Actualizamos el hecho del tablero
    (modify ?tab (matriz $?tableroLocal) (turno ?*iaju*) (heuristico ?heur))

    (bind ?*tableroOpt* $?tableroLocal)

    (bind ?*score1* (nth$ 1 (obtenerPuntos $?tableroLocal)))
    (bind ?*score2* (nth$ 2 (obtenerPuntos $?tableroLocal)))

    ;Mostramos las puntuaciones hasta el momento
    (printout t crlf)
    (printout t "Puntuacion del jugador1 (R): " ?*score1* crlf)
    (printout t "Puntuacion del jugador2 (A): " ?*score2* crlf)
    (printout t crlf)
    (printout t crlf)

    
    
 
)

(defrule FINAL
    ?a<-(estado "GANAR")
=>
    (retract ?a)

    (if (= ?*score1* ?*score2*) then
        (printout t "Empate")
        (halt)
    else
        (if (> ?*score1* ?*score2*) then
            (printout t "El jugador1 (R) gana." crlf)
            (halt)
        else
            (printout t "El jugador2 (A) gana." crlf)
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
    (printout t "Puntuacion del jugador1 (R): " ?*score1* crlf)
    (printout t "Puntuacion del jugador2 (A): " ?*score2* crlf)
    (halt)

)

(defrule RANDOM
    ?tab <-(tablero (matriz $?tableroLocal) (id ?id) (padre 0) (prof ?prof) (alfa ?alfa) (beta ?beta) (heuristico ?heur) (turno ?) (real "si"))
    ?a <-(estado "RANDOM")
=>
    (retract ?a)
    
    ;Valor random
    (if (= (mod  ?*turnos*  2) 0) then
        (bind $?fichasLibres (explode$ ?*jugador1*))
        (bind ?color "R")
    else
        (bind $?fichasLibres (explode$ ?*jugador2*))   
        (bind ?color "A")     
    )
    (bind ?valorRandom -)
    (while (=(str-compare ?valorRandom -) 0)
        (bind ?valorRandom (random 1 (div (* ?*tamano* ?*tamano*) 2)))
        (bind ?valorRandom (nth$ ?valorRandom $?fichasLibres))
        (bind ?valorRandom (str-cat ?valorRandom ""))
    )
    ;Posición random
    (bind ?posFichaRandom "inicio")
    (while (not(=(str-compare (sub-string 1 1 ?posFichaRandom)"_")0))
        (bind ?posRandom (random 1 (* ?*tamano* ?*tamano*)))
        (bind ?posFichaRandom (nth$ ?posRandom $?tableroLocal))
        (bind ?posFichaRandom (str-cat  ?posFichaRandom ""))
    )
    ;creamos la ficha
    (bind $?coord (create$ (posMatriz2D ?posRandom)))
    (bind ?x (- (nth$ 1 $?coord)1))
    (bind ?y (-(nth$ 2 $?coord)1))

    (bind ?valorRandom (eval ?valorRandom))
    (assert (ficha ?color ?valorRandom ?x ?y ))
    
    (bind $?fichasLibres (replace$ $?fichasLibres ?valorRandom ?valorRandom -))
    (if (= (mod  ?*turnos*  2) 0) then
        (bind ?*jugador1* (implode$ ?fichasLibres))
        ;(bind ?*score1* (+ ?*score1* ?valorRandom))
    else
        (bind ?*jugador2* (implode$ ?fichasLibres)) 
        ;(bind ?*score2* (+ ?*score2* ?valorRandom))
    )


    (assert (estado "ACTUALIZAR"))

)

(defrule MINIMAX
    ?tab <-(tablero (matriz $?tableroLocal) (id ?id) (padre ?padre) (prof ?prof) (alfa ?alfa) (beta ?beta) (heuristico ?heur) (turno ?) (real "si"))
    ?a <-(estado "MINIMAX")
=>
 
    (retract ?a)
    
    (if (= (mod  ?*turnos*  2) 0) then
        (bind $?fichasLibres (explode$ ?*jugador1*))
    else
        (bind $?fichasLibres (explode$ ?*jugador2*))        
    )

    (bind ?*heurOpt* ?heur)
    
    (generarHijos ?id $?tableroLocal $?fichasLibres)
    
    (assert (estado "SELECCION_JUGADA"))
)

(defrule SELECCION_JUGADA
    (declare (salience 10))
    ?tab <-(tablero (matriz $?tableroLocal) (id ?id) (padre ?padre) (prof ?prof) (alfa ?alfa) (beta ?beta) (heuristico ?heur) (turno ?) (real "no"))
    ?a <-(estado "SELECCION_JUGADA")
=>
    (retract ?tab)
    (if (<= ?*heurOpt* ?heur) then       ;si el optimo es igual o peor, nos quedamos con esa jugada
        (bind ?*heurOpt* ?heur)

        (bind ?*tableroOpt* $?tableroLocal)
        (bind ?*idOpt* ?id)
        (bind ?*padreOpt* ?padre)
        (bind ?*profOpt* ?prof)
        (bind ?*alfaOpt* ?alfa)
        (bind ?*betaOpt* ?beta)
    )
)

(defrule GENERAR_FICHA_MINIMAX
    (declare (salience 5))
    ?tab <-(tablero (matriz $?tableroLocal) (id ?id) (padre ?padre) (prof ?prof) (alfa ?alfa) (beta ?beta) (heuristico ?heur) (turno ?) (real "si"))
    ?a <-(estado "SELECCION_JUGADA")

=>
    (retract ?a)
    
    (bind ?i 1)
    
    ;la ficha global esta actualizada, la local no. Cuando sean distintas significara que es la que se ha cambiado
    (progn$ (?fichaGlobal ?*tableroOpt*)
        (bind ?fichaLocal (nth$ ?i $?tableroLocal))
        (if (not(=(str-compare ?fichaGlobal ?fichaLocal) 0)) then
            ;Obtenemos los datos de la ficha para insertarla
            (printout t "color : " (sub-string 1 1 ?fichaGlobal) crlf)
            (bind ?color (sub-string 1 1 ?fichaGlobal))
            (bind ?valor (eval(sub-string 2 3 ?fichaGlobal)))
            (bind ?x (-(nth$ 1 (posMatriz2D ?i))1))
            (bind ?y (-(nth$ 2 (posMatriz2D ?i))1))
        )
        (bind ?i (+ ?i 1))
    )
    
    (assert (ficha ?color ?valor ?x ?y ))

    ;Marcar la ficha como usada
    (if (= (mod  ?*turnos*  2) 0) then
        (bind $?fichasLibres (explode$ ?*jugador1*))
        (bind $?fichasLibres (replace$ $?fichasLibres ?valor ?valor -))
        (bind ?*jugador1* (implode$ ?fichasLibres))
        
    else
        (bind $?fichasLibres (explode$ ?*jugador2*))
        (bind $?fichasLibres (replace$ $?fichasLibres ?valor ?valor -))
        (bind ?*jugador2* (implode$ ?fichasLibres)) 
      
    )

    (assert (estado "ACTUALIZAR"))
)


