(deftemplate tablero
    (multislot matriz)
    (slot id)
    (slot padre)
    (slot prof)
    (slot heuristico)
    (slot turno)
    (slot real)
)

(deftemplate ficha
    (slot color)
    (slot valor)
    (multislot coordenadas)
)

;Turnos pares:      Rojo jugador1
;Turnos impares:    Azul jugador2
(defglobal 
    ?*turnos* = 1                       ;Turno actual de juego
    ?*tamano* = 0                       ;Tamaño del tablero
    ?*id* = 1                           ;Id de cada jugada
    ?*jugador1* = ""                    ;Fichas disponibles del jugador 1 (ROJO)
    ?*jugador2* = ""                    ;Fichas disponibles del jugador 2 (AZUL)
    ?*score1* = 0                       ;Puntuación del jugador 1 (ROJO)
    ?*score2* = 0                       ;Puntuación del jugador 2 (AZUL)
    ?*iaju* = ""                        ;Turno ia / ju (jugador)
    ?*inicio* = ""                      ;Variable que guarda quien empieza (ia o jugador)
    ?*AGENTE* = "MINIMAX"               ;MINIMAX // RANDOM (para pruebas)
    
    ;Información que guarda la jugada óptima hasta el momento
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
    (printout t crlf)
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


;Dada la posicion (1D), la lista de adyacentes y el tablero, actualiza el valor de las fichas adyacentes en el tablero
(deffunction actualizarAdyacentes (?numero ?adyacentes $?tableroLocal)
    (progn$ (?pos $?adyacentes)
        (bind ?ficha (nth$ ?pos $?tableroLocal))
        (bind ?color (sub-string 1 1 ?ficha))
        
        (bind ?pts (eval (sub-string 2 4 ?ficha))) ;Crea un string con los pts y los pasa a integer
        
        (if (or (and (eq ?color "R") (= (mod ?*turnos* 2) 0))           ; Ficha roja en turno rojo: sumar
                (and (eq ?color "A") (= (mod ?*turnos* 2) 1))) then     ; Ficha azul en turno azul: sumar
            (bind ?pts (+ ?pts 1))
        )
        (if (or (and (eq ?color "R") (= (mod ?*turnos* 2) 1))           ; Ficha roja en turno azul: cambiar color
                (and (eq ?color "A") (= (mod ?*turnos* 2) 0))) then     ; Ficha azul en turno rojo: cambiar color
                
            (bind ?ptsFichaActual (eval ?numero))

            (if (> ?ptsFichaActual ?pts) then 
                (if (eq ?color "R") then    (bind ?color "A") 
                else                        (bind ?color "R"))
            )
             
        )
        ;Para que el tablero se imprima bien, añadimos un 0 si el valor es de un solo digito
        (if (< ?pts 10) then (bind ?ptsString (str-cat "0" ?pts))
        else (bind ?ptsString (str-cat "" ?pts)))

        (bind ?ficha (str-cat ?color ?ptsString))
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


;Dado un tablero, calcula los puntos de cada jugador
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
    (bind $?puntosTablero (create$ ?puntosRojo ?puntosAzul))
    (return $?puntosTablero)
)


;Dados el numero de ficha insertada y el tablero, calcula el valor del heurístico
(deffunction obtenerHeuristico (?numero $?tablero)
    (bind ?numero (eval ?numero))
    
    (bind $?puntos (create$ (obtenerPuntos $?tablero)))

    (bind ?puntosRojo (nth$ 1 $?puntos))
    (bind ?puntosAzul (nth$ 2 $?puntos))
 
    ; heur = (ptsR-ptsA)/(ptsFicha/2)
    (bind ?heuristico (- ?puntosRojo ?puntosAzul))
    (bind ?numero (div ?numero 2))
    (if (= ?numero 0) then (bind ?numero 1))
    (bind ?heuristico (div ?heuristico ?numero))
  
    (return ?heuristico)

)

;Dado un tablero y las fichas disponibles del jugador, genera todos los estados siguientes posibles (prof 1)
(deffunction generarHijos (?idPadre ?tableroPadre $?fichasLibres)
    
    (bind $?posLibres (obtenerLibres $?tableroPadre))
    ;Para cada posicion libre del tablero
    (progn$ (?pos $?posLibres)

        ;para cada ficha libre del jugador
        (progn$ (?ficha $?fichasLibres)
            
            ;generamos multicampo para el nuevo tablero (copia del padre)
            (bind $?tableroNuevo $?tableroPadre)
            
            ;Inicializa la ficha a insertar (su valor en formato string)
            (bind ?fichaString (str-cat ?ficha ""))

            ;Comprobamos que NO sea una ficha ya utilizada 
            (if (not (=(str-compare ?fichaString -)0)) then
                
                ;Obtenemos el valor de la ficha en formato int
                (bind ?fichaInt (nth$ ?ficha $?fichasLibres))

                ;(bind ?fichaString (str-cat ?fichaString ""))
                
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

                ;calcular heuristico
                (bind ?heur (obtenerHeuristico (sub-string 2 3 ?fichaString) $?tableroNuevo))

                (assert (tablero (matriz $?tableroNuevo) (id ?*id*) (padre ?idPadre) (prof 1) (heuristico ?heur) (turno ?*iaju*) (real "no")))
                
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
    
    ;Elegir quien empieza
    (while (and(not (=(str-compare ?*iaju* "ia")0))(not(=(str-compare ?*iaju* "ju")0)))
        (printout t "¿Quien empieza? ia/ju" crlf)
        (bind ?*iaju* (read))
    )
    (bind ?*inicio* ?*iaju*)
    
    ;Inicializar hecho de tablero
    (assert (tablero (matriz $?tableroLocal) (id ?*id*) (padre 0) (prof 0) (heuristico 0) (turno "ju") (real "si")))
    
    ;Dar paso al jugador o a la ia
    (if (=(str-compare ?*iaju* "ia")0) then
        (assert (estado ?*AGENTE*))
    else
        (assert (estado "JUGADOR"))
    )  
)


;PIDE AL USUARIO COORDENADAS Y GENERA LA FICHA
(defrule INSERTAR_FICHA
    ?tab <-(tablero (matriz $?tableroLocal))
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
        
        ;Eleccion de ficha
        (if (= (mod  ?*turnos*  2) 0) then
        
            ;lista=multicampo de fichas del jugador
            (bind $?lista (create$ (explode$ ?*jugador1*)))

            (printout t "Turno del jugador 1 (ROJO)" crlf)
            (printout t "Fichas disponibles:" ?*jugador1* crlf)
            (printout t "Elija numero de ficha" crlf)
            (bind ?numero (read))

            (while (not (subset (create$ ?numero) $?lista))
                (printout t "Ese numero ya se ha usado, por favor elija otro" crlf)
                (printout t "Fichas disponibles:" ?*jugador1* crlf)
                (bind ?numero (read))
            )
        else

            ;lista=multicampo del fichas del jugador
            (bind $?lista (create$ (explode$ ?*jugador2*)))

            (printout t "Turno del jugador 2 (AZUL)" crlf)
            (printout t "Fichas disponibles:" ?*jugador2* crlf)
            (printout t "Elija numero de ficha" crlf)
            (bind ?numero (read))

            (while (not (subset (create$ ?numero) $?lista))
                (printout t "Ese numero ya se ha usado, por favor elija otro" crlf)
                (printout t "Fichas disponibles:" ?*jugador2* crlf)
                (bind ?numero (read))
            )
        )
    
        ;Eleccion de posicion
        (printout t "Donde desea insertar la ficha?" crlf)
        
        (printout t "Indica la fila" crlf)
        (bind ?x (read))
        (while (or (> ?x ?*tamano*) (< ?x 1))
            (printout t "Seleccione una fila correcta" crlf)
            (bind ?x (read))
        )

        (printout t "Indica la columna" crlf)
        (bind ?y (read))
        (while (or (> ?y ?*tamano*) (< ?y 1))
            (printout t "Seleccione una columna correcta" crlf)
            (bind ?y (read))
        )

        ;Para nosotros los indices empiezan en 0 (facilita calculos)
        ;Para el usuario empiezan en 1, por eso restamos 1
        (bind ?x (- ?x 1)) (bind ?y (- ?y 1))
        (if (= (mod  ?*turnos*  2) 0) then  (bind ?color "R")
        else                                (bind ?color "A"))

        (bind ?posicion (posTablero ?x ?y))
            
        (bind ?comprobar (subseq$ $?tableroLocal ?posicion ?posicion))

        (while (not(subset ?comprobar (create$ "_00")))
            (printout t "Esa posicion ya esta ocupada, por favor seleccione una correcta" crlf)

            ;Eleccion de posicion
            (printout t "Donde desea insertar la ficha?" crlf)
            (printout t "Indica la fila" crlf)
            (bind ?x (read))

            (while (or (> ?x ?*tamano*) (< ?x 1))
                (printout t "Seleccione una fila correcta" crlf)
                (bind ?x (read))
            )

            (printout t "Indica la columna" crlf)
            (bind ?y (read))
            (while (or (> ?y ?*tamano*) (< ?y 1))
                (printout t "Seleccione una fila correcta" crlf)
                (bind ?x (read))
            )
            ;Para nosotros los indices empiezan en 0 (facilita calculos)
            ;Para el usuario empiezan en 1, por eso restamos 1
            (bind ?x (- ?x 1)) (bind ?y (- ?y 1))
            (if (= (mod  ?*turnos*  2) 0) then  (bind ?color "R")
            else                                (bind ?color "A"))

            ;(bind ?posicion (+(+(* ?x ?*tamano*) ?y)1))
            (bind ?posicion (posTablero ?x ?y))
            
            (bind ?comprobar (subseq$ $?tableroLocal ?posicion ?posicion))
        )

        (bind $?lista (replace$ $?lista ?numero ?numero -))

        (if (= (mod  ?*turnos*  2) 0) then  (bind ?*jugador1* (implode$ $?lista))
        else                                (bind ?*jugador2* (implode$ $?lista)))

        (assert (estado "ACTUALIZAR"))
        (assert (ficha (color ?color) (valor ?numero) (coordenadas (create$ ?x ?y))))

    ;salir
    else
        (if (=(str-compare ?salir "s")0) then
            (assert (estado "SALIR"))
        )   
    )
)


;METE LA FICHA GENERADA EN EL TABLERO
(defrule ACTUALIZAR_TABLERO
    ?tab <-(tablero (matriz $?tableroLocal) (real "si"))
    ?a<-(ficha (color ?color) (valor ?numero) (coordenadas $?coord))
    ?b<-(estado "ACTUALIZAR")

=>
    (retract ?a)
    (retract ?b)
    
    (bind ?x (nth$ 1 $?coord))
    (bind ?y (nth$ 2 $?coord))

    ;Para que el tablero se imprima bien, añadimos un 0 si el valor es de un solo digito
    (if (< ?numero 10) then (bind ?numero (str-cat "0" ?numero)))
    
    ;damos formato a la ficha
    (bind ?ficha (str-cat ?color ?numero))

    ;calculamos su posicion en el tablero
    (bind ?posicion (posTablero ?x ?y))
    
    (printout t "Ficha insertada en (fila,columna):  (" (+ ?x 1) "," (+ ?y 1 ) ")" crlf)
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
    ;Calcular heuristico
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
    (printout t crlf crlf)
)


;FIN DEL JUEGO
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

;PERMITE SALIR DEL JUEGO ANTES DE QUE ACABE
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

;IA RANDOM, ELIGE UNA FICHA DE LAS DISPONIBLES AL AZAR Y LA INSERTA EN UNA DE LAS POSICIONES LIBRES AL AZAR
(defrule RANDOM
    ?tab <-(tablero (matriz $?tableroLocal) (real "si"))
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
    (assert (ficha (color ?color) (valor ?valorRandom) (coordenadas (create$ ?x ?y))))
    
    (bind $?fichasLibres (replace$ $?fichasLibres ?valorRandom ?valorRandom -))
    (if (= (mod  ?*turnos*  2) 0) then
        (bind ?*jugador1* (implode$ ?fichasLibres))
    else
        (bind ?*jugador2* (implode$ ?fichasLibres)) 
    )

    (assert (estado "ACTUALIZAR"))

)

;PRIMERA REGLA PARA EL PROCESO DE MINIMAX, SE GENERAN TODOS LOS ESTADOS POSIBLES A PROFUNDIDAD 1
(defrule MINIMAX
    ?tab <-(tablero (matriz $?tableroLocal) (id ?id) (heuristico ?heur) (real "si"))
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


;SEGUNDA REGLA PARA EL PROCESO MINIMAX, SELECCIONA LA JUGADA CON EL HEURISTICO MAS FAVORABLE PARA LA IA (MIN O MAX)
(defrule SELECCION_JUGADA
    (declare (salience 10))
    ?tab <-(tablero (matriz $?tableroLocal) (id ?id) (padre ?padre) (prof ?prof) (heuristico ?heur) (real "no"))
    ?a <-(estado "SELECCION_JUGADA")
=>
    (retract ?tab)
                                                        ;Rojo-Azul
    (if (=(str-compare ?*inicio* "ia")0) then           ;Si empieza ia => ia azul (minimiza)
        (if (>= ?*heurOpt* ?heur) then
            (bind ?*heurOpt* ?heur)
            (bind ?*tableroOpt* $?tableroLocal)
            (bind ?*idOpt* ?id)
            (bind ?*padreOpt* ?padre)
            (bind ?*profOpt* ?prof)
        ) 
    else                                               ;Si empieza ju => ia rojo (maximiza)
        (if (<= ?*heurOpt* ?heur) then
            (bind ?*heurOpt* ?heur)
            (bind ?*tableroOpt* $?tableroLocal)
            (bind ?*idOpt* ?id)
            (bind ?*padreOpt* ?padre)
            (bind ?*profOpt* ?prof)
        )   
    )
)


;TERCERA REGLA PARA EL PROCESO MINIMAX, SELECCIONADA LA JUGADA, GENERA LA FICHA PARA INSERTARLA EN EL TABLERO
(defrule GENERAR_FICHA_MINIMAX
    (declare (salience 5))
    ?tab <-(tablero (matriz $?tableroLocal) (real "si"))
    ?a <-(estado "SELECCION_JUGADA")

=>
    (retract ?a)
    
    (bind ?i 1)
    
    ;la ficha global esta actualizada, la local no. Cuando sean distintas significara que es la que se ha cambiado
    (progn$ (?fichaGlobal ?*tableroOpt*)
        (bind ?fichaLocal (nth$ ?i $?tableroLocal))
        (if (not(=(str-compare ?fichaGlobal ?fichaLocal) 0)) then
            (if (=(str-compare ?fichaLocal "_00")0) then
                ;Obtenemos los datos de la ficha para insertarla
                (bind ?color (sub-string 1 1 ?fichaGlobal))
                (bind ?valor (eval(sub-string 2 3 ?fichaGlobal)))
                (bind ?x (-(nth$ 1 (posMatriz2D ?i))1))
                (bind ?y (-(nth$ 2 (posMatriz2D ?i))1))
            )
        )
        (bind ?i (+ ?i 1))
    )
    
    (assert (ficha (color ?color) (valor ?valor) (coordenadas (create$ ?x ?y))))

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


