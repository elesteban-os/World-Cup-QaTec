#lang racket

;;------------------Función Aptitud-----------------
(define (aptitud equipo1 equipo2)
  (aptitud-aux equipo1 equipo2 '() '()))

(define (aptitud-aux j1 j2 bestgen1 bestgen2)
  (set! bestgen1 (best j1 1))
  (set! bestgen2 (best j2 1))
  (set! bestgen1 (append (list bestgen1) (list (best j1 2)) (list (best j1 3))))
  (set! bestgen2 (append (list bestgen2) (list (best j2 2)) (list (best j2 3))))
  (display "1er equipo de mejores \n")
  (display bestgen1)
  (display "\n\n2do equipo de mejores\n")
  (display bestgen2)
  )
  

(define (best equipo parametro)
  (cond
    ((null? equipo)'());;((1 2 3 4) (4 5 6 7) (7 8 9 10))
    (else
    (listamejores equipo (compara (recortaLista parametro equipo) 0 0 0))
    )))
    
(define (listamejores lista resultado)
  (cond
  ((equal? resultado 0) (car lista))
    (else (listamejores (cdr lista) (- resultado 1))))
)

;;(listamejores '((10 60 10 4) (30 90 10 7) (7 180 10 10)) 2)

(define (recortaLista pos matriz);;((1 2 3 4) (4 5 6 7) (7 8 9 10))
  (cond
    ((null? matriz) '())
    (else 
      (cond
      ((equal? pos 1)(cons (caar matriz) (recortaLista pos (cdr matriz))))
      ((equal? pos 2)(cons (cadar matriz) (recortaLista pos (cdr matriz))))
      ((equal? pos 3)(cons (caddar matriz) (recortaLista pos (cdr matriz))))))))

(define (compara lista pos maxpos max)
  (cond ((null? lista)maxpos)
    ((> (car lista) max) (compara (cdr lista) (+ pos 1) pos (car lista)))
    (else (compara (cdr lista) (+ pos 1) maxpos max))))



;(aptitud '((10 60 10 4) (30 90 10 7) (7 180 10 10)) '((50 60 10 4) (30 90 10 7) (7 180 10 10)))
;;------------------Función Aptitud-----------------




; Funcion primera generacion
(define (primera_generacion nJugadores)
    (primera_generacion_aux '() '() 4 nJugadores))

(define (primera_generacion_aux equipo jugador atributo nJugadores)
    (cond ((eq? nJugadores 0) equipo)
          ((eq? atributo 0) (primera_generacion_aux (cons jugador equipo) '() 4 (- nJugadores 1)))
          (else (primera_generacion_aux equipo (cons (random 0 11) jugador) (- atributo 1) nJugadores))))


; Funcion reproducion


; Funcion mutacion
(define (mutacion equipo))

(define (mutacion_aux equipo jugador)
    (cond ((and (eq? jugador 0) (null? (cdr equipo))) (list (cambiarGen (car equipo) (random 0 4))))
          ((eq? jugador 0) (cons (cambiarGen (car equipo) (random 0 3)) (cdr equipo)))
          (else (cons (car equipo) (mutacion_aux (cdr equipo) (- jugador 1))))))

(define (cambiarGen jugador gen)
    (cond ((and (eq? gen 0) (null? (cdr jugador))) (list (random 0 11)))
          ((eq? gen 0) (cons (random 0 11) (cdr jugador)))
          (else (cons (car jugador) (cambiarGen (cdr jugador) (- gen 1))))))

; Funcion algoritmo genetico
;(define (algoritmo_genetico)

;    (algoritmo_genetico_aux '() '()))

;(define (algoritmo_genetico_aux equipo1 equipo2))
