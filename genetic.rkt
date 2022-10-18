#lang racket

;; Estructura de los genes:
(struct individual '(Velocidad, Fuerza, Posicion, Habilidad)) ;; En la primera generacion todos estos atributos seran aleatorios.

;; Definicion de la primera generacion


;;;; Operaciones

;; Primera generacion

;; Funcion de aptitud (asignacion del "Fitness Score")

;; Funcion de seleccion (eleccion de los mejores individuos)

;; Funcion de reproduccion (creacion de hijos con base en los mejores individuos seleccionados)

;; Funcion de mutacion (insertar genes aleatorios)



(define a (random 0 2))
a
