#lang racket

;zones = (history index localRepo remoteRepo)
;history: log de las funciones aplicadas de la forma '( '(tiempo1 funcion1) ... '(tiempoN funcionN) )
;en orden cronologico
(define zones
  (list (list (list 43242424 "hello")  (list 4121234 "boomer")) null null null))

(define get-history
  ( lambda (zones-list)
     (car zones-list)))

(define get-index
  ( lambda (zones-list)
     (cadr zones-list)))

(define get-localRepo
  ( lambda (zones-list)
     (caddr zones-list)))

(define get-remoteRepo
  ( lambda (zones-list)
     (cadddr zones-list)))


;crea la unidad de historial. recibe el nombre de la funcion como string. se arreglara ingreso de
;datos despues.
(define create-log
  ( lambda (name-of-function-string)
     (list (current-seconds) name-of-function-string)))


;actualiza el historial con una funcion dada. recibe una lista de log y el historial a sumar y retorna
;el historial con el log añadido.
(define update-history
  ( lambda (addition-log-list zonas)
     (append (car zonas) (list addition-log-list))))


;añade el historial actualizado al TDA zonas de la funcion realizada. recibe el historial actualizado
;(lista), el tda zonas y retorna el tda zonas con el historial actualizado.
(define add-history
  ( lambda (updated-history zonas)
     (append (list updated-history) (cdr zonas))))


(define git (lambda (function)
              (lambda (zonas)
                (function zonas))))

(define pull (lambda (x) (+ x 2)))