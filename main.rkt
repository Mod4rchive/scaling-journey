#lang racket

;zones = (history index localRepo remoteRepo)
;history: log de las funciones aplicadas de la forma '( '(tiempo1 funcion1) ... '(tiempoN funcionN) )
;en orden cronologico
(define zones
  (list null null null null))

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

;actualiza el historial con una funcion dada. recibe una lista de log y el historial a sumar y retorna
;el historial con el log añadido.
(define update-history
  ( lambda (function-log-list history-list)
     (append (history-list) '(function-log-list))))

;añade el historial actualizado al TDA zonas de la funcion realizada. recibe el historial actualizado
;(lista), el tda zonas y retorna el tda zonas con el historial actualizado.
(define add-history
  ( lambda (addition-list zonas)
     (append '(addition-list) (cdr zonas))))

(define git (lambda (function) (list (current-seconds) function)))