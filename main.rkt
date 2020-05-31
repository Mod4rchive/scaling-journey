#lang racket

;zones = (history index localRepo remoteRepo)
;commit = (mensaje (archivo1 ... archivoN))
;history: log de las funciones aplicadas de la forma '( '(tiempo1 funcion1) ... '(tiempoN funcionN) )
;en orden cronologico
(define zonas1
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


;crea la unidad de historial. recibe el nombre de la funcion como string. se arreglara ingreso de
;datos despues.
(define create-log
  ( lambda (name-of-function-string)
     (list (current-seconds) name-of-function-string)))


;actualiza el historial con una funcion dada. recibe una lista de log y el historial a sumar y retorna
;el historial con el log añadido.
(define update-history
  ( lambda (addition-log-list zones)
     (append (car zones) (list addition-log-list))))


;añade el historial actualizado al TDA zonas de la funcion realizada. recibe el historial actualizado
;(lista), el tda zonas y retorna el tda zonas con el historial actualizado.
(define add-history
  ( lambda (updated-history zones)
     (append (list updated-history) (cdr zones))))


(define git (lambda (function)
              (lambda (zones)
                (function zones))))

(define pull
  (lambda (zones)
               (add-history (update-history (create-log "pull") zones) zones)))

(define push
  (lambda (zones)
               (add-history (update-history (create-log "push") zones) zones)))

(define commit
  (lambda (message)
    (lambda (zones)
      (add-history (update-history (create-log (string-append "commit -m " message)) zones) zones))))

(define file-list-to-string
  (lambda (file-list)
    (apply string-append (map (lambda (file-string) (string-append " " file-string)) file-list))))

(define add
  (lambda (file-list)
    (lambda (zones)
      (add-history
       (update-history
        (create-log
         (string-append "add" (file-list-to-string file-list)))
        zones)
       zones))))