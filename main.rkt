#lang racket

;zones = ((history index localRepo remoteRepo branch1) ... (... branchN))
;commit = (archivo1 ... archivoN)
;localRepo = remoteRepo = (commit1 ... commitN)
;history: log de las funciones aplicadas de la forma '( '(tiempoN funcionN) ... '(tiempo1 funcion1) )
;en orden cronologico
(define zonas1
  (list (list (list "HISTORY") (list "INDEX") (list "LOCAL REPO") (list "REMOTE REPO") "master")))

(define get-history
  ( lambda (zones-list)
     (car (car zones-list))))

(define get-index
  ( lambda (zones-list)
     (cadr (car zones-list))))

(define get-localRepo
  ( lambda (zones-list)
     (caddr (car zones-list))))

(define get-remoteRepo
  ( lambda (zones-list)
     (cadddr (car zones-list))))

(define get-currentBranch
  ( lambda (zones-list)
     (car (cddddr (car zones-list)))))


;crea la unidad de historial. recibe el nombre de la funcion como string. se arreglara ingreso de
;datos despues.
(define create-log
  ( lambda (name-of-function-string)
     (list (number->string (current-seconds)) name-of-function-string)))


;actualiza el historial con una funcion dada. recibe una lista de log y el historial a sumar y retorna
;el historial con el log añadido.
(define update-history
  ( lambda (addition-log-list zones)
     (append (list (car (car zones))) (list addition-log-list))))


;añade el historial actualizado al TDA zonas de la funcion realizada. recibe el historial actualizado
;(lista), el tda zonas y retorna el tda zonas con el historial actualizado.
(define set-history
  ( lambda (updated-history zones)
     (list (append (list updated-history) (cdr (car zones))) (cdr zones))))


(define git (lambda (function)
              (lambda (zones)
                (function zones))))

(define pull
  (lambda (zones)
               (set-history (update-history (create-log "pull") zones) zones)))

(define push
  (lambda (zones)
               (set-history (update-history (create-log "push") zones) zones)))

(define commit
  (lambda (message)
    (lambda (zones)
      (set-history (update-history (create-log (string-append "commit -m " message)) zones) zones))))

(define file-list-to-string
  (lambda (file-list)
    (if null?
        ""
        (apply string-append (map (lambda (file-string) (string-append " " file-string)) file-list)))))

(define add
  (lambda (file-list)
    (lambda (zones)
      (set-history
       (update-history
        (create-log
         (string-append "add" (file-list-to-string file-list)))
        zones)
       zones))))


;(define zonas->string
 ; (lambda (zones)
  ;  ()))